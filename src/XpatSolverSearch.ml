open XpatSolverValidate

let max_score = 52;;
let ecart = 20;;
let currennt_best_score = ref 0;;

let score state = 
	let rec score_aux state index = 
		match FArray.get (state.deposit) (index) with
			| exception Not_found -> 0
			| c -> c + score_aux state (index+1)
	in score_aux state 0;;

let compare_state state1 state2 = 
	if ((Stdlib.compare state1.registers state2.registers) = 0
			&& (Stdlib.compare state1.columns state2.columns) = 0) then 0
	else 
	 	if ((Stdlib.compare state1.registers state2.registers) = 0) then
			Stdlib.compare state1.columns state2.columns
		else 
			Stdlib.compare state1.registers state2.registers;;


module States = 
	Set.Make (struct type t = state let compare = compare_state end);;

(* structures pour la gestions des score  du type States FArray.t*)
let add e r =
	let s = score e in
		let c = FArray.get r s 
			in if (States.mem e c) then r
					else FArray.set r s (States.add e c);;

let init s = 
	FArray.init 53 (fun i -> if (i = score s) then States.singleton s else States.empty);;

let max_state s = 
		let result = States.max_elt_opt s in
			 result;;

let max r = 
	let rec max_aux r i = 
		if (i < 0) then None
		else	
			let s = FArray.get r i in
				if (States.is_empty s) then max_aux r (i-1)
				else max_state s
	in max_aux r 52;;

let supp s r = 
	let sc = score s in
		let c = FArray.get r sc in
			FArray.set r sc (States.remove s c)
(*--------------------------------------------------*)
let is_sa_mo game = 
	match game with Freecell | Baker -> false 	
											|	Seahaven | Midnight -> true

let list_of_id state = 
	let rec list_of_id_col col i =
		match FArray.get col i with
			| exception Not_found -> []
			| l -> if List.length l > 0 then 
					Card.to_num (List.hd l) 
					:: list_of_id_col col (i+1)
				else list_of_id_col col (i+1)

	in let rec list_of_id_reg reg i = 
		match FArray.get reg i with
			| exception Not_found|None-> []
			| Some c-> let id = Card.to_num c in id::list_of_id_reg reg (i+1)

	in (list_of_id_col state.columns 0) @ (list_of_id_reg state.registers 0);;


let sort_reg state =
	let liste_reg = FArray.to_list state.registers in
	let sorted_liste_reg = 
		List.sort (fun a' b' -> 
			match (a',b') with 
				| (Some a, Some b) -> (Card.to_num a) - (Card.to_num b)
				| (None , None) -> 0
				| (None, Some _) -> -1
				| (Some _, None) -> 1
		) liste_reg in
	state.registers <- FArray.of_list sorted_liste_reg;;

	let rec print_reg reg i = 
		match FArray.get reg i with
			| exception Not_found -> ()
			| r -> match r with 
				| None -> Printf.printf "None,";print_reg reg (i+1)
				| Some r' -> (Printf.printf "%d," (Card.to_num r')); print_reg reg (i+1);;
	
	let rec print_col c i = 
		match FArray.get c i with
			| exception Not_found -> ()
			| r -> Printf.printf "col %d:" i;List.iter (fun x -> Printf.printf "%d," (Card.to_num x)) r; print_col c (i+1);;

let copy_state state game = 
	let state' = set_state game in
	state'.deposit <- state.deposit;
	state'.columns <- state.columns;
	state'.registers <- state.registers;
	state'.movelist <- state.movelist;
	state';;

let contains_big_seq state game =
	let contains_big_seq_aux n c game =
		List.length c > n + 1
	in let n = FArray.length state.registers in
	let rec check_all_columns state i game =
		try 
			let c = FArray.get state.columns i
				in if (contains_big_seq_aux (n+2) c game) then true
				else check_all_columns state (i+1) game
		with Not_found -> false
	in if (is_sa_mo game) then check_all_columns state 0 game else false;;

let check_gap state =
	(!currennt_best_score - score state) <= ecart

let add_move state s d = 
	state.movelist <- state.movelist @ [(s^" "^d^"\n")];;

let update_best_score state =
	if (score state > !currennt_best_score) then currennt_best_score := score state
	else ();;

let move_id_to_id id move state treated_states game r = 
	let rec move_id_to_id_aux s d move state treated_states game r =
		if (d < 52) then 
			let state' = copy_state state game in
				try 
					let _ = move s (Id d) state' in
					let _ = normalize state' in
					let _ = sort_reg state' in
					if (not (contains_big_seq state' game)
							&& not (States.mem state' treated_states)
								&& check_gap state') then
						let _ = add_move state' (string_of_int (id)) (string_of_int (d)) in
						let _ = update_best_score state' in
						let new_r = add state' r in
							(move_id_to_id_aux s (d+1) move state treated_states game new_r)
					else move_id_to_id_aux s (d+1) move state treated_states game r
				with Move_error -> 
					move_id_to_id_aux s (d+1) move state treated_states game r
		else r
	in move_id_to_id_aux id 0 move state treated_states game r ;;

let move_id_to_col id move state treated_states game r = 
	if (game = Baker) then r
		else
		try 
			let index = get_dst_ind id state in
			match FArray.get state.columns index with
				| exception Not_found -> r
				| l -> if (List.length l > 1) then
							let state' = copy_state state game in
							let _ = move id V state' in
							let _ = normalize state' in
							let _ = sort_reg state' in
							if (not (contains_big_seq state' game)
									&& not (States.mem state' treated_states)
											&& check_gap state') then
								let _ = add_move state' (string_of_int (id)) "V" in
								let _ = update_best_score state' in
									add state' r
							else r
						else r
		with
			Move_error|Not_found -> r;;


let move_id_to_reg id move state treated_states game r = 
	if (game = Baker) then r
	else
		try 
			let state' = copy_state state game in
			let _ = move id T state' in
			let _ = normalize state' in
			let _ = sort_reg state' in
			if (not (contains_big_seq state' game) &&
					not (States.mem state' treated_states)
						&& check_gap state') then
				let _ = add_move state' (string_of_int (id)) "T" in
				let _ = update_best_score state' in
				add state' r
			else
				r
		with
			Move_error -> r;;
	
let valid_moves_from_id id move state treated_states game r_states= 
	let r1 = move_id_to_id id move state treated_states game r_states in
	let r2 = (move_id_to_reg id move state treated_states game ) r1 in
			move_id_to_col id move state treated_states game r2;;

let valide_moves move state treated_states game r = 
	let rec valide_moves_aux move state l treated_states game r = match l with
		| [] -> r
		| id::ids ->  let vmid = valid_moves_from_id id move state treated_states game r
					in  (valide_moves_aux move state ids treated_states game vmid) 
	in valide_moves_aux move state (list_of_id state) treated_states game r;;

let is_winnig state = if (score state = max_score) then true else false;;

let rec write_lines co l = 
	match l with
		| [] -> ()
		| b::bs -> output_string co b;write_lines co bs;;

let write_file filename l= 
	let co = open_out filename in write_lines co l;;

let rec search_aux move state_set treated_states filename game= 
	match max state_set with
		| None -> Printf.printf "INSOLUBLE"; exit 2
		| Some s ->
			match is_winnig s with
			| true -> Printf.printf "SUCCES";write_file (filename) (s.movelist);exit 0 
			| false -> 
				let updated_state = supp (s) (state_set) in
				let new_treated = States.add s treated_states in
				let remaining_states = valide_moves move s new_treated game updated_state in
				search_aux (move) (remaining_states) (new_treated) (filename) (game)

let search filename state_init game =
	let _ = currennt_best_score := score state_init  in
	let _ = sort_reg state_init in
	let move = XpatSolverValidate.move in 
	let state_set = init (state_init) in 
		search_aux (move) (state_set) (States.empty) (filename) (game)
