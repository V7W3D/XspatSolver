open XpatSolverValidate

let max_score = 52;;
let ecart = 10;;

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
		if (score state1 > score state2) then 1 else -1;;

module States = 
	Set.Make (struct type t = state let compare = compare_state end);;

let init_set_states state_init = States.singleton state_init;;

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

let copy_state state = 
	let state' = {
		columns = FArray.init (FArray.length state.columns) 
								(fun i -> try (FArray.get state.columns i) with Not_found -> []);
		deposit = FArray.init (FArray.length state.deposit)
								(fun i -> try FArray.get state.deposit i with Not_found -> 0);
		registers = FArray.init (FArray.length state.registers)
								(fun i -> try FArray.get state.registers i with Not_found -> None);
		movelist = state.movelist;
	}
	in state';;

let compare_card c1 c2 =
	let (rk1, suit1) = c1 in
	let (rk2, suit2) = c2 in
		if (rk1 = rk2 && suit1 < suit2) then true
		else false 

let contains_big_seq state =
	let rec contains_big_seq_aux state n n' elm c =
		if (n>=n') then true
		else 
			try
				let e = (List.nth c n) in
				(compare_card e elm)
				&& (contains_big_seq_aux state (n+1) n' e c)
			with Failure a -> false

	in let n = FArray.length state.registers in
	let rec check_all_columns state i =
		try 
			let c = FArray.get state.columns i
			in try
				let sommet = List.nth c 0 
				in if (contains_big_seq_aux state 1 (n+2) sommet c) then true
				else check_all_columns state (i+1)
			with Failure a -> false
		with Not_found -> false
	in check_all_columns state 0;;

let check_gap state1 state2 =
	(score state1 - score state2) <= ecart

let add_move state s d = 
	state.movelist <- state.movelist @ [(s^" "^d^"\n")];;

let move_id_to_id id move state treated_states = 
	let rec move_id_to_id_aux s d move state treated_states =
		if (d < 52) then 
			let s_ = state in
			let state' = copy_state state in
				try 
					let _ = move s (Id d) state' in
					let _ = normalize state' in
					let _ = sort_reg state' in
					if (not (contains_big_seq state')
							&& not (States.mem state' treated_states)
								&& check_gap state state') then
						let _ = add_move state' (string_of_int (id)) (string_of_int (d)) in
						States.add state' (move_id_to_id_aux s (d+1) move state treated_states)
					else move_id_to_id_aux s (d+1) move state treated_states
				with Move_error -> 
					move_id_to_id_aux s (d+1) move state treated_states
		else States.empty
	in move_id_to_id_aux id 0 move state treated_states;;

let move_id_to_col id move state treated_states = 
	try 
		let index = get_dst_ind id state in
		match FArray.get state.columns index with
			| exception Not_found -> States.empty
			| l -> if (List.length l > 1) then
						let state' = copy_state state in
						let _ = move id V state' in
						let _ = normalize state' in
						let _ = sort_reg state' in
						if (not (contains_big_seq state')
								&& not (States.mem state' treated_states)
							      && check_gap state state') then
							let _ = add_move state' (string_of_int (id)) "V" in
							States.singleton state'
						else States.empty
					else States.empty
	with
		Move_error|Not_found -> States.empty;;


let move_id_to_reg id move state treated_states= 
	try 
		let state' = copy_state state in
		let _ = move id T state' in
		let _ = normalize state' in
		let _ = sort_reg state' in
		if (not (contains_big_seq state') &&
				not (States.mem state' treated_states)
					&& check_gap state state') then
			let _ = add_move state' (string_of_int (id)) "T" in
			States.singleton state'
		else
			States.empty
	with
		Move_error -> States.empty;;
	
let valid_moves_from_id id move state treated_states = 
	let id_states = (move_id_to_id id move state treated_states) in
	let reg_states = (move_id_to_reg id move state treated_states) in
	let col_states = (move_id_to_col id move state treated_states) in
	States.union col_states (States.union id_states reg_states)
			

let valide_moves move state treated_states = 
	let rec valide_moves_aux move state l treated_states = match l with
		| [] -> States.empty
		| id::ids ->  let vmid = valid_moves_from_id id move state treated_states
					in States.union 
						(vmid)
						(valide_moves_aux move state ids treated_states) 
	in valide_moves_aux move state (list_of_id state) treated_states;;

let is_winnig state = if (score state = max_score) then true else false;;

let max state_set = 
	let max_elt = States.max_elt_opt state_set in
	match max_elt with
		| None -> None
		| Some e ->States.find_first_opt 
		(fun x -> (score x = score e) ) state_set  

let rec write_lines co l = 
	match l with
		| [] -> ()
		| b::bs -> output_string co b;write_lines co bs;;

let write_file filename l= 
	let co = open_out filename in
		write_lines co l;;

let rec search_aux move state_set treated_states filename = 
	Printf.printf "current number of states : %d\n"(States.cardinal state_set);
	match max state_set with
		| None -> Printf.printf "INSOLUBLE"; exit 2
		| Some s ->
			Printf.printf ";best score : %d\n"(score s);
			match is_winnig s with
			| true -> Printf.printf "SUCCES";write_file (filename) (s.movelist);exit 0 
			| false -> 
				let updated_state = States.remove s state_set in
				let new_treated = States.add s treated_states in
				let valide_move = valide_moves move s new_treated in
				let new_state = States.union valide_move updated_state in
					search_aux (move) (new_state) (new_treated) (filename)

let search filename state_init =
	let move = XpatSolverValidate.move in 
	let state_set = init_set_states (state_init) in 
		search_aux (move) (state_set) (States.empty) (filename)
