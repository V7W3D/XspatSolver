open XpatSolverValidate

let max_score = 52;;

let score state = 
	let rec score_aux state index = 
		match FArray.get (state.deposit) (index) with
			| exception Not_found -> 0
			| c -> c + score_aux state (index+1)
	in score_aux state 0;;

let compare_state_aux state1 state2 reg1 reg2 = 
	if ((Stdlib.compare reg1 reg2) = 0
			&& (Stdlib.compare state1.columns state2.columns) = 0) then 0
	else 
		if (score state1 > score state2) then 1 else -1;;

let compare_state state1 state2 = 
	let liste_register1 = FArray.to_list state1.registers in
		let liste_register2 = FArray.to_list state2.registers in
			let sorted_list1 = 
				List.sort (fun a' b' -> 
					match (a',b') with 
						| (Some a, Some b) -> (Card.to_num a) - (Card.to_num b)
						| (None , _) -> -1
						| (_, None) -> 1
					) 
				liste_register1
			in let sorted_list2 = 
				List.sort (fun a' b' -> 
					match (a',b') with 
						| (Some a, Some b) -> (Card.to_num a) - (Card.to_num b)
						| (None , _) -> -1
						| (_, None) -> 1
					) liste_register2
			in compare_state_aux state1 state2 sorted_list1 sorted_list2;;

module States = 
	Set.Make (struct type t = state let compare = compare_state end);;

let init_set_states state_init = States.singleton state_init;;

let get_max_state state_set = States.max_elt_opt state_set;;

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

let copy_state state = 
	let state' = {
		columns = FArray.init (FArray.length state.columns) 
								(fun i -> try FArray.get state.columns i with Not_found -> []);
		deposit = FArray.init (FArray.length state.deposit)
								(fun i -> try FArray.get state.deposit i with Not_found -> 0);
		registers = FArray.init (FArray.length state.registers)
								(fun i -> try FArray.get state.registers i with Not_found -> None);
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


let move_id_to_id id move state = 
	let rec move_id_to_id_aux s d move state =
		if (d < 52) then 
			let state' = copy_state state in
				try 
					let _ = set_state_s state' in
					let _ = move s (Id d) in
					if (not (contains_big_seq state')) then
						States.add state' (move_id_to_id_aux s (d+1) move state)
					else move_id_to_id_aux s (d+1) move state
				with Move_error -> 
					move_id_to_id_aux s (d+1) move state
		else States.empty
	in move_id_to_id_aux id 1 move state;;

let move_id_to_col id move state = 
	try 
		let _ = set_state_s state in
		let index = get_dst_ind id in
		match FArray.get state.columns index with
			| exception Not_found -> States.empty
			| l -> if (List.length l > 1) then
						let state' = copy_state state in
						let _ = set_state_s state' in
						let _ = move id V in
						if (not (contains_big_seq state')) then
							States.singleton state'
						else States.empty
					else States.empty
	with
		Move_error|Not_found -> States.empty;;


let move_id_to_reg id move state = 
	try 
		let state' = copy_state state in
		let _ = set_state_s state' in
		let _ = move id T in
		if (not (contains_big_seq state')) then
			States.singleton state'
		else
			States.empty
	with
		Move_error -> States.empty;;
	
let valid_moves_from_id id move state = 
	States.union 
		(move_id_to_id id move state)
		(States.union (move_id_to_col id move state) (move_id_to_reg id move state));;

let valide_moves move state = 
	let rec valide_moves_aux move state l = match l with
		| [] -> States.empty
		| id::ids ->  States.union 
						(valid_moves_from_id id move state)
						(valide_moves_aux move state ids) 
	in let s = valide_moves_aux move state (list_of_id state) in
		if States.is_empty s then None else Some s ;;

let is_winnig state = if (score state = max_score) then true else false;;

let rec search_aux move state_set acc = 
	match get_max_state state_set with
		| None -> false
		| Some s -> let _ = set_state_s s
			in let _ = normalize () in
			match is_winnig s with
			| true -> true
			| false -> match valide_moves move s with 
				| None -> begin
							match acc with 
								| [] -> false
								| b::bs -> search_aux move b bs
						  end
				| Some new_states -> let updated_state = States.remove s state_set in
						search_aux (move) (new_states) (updated_state :: acc)

let search () =
	let move = XpatSolverValidate.move in 
	let state_init = XpatSolverValidate.get_state () in
	let state_set = init_set_states (state_init) in 
		search_aux move (state_set) ([])