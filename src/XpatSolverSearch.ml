let max_score = 52

let score state = 
	let score_aux state index = 
		match FArray.get state.deposit index with
			| exception Not_found -> 0
			| c -> c + score_aux state (index+1)
	in score_aux state 0

let compare_state state1 state2 = 
	let liste_register1 = FArray.to_list state1.registers in
		let liste_register2 = FArray.to_list state2.registers in
			let sorted_list1 = 
				List.sort (fun a b -> (Card.to_num a) - (Card.to_num b)) liste_register1
			in let sorted_list2 = 
				List.sort (fun a b -> (Card.to_num a) - (Card.to_num b)) liste_register2
			in compare_state_aux state1 state2 sorted_list1 sorted_list2

let compare_state_aux state1 state2 reg1 reg2 = 
	if ((Stdlib.compare reg1 reg2) = 0
			&& (Stdlib.compare state1.columns state2.columns) = 0) then 0
	else 
		if (score state1 > score state2) then 1 else -1 

module States = 
	Set.Make (struct type t = XpatSolver.state let compare = compare_state end)

let init_set_states state_init = States.singleton state_init

let get_max_state state_set = States.max_elt_opt state_set

let valide_moves move state = ()

let is_winnig state = if (score state = max_score) then true else false

let rec search_aux move state_set saved_state =
	match get_max_state state_set with
		| None -> false
		| Some s -> match is_winnig s with
			| true -> true
			| false -> match valide_moves move s with 
				| None -> search_aux saved_state States.empty
				| Some new_state -> let updated_state = States.remove s state_set in
						search_aux move new_state updated_state

let search move state_init = 
	let state_set = init_set_states state_init in 
		search_aux move state_set