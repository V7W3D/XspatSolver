let max_score = 52

let score state = 
	let score_aux state index = 
		match FArray.get state.deposit index with
			| exception Not_found -> 0
			| c -> c + FArray.get state.deposit (index+1)
	in score_aux state 0

let compare_state state1 state2 = 
	if ((Stdlib.compare state1.registers state2.registers) = 0
			&& (Stdlib.compare state1.columns state2.columns) = 0) then 0
	else 
		if (score state1 > score state2) then 1 else -1 

module States = 
	Set.Make (struct type t = XpatSolver.state let compare = compare_state end)

let init_set_states state_init = States.singleton state_init

let get_max_state state_set = States.max_elt_opt state_set

let valide_move move state = ()

let set_add_all state move_list = ()

let is_winnig state = if (score state = max_score) then true else false

let rec search_aux move state_set saved_state =
	match get_max_state state_set with
		| None -> false
		| Some s -> match is_winnig s with
			| true -> true
			| false -> match valide_move move s with 
				| None -> search_aux saved_state States.empty
				| Some l -> let updated_state = States.remove s state_set in
						let filled_updated_state = set_add_all s l in
						search_aux move filled_updated_state updated_state

let search move state_init = 
	let state_set = init_set_states state_init in 
		search_aux move state_set

