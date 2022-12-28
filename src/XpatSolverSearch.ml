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

let get_max_state state_set = States.max_elt state_set

let search_aux move state_set = ()

let search move state_init = 
	let state_set = init_set_states state_init in 
		search_aux move state_set

