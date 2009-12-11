
type 'a state = 'a list

type 'a subst = 'a * 'a
;;

let apply_subst state (a, a') = List.map (fun b -> if b = a then a' else b) state
;;

let extract_subst s s' =
	let all_a = Util.list_sub s s'
	and all_a' = Util.list_sub s' s
	in assert (List.length all_a = 1); assert (List.length all_a' = 1);
	List.hd all_a, List.hd all_a'
;;

let rec string_of_substs string_of_state state =  function
	  [] -> "("^(string_of_state state)^")"
	| subst::q -> "("^(string_of_state state)^") -> "^
					string_of_substs string_of_state (apply_subst state subst) q
;;

let contains_subst graph state subst =
	let next = apply_subst state subst
	in
	List.mem next (Graph.next_vertices graph state)
;;
let contains_path graph state substs =
	let folder state subst =
		if contains_subst graph state subst then
			apply_subst state subst
		else
			raise Not_found
	in
	try 
		ignore(List.fold_left folder state substs);
		true
	with Not_found -> false
;;

let count graph =
	let sum acc state =
		acc + List.length (Graph.next_vertices graph state)
	in
	List.fold_left sum 0 (Graph.vertices graph)
;;

let project dyn pred =
	let project_state state = List.filter pred state
	in
	let dyn' = Graph.create 0
	in
	let register_transition s (l, s') =
		Graph.add dyn' (project_state s) (l, project_state s')
	in
	Graph.iter register_transition dyn;
	dyn'
;;

let extract_strict dyn states =
	let dyn' = Graph.create (List.length states)
	in
	let register_transition s (l, s') =
		if List.mem s states && List.mem s' states then
			Graph.add dyn' s (l, s')
	in
	Graph.iter register_transition dyn;
	dyn'
;;
let extract dyn states =
	let dyn' = Graph.create (List.length states)
	in
	let register_transition s (l, s') =
		if List.mem s states then
			Graph.add dyn' s (l, s')
	in
	Graph.iter register_transition dyn;
	dyn'
;;


