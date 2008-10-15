
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

