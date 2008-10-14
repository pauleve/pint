
type 'a state = 'a list

type 'a subst = 'a * 'a
;;

let apply_subst state (a, a') = List.map (fun b -> if b = a then a' else b) state
;;

let rec string_of_substs string_of_state state =  function
	  [] -> "("^(string_of_state state)^")"
	| subst::q -> "("^(string_of_state state)^") -> "^
					string_of_substs string_of_state (apply_subst state subst) q
;;

