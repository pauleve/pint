
type t_trace = Trace of (Spig.t_state list)
			| Stable of (Spig.t_state list * Spig.t_state list)

type t_prop = float
;;

let extract_tracename = List.map (fun (Spig.Transition name, dest) -> name);;

let transition_exists (Spig.Transition name, dest) = Constraint.NotNull name;;
let transition_never (Spig.Transition name, dest) = Constraint.Null name;;

let string_of_trace string_of_state trace = 
	let string_of_states states = "["^(String.concat "];["
		(List.map string_of_state states))^"]"
	in
	match trace with
		  Trace states -> "Trace("^(string_of_states states)^")"
		| Stable (states, cycle) -> "Stable("^(string_of_states states) ^
			","^(string_of_states cycle)^")"
;;

let rec _always spig = function [] | [_] -> []
	| s1::s2::q ->
		let rset = List.filter (fun (tr,d) -> d <> s2) (Spig.next spig s1)
		in
		let cs = List.map transition_never rset
		in
		cs @ (_always spig (s2::q))
;;

let rec exists spig = function
	  Trace([]) | Trace([_]) -> []
	| Trace(s1::s2::q) -> 
		let rset = List.filter (fun (tr, d) -> d = s2) (Spig.next spig s1)
		in
		if s1 = s2 && List.length rset = 0 then exists spig (Trace (s2::q))
		else (
		assert (List.length rset = 1);
		let cs = transition_exists (List.hd rset)
		in
		cs::(exists spig (Trace (s2::q)))
		)

	| Stable(trace, []) -> exists spig (Trace trace)
	| Stable(trace, h::q) ->
		let cycle = h::q @ [h]
		in
		(exists spig (Trace (trace@cycle))) @ (_always spig cycle)
;;

let rec proportion spig p = function
	  Stable (trace, h::cycle) -> 
	  	(proportion spig p (Trace (trace@[h]))) @ 
			(exists spig (Stable ([], h::cycle)))
	| Stable (trace, []) -> proportion spig p (Trace trace)
	| Trace [] | Trace [_] -> invalid_arg "trace must have at least 2 states!"
	| Trace trace ->
		let rec filter_trace pred = function [] | [_] -> []
			| s1::s2::q ->
				let trs = Spig.next spig s1
				in
				(if List.length trs > 1 then [List.filter (pred s2) trs] else [])
				@ (filter_trace pred (s2::q))
		in

		(* deviant *)
		let deviant = Constraint.ProdSum (List.map extract_tracename 
						(filter_trace (fun s (tr,d) -> d <> s) trace))

		(* following *)
		and following_l = List.map extract_tracename
						(filter_trace (fun s (tr,d) -> d = s) trace)
		in
		let following = List.flatten following_l
		in
		assert ((List.length following) = (List.length following_l));
		let following = Constraint.Prod following

		(* factor *)
		and factor = p /. (1. -. p)

		in
		(Constraint.FactorEqual (following, factor, deviant))
		::exists spig (Trace trace)
;;

