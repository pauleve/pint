
type t_trace = Trace of (Spig.t_state list)
			| Stable of (Spig.t_state list * Spig.t_state list)

type t_prop = float
;;

let transition_exists (Spig.Transition name, dest) = (name, Constraint.NotNull);;
let transition_never (Spig.Transition name, dest) = (name, Constraint.Null);;

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
		assert (List.length rset = 1);
		let cs = transition_exists (List.hd rset)
		in
		cs::(exists spig (Trace (s2::q)))

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
		let extract_tracename traces =
			List.map (fun (Spig.Transition name, dest) -> name) traces
		in

		(* deviant *)
		let deviant = List.map extract_tracename 
						(filter_trace (fun s (tr,d) -> d <> s) trace)
		in
		let sum_of_list = function [] -> raise (Invalid_argument "sum_of_list")
			| [t] -> t
			| h::q -> List.fold_left (fun exp t -> Constraint.Sum (exp, t)) h q
		and prod_of_list = function [] -> raise (Invalid_argument "prod_of_list")
			| [t] -> t
			| h::q -> List.fold_left (fun exp t -> Constraint.Prod (exp, t)) h q
		and vars_of_list = List.map (fun t -> Constraint.Var t)
		in
		let deviant_exp = prod_of_list (List.map sum_of_list
				(List.map vars_of_list deviant))
		in

		(* following *)
		let following_l = List.map extract_tracename
							(filter_trace (fun s (tr,d) -> d = s) trace)
		in
		let following = List.flatten following_l
		in
		assert ((List.length following) = (List.length following_l));

		(* x = factor * (deviant / following - x) *)
		let factor = p /. (1. -. p)
		in
		let div = match following with [] -> failwith "no following..."
			| [t] -> deviant_exp
			| t::q -> Constraint.Div (deviant_exp, prod_of_list (vars_of_list q))
		in
		let relation = Constraint.FactorEqual (factor, div)
		and var = List.hd following
		in 
		(var, relation)::exists spig (Trace trace)
;;

