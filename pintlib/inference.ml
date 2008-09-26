
type t_trace = Trace of (Spig.t_state list)
			| Stable of (Spig.t_state list * Spig.t_state list)

type t_prop = float
;;

let rec exists spig = function
	  Trace([]) | Trace([_]) -> Constraint.Empty
	| Trace(s1::s2::q) -> 
		let rset = List.filter (fun (tr, d) -> d = s2) (Spig.next spig s1)
		in
		let make_constraint = function (Spig.Transition name,d) -> 
			Constraint.Relation(Constraint.Var name, Constraint.Greater, Constraint.Value 0.)
		in
		let cs = List.map make_constraint rset
		in
		let c = List.fold_left Constraint.disjonction Constraint.Empty cs
		in Constraint.And(c, exists spig (Trace (s2::q)))

	| Stable(trace, []) -> exists spig (Trace trace)
	| Stable(trace, h::q) ->
		let cycle = h::q @ h::[]
		in
		let rec no_deviation = function [] | [_] -> Constraint.Empty
			| s1::s2::q ->
				let rset = List.filter (fun (tr,d) -> d <> s2) (Spig.next spig s1)
				in
				let make_constraint = function (Spig.Transition name,d) ->
					Constraint.Relation(Constraint.Var name, Constraint.Equal, Constraint.Value 0.)
				in
				let cs = List.map make_constraint rset
				in
				let c = List.fold_left Constraint.conjonction Constraint.Empty cs
				in
				Constraint.And(c, no_deviation (s2::q))
		in
		Constraint.And(exists spig (Trace (trace@cycle)), no_deviation cycle)
;;

let rec proportion spig p = function
	  Stable (trace, h::cycle) -> Constraint.And(proportion spig p (Trace (trace@[h])),
	  											exists spig (Stable ([], h::cycle)))
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
		let following = filter_trace (fun s (tr,d) -> d = s) trace
		and deviant = filter_trace (fun s (tr,d) -> d <> s) trace
		in
		let build_sum e = fun (Spig.Transition a, d) -> Constraint.Sum(e, a)
		in
		let build_prod e trs = Constraint.Prod(e, List.fold_left build_sum Constraint.None trs)
		in
		let vars_following = Constraint.Vars (List.fold_left build_prod Constraint.None following)
		and vars_deviant = Constraint.Vars (List.fold_left build_prod Constraint.None deviant)
		in
		let cs = Constraint.Relation(vars_following, Constraint.FactorEqual (p/.(1.-.p)), vars_deviant)
		in
		Constraint.And (exists spig (Trace trace), cs)
;;

