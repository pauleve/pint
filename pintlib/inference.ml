
type t_trace = Trace of (Spig.t_state list)
			| Stable of (Spig.t_state list * t_state list)
type t_prop = float
type t_truth = Exists of t_trace
			| Proportion of t_trace * t_prop
;;

let xxx = function
	  [] | [s] -> ()
	| s1::s2::q ->
		Spig.transitions s1 s2

let algo1 spig truth =
	let cs = Constraint.create 10 in
	( match truth with
	  Exists(trace) -> (
		  	match trace with
			  Trace(trace) -> ()
			| Stable(trace, cycle) -> ()
		)
	| Proportion(trace) -> ()
	);
	cs
;;


