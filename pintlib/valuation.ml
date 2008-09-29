
type bound = Excluded of float | Included of float | Inf
type values = Interval of (bound * bound)

type 'a t = ('a * values) list
;;

exception Invalid_Values;;

let bound_left b1 b2 = match b1, b2 with
	  (b, Inf) | (Inf, b) -> b
	| (Included v1, Included v2)
	| (Excluded v1, Excluded v2) -> if v1 <= v2 then b1 else b2
	| (Included v1, Excluded v2) -> if v1 < v2 then b1 else b2
	| (Excluded v1, Included v2) -> if v2 < v1 then b2 else b1
;;
let bound_right b1 b2 = match b1, b2 with
	  (b, Inf) | (Inf, b) -> Inf
	| (Included v1, Included v2)
	| (Excluded v1, Excluded v2) -> if v1 >= v2 then b1 else b2
	| (Included v1, Excluded v2) -> if v1 < v2 then b1 else b2
	| (Excluded v1, Included v2) -> if v2 < v1 then b2 else b1
;;
let bound_leq br bl = match (br, bl) with
	  (Inf, Inf) -> true | (b, Inf) -> true | (Inf, b) -> false
	| (Included v1, Included v2) -> v1 <= v2
	| (Excluded v1, Excluded v2)
	| (Included v1, Excluded v2)
	| (Excluded v1, Included v2) -> v1 < v2
;;

let string_of_values = function
	Interval (br, bl) -> 
		" in " ^
		(match br with 
			  Excluded x -> "("^(string_of_float x)
			| Included x -> "["^(string_of_float x)
			| Inf -> "(Inf"
		) ^ ".." ^
		(match bl with
			  Excluded x -> (string_of_float x)^")"
			| Included x -> (string_of_float x)^"]"
			| Inf -> "Inf)"
		)
;;
let string_of_valuation string_of_var valuation = 
	String.concat "\n"
	(List.map (fun (var, values) ->
				"val "^(string_of_var var)^(string_of_values values))
		valuation)
;;
let string_of_valuations string_of_var = function [] -> "No valuation possible"
	| valuations -> String.concat "===== OR =====\n"
		(List.map (string_of_valuation string_of_var) valuations)
;;

let values_intersection v1 v2 =
	match v1 with
	  Interval (f1, t1) ->
	  	match v2 with
		  Interval (f2, t2) -> 
		  	let f = bound_right f1 f2
			and t = bound_left t1 t2
			in if bound_leq f t then Interval (f, t) else raise Invalid_Values
;;

let valuation_append valuation entry values =
	let cur_values = try List.assoc entry valuation
		with Not_found -> Interval (Included 0., Inf)
	in 
	(entry, values_intersection cur_values values)::
	(List.remove_assoc entry valuation)
;;

let rec valuation_merge valuation = function [] -> valuation
	| (entry, values)::q -> 
		valuation_merge (valuation_append valuation entry values) q
;;

let valuations_merge valuations v1 = 
	List.flatten (
		List.map (fun v2 -> try [valuation_merge v1 v2] with Invalid_Values -> [])
			valuations)
;;

let valuation_of_relation = function
	  (Constraint.Var x, Constraint.Equal, Constraint.Value v)
	| (Constraint.Value v, Constraint.Equal, Constraint.Var x) ->
	  	[x, Interval (Included v, Included v)]
	| (Constraint.Var x, Constraint.Greater, Constraint.Value v) ->
		[x, Interval (Excluded v, Inf)]
	| (Constraint.Value v, Constraint.Greater, Constraint.Var x) ->
		[x, Interval (Included 0., Excluded v)]
	| _ -> []
;;

let rec valuations_of_constraints = function
	  Constraint.And (e1, e2) -> 
	  	let vs1 = valuations_of_constraints e1
		and vs2 = valuations_of_constraints e2
		in List.flatten (List.map (valuations_merge vs1) vs2)
	| Constraint.Or (e1, e2) ->
	  	let vs1 = valuations_of_constraints e1
		and vs2 = valuations_of_constraints e2
		in vs1 @ vs2
	| Constraint.Empty -> [[]]
	| Constraint.Relation r -> [valuation_of_relation r]
;;

