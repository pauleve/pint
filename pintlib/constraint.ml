

type 'a expression = Var of 'a
				| Prod of ('a expression * 'a expression)
				| Sum of ('a expression * 'a expression)
				| Div of ('a expression * 'a expression)

type 'a relation = Null | NotNull
			| FactorEqual of (float * 'a expression)

type 'a constraints = ('a * 'a relation) list
;;

let rec string_of_expression string_of_var e =
	let do_protect curexp child = match curexp, child with
		  (_, Var _) -> false
		| (Prod _, Prod _) | (Sum _, Sum _) | (Div _, Div _) -> false
		| _ -> true
	in
	let protect curexp child = 
		if do_protect curexp child then "("^(string_of_expression string_of_var child)^")"
		else string_of_expression string_of_var child
	in
	match e with
	  Var var -> string_of_var var
	| Prod (c1, c2) -> (protect e c1)^" * "^(protect e c2)
	| Sum (c1, c2) -> (protect e c1)^" + "^(protect e c2)
	| Div (c1, c2) -> (protect e c1)^" / "^(protect e c2)
;;

let string_of_relation string_of_var (var, relop) =
	string_of_var var ^
	match relop with
		  Null -> " = 0"
		| NotNull -> " > 0"
		| FactorEqual (f, expr) -> " = "^(string_of_float f)^" * "^
								(string_of_expression string_of_var expr)
;;

let rec string_of_constraints string_of_var constraints =
	String.concat " ; " (List.map (string_of_relation string_of_var) constraints)
;;

