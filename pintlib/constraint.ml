

type 'a expression = Var of 'a
				| Prod of ('a expression * 'a expression)
				| Sum of ('a expression * 'a expression)
				| Div of ('a expression * 'a expression)

type 'a relop = Equal of float | Greater of float
			| FactorEqual of (float * 'a expression)

type 'a constraints =
	  Empty
	| Relation of ('a * 'a relop)
	| Or of ('a constraints * 'a constraints)
	| And of ('a constraints * 'a constraints)
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
		  Equal x -> " = "^(string_of_float x)
		| Greater x -> " > "^(string_of_float x)
		| FactorEqual (f, expr) -> " = "^(string_of_float f)^" * "^
								(string_of_expression string_of_var expr)
;;

let rec string_of_constraints string_of_var constraints = 
	let protect e = function
		  true -> "(" ^ (string_of_constraints string_of_var e) ^ ")"
		| false -> (string_of_constraints string_of_var e)
	and is_and = function And _ -> true | _ -> false
	and is_or = function Or _ -> true | _ -> false
	in
	match constraints with 
      Empty -> "$"
	| Relation r -> string_of_relation string_of_var r
	| And (e1, e2) -> (protect e1 (is_or e1))^" AND "^(protect e2 (is_or e2))
	| Or (e1, e2) -> (protect e1 (is_and e1))^" OR "^(protect e2 (is_and e2))
;;

let disjonction e1 e2 = Or(e1, e2);;
let conjonction e1 e2 = And(e1, e2);;

let rec noempty = function
	  And (e, Empty) | And (Empty, e) | Or (Empty, e) | Or (e, Empty) -> noempty e
	| And (e1, e2) -> And (noempty e1, noempty e2)
	| Or (e1, e2) -> Or (noempty e1, noempty e2)
	| Empty -> Empty
	| Relation r -> Relation r
;;

let rec dnf = function
	| And (Or (e1, e2), e3) | And (e3, Or (e1,e2)) -> Or (dnf (And (e1,e3)), dnf (And (e2,e3)))
	| And (e1, e2) -> And (dnf e1, dnf e2)
	| Or (e1, e2) -> Or (dnf e1, dnf e2)
	| Empty -> Empty
	| Relation x -> Relation x
;;

