
type 'a elexpr = None | Sum of ('a elexpr * 'a) | Prod of ('a elexpr * 'a elexpr)
type 'a element = Value of float | Var of 'a | Vars of 'a elexpr
type relop = Equal | FactorEqual of float | Greater

type 'a expression =
	  Empty
	| Relation of ('a element * relop * 'a element)
	| Or of ('a expression * 'a expression)
	| And of ('a expression * 'a expression)
;;

let rec string_of_elexpr string_of_atype = function
	  None -> "$"
	| Sum(e,a) -> "("^(string_of_atype a)^" + "^(string_of_elexpr string_of_atype e)^")"
	| Prod(e1,e2) -> "("^(string_of_elexpr string_of_atype e1)^" * "^(string_of_elexpr string_of_atype e2)^")"
;;
let string_of_element string_of_atype = function
	  Value(x) -> string_of_float x
	| Var(a) -> string_of_atype a
	| Vars(elexpr) -> string_of_elexpr string_of_atype elexpr
;;
let string_of_relop = function
	  Equal -> "="
	| FactorEqual(x) -> "= "^string_of_float(x)
	| Greater -> ">"
;;
let rec string_of_expression string_of_atype = function
	  Empty -> "$"
	| Or(e1, e2) -> (string_of_expression string_of_atype e1) ^ " OR " ^ 
						(string_of_expression string_of_atype e2)
	| And(e1, e2) -> "{" ^ (string_of_expression string_of_atype e1) ^ ", " ^
						(string_of_expression string_of_atype e2) ^ "}"
	| Relation(a, r, b) -> (string_of_element string_of_atype a) ^ " " ^
						(string_of_relop r) ^ " " ^ 
						(string_of_element string_of_atype b)
;;

let disjonction e1 e2 = Or(e1, e2);;
let conjonction e1 e2 = And(e1, e2);;

