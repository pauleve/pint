
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
	| Sum (None, a) -> string_of_atype a
	| Prod (None, e) | Prod (e, None) -> string_of_elexpr string_of_atype e
	| Sum (e,a) -> "("^(string_of_atype a)^" + "^(string_of_elexpr string_of_atype e)^")"
	| Prod (e1,e2) -> "("^(string_of_elexpr string_of_atype e1)^" * "^(string_of_elexpr string_of_atype e2)^")"
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

let rec string_of_expression string_of_atype expr = 
	let protect e = function
		  true -> "(" ^ (string_of_expression string_of_atype e) ^ ")"
		| false -> (string_of_expression string_of_atype e)
	and is_and = function And _ -> true | _ -> false
	and is_or = function Or _ -> true | _ -> false
	in
	match expr with 
      Empty -> "$"
	| Relation (a, r, b) -> (string_of_element string_of_atype a) ^ " " ^
						(string_of_relop r) ^ " " ^ 
						(string_of_element string_of_atype b)
	| And (e1, e2) -> (protect e1 (is_or e1)) ^ " AND " ^	(protect e2 (is_or e2))
	| Or (e1, e2) -> (protect e1 (is_and e1)) ^ " OR " ^	(protect e2 (is_and e2))
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

