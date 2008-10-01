

type 'a expleft = Prod of ('a list)
type 'a expright = ProdSum of ('a list list)

type 'a relation = Null of 'a | NotNull of 'a
			| FactorEqual of ('a expleft * float * 'a expright)

type 'a constraints = 'a relation list
;;

let string_of_prod string_of_var (Prod vars) = 
	String.concat "." (List.map string_of_var vars)
;;
let string_of_prodsum string_of_var (ProdSum sums) =
	"(" ^ (String.concat ") * ("
		(List.map (fun vars -> String.concat "+" (List.map string_of_var vars))
			sums)) ^ ")"
;;

let string_of_relation string_of_var = function
	  Null var -> (string_of_var var) ^ " = 0"
	| NotNull var -> (string_of_var var) ^ " > 0"
	| FactorEqual (e1, f, e2) -> 
		(string_of_prod string_of_var e1) ^ " = " ^ (string_of_float f) ^
			" * " ^ (string_of_prodsum string_of_var e2)
;;

let rec string_of_constraints string_of_var constraints =
	String.concat " ; " (List.map (string_of_relation string_of_var) constraints)
;;

