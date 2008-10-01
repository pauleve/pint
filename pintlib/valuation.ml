
type 'a equation = (float * 'a list) list * float
;;
exception No_solution;;

let merge_factorequal (Constraint.Prod e1, f, Constraint.ProdSum e2)
						(Constraint.Prod e1', f', Constraint.ProdSum e2') = 
	(Constraint.Prod (e1@e1'), f*.f', Constraint.ProdSum (e2@e2'))
;;

let string_of_prod string_of_var (f, vars) = (string_of_float f)^"."^
	String.concat "." (List.map string_of_var vars)
;;
let rec string_of_sumprod string_of_var = function [] -> "0"
	| [prod] -> string_of_prod string_of_var prod
	| prod::q -> (string_of_prod string_of_var prod)^" + "^string_of_sumprod string_of_var q
;;
let string_of_equation string_of_var (sumprod, value) =
	(string_of_sumprod string_of_var sumprod) ^ " = " ^ string_of_float value
;;
let string_of_valuation string_of_var valuation =
	String.concat "\n" (List.map (fun (var,value) -> 
		"val "^(string_of_var var)^" = "^(string_of_float value)) valuation)
;;

let equation_of_factorequal (Constraint.Prod e1, f, Constraint.ProdSum e2) =
	let develop prodsum =
		let rec _develop sumprod = function [] -> sumprod
			| sum::q -> _develop (Util.cross sumprod sum) q
		in _develop (List.map (fun x -> [x]) (List.hd prodsum)) (List.tl prodsum)
	in
	((-1., e1)::List.map (fun prod -> f, prod) (develop e2)), 0.
;;

let equation_null_var (sumprod, value) var = 
	List.filter (fun (f, vars) -> not (List.mem var vars)) sumprod, value
;;
let equation_unit_var (sumprod, value) var = 
	let sumprod = List.map (fun (f,vars) -> f, Util.list_remove var vars) sumprod
	in
	let value = List.fold_left (fun value (f,vars) -> if vars = [] then value-.f else value) value sumprod
	and sumprod = List.filter (fun (f,vars) -> vars <> []) sumprod
	in
	sumprod, value
;;
let rec equation_vars = function [],_ -> []
	| (f, vars)::q,v -> Util.list_uniq (vars @ equation_vars (q,v))
;;
let equation_var_is_free (sumprod, value) var = 1 >= List.length (List.filter
	(fun (f,vars) -> List.mem var vars) sumprod)
;;

let valuation_set valuation var value = 
	try
		if List.assoc var valuation <> value then raise No_solution
		else valuation
	with Not_found -> 
		(var, value)::valuation
;;

let rec equation_solve valuation = function 
	  [f, [var]], value -> (valuation_set valuation var (value/.f)), ([],0.)
	| eq -> 
		let freevars = List.filter (equation_var_is_free eq) (equation_vars eq)
		in
		match freevars with
			  [] -> valuation, eq
			| var::q -> equation_solve (valuation_set valuation var 1.) (equation_unit_var eq var)
;;

let equation_solved = function [],0. -> true | _ -> false;;

let valuation_of_constraints constraints =
	(* 1. extract Null and NotNull constraints, init valuation *)
	let nullvars = List.flatten
		(List.map (fun x -> match x with Constraint.Null var -> [var] | _ -> []) constraints)
	and notnullvars = List.flatten
		(List.map (fun x -> match x with Constraint.NotNull var -> [var] | _ -> []) constraints)
	in
	let valuation = List.map (fun var -> (var, 0.)) nullvars
	in

	(* 2. extract factorequals and merge them *)
	let factorequals = List.flatten 
		(List.map (fun x -> match x with Constraint.FactorEqual feq -> [feq] | _ -> []) constraints)
	in
	let valuation, equation = match factorequals with
		  [] -> valuation, ([],0.)
		| h::q -> let gfactorequal = List.fold_left merge_factorequal h q
			in
			(* 3. convert to equation, and solve it *)
			let equation = equation_of_factorequal gfactorequal
			in
			let valuation, equation = equation_solve valuation equation
			in
			(* 4. check coherence *)
			let nullvars = nullvars @ List.flatten
				(List.map (fun (var,value) -> match value with 0. -> [var] | _ -> []) valuation)
			and notnullvars = notnullvars @ List.flatten
				(List.map (fun (var,value) -> match value with 0. -> [] | _ -> [var]) valuation)
			in
			match Util.list_intersection nullvars notnullvars with
				  [] -> valuation, equation
				| _ -> raise No_solution
	in
	let eqvars = equation_vars equation
	in
	let valuation = valuation @
		List.map (fun var -> (var,1.0)) (List.filter
			(fun var -> not (List.mem var eqvars))
			notnullvars)
	in
	valuation, equation
;;

