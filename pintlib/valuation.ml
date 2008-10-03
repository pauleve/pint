
type 'a equation = (float * 'a list) list * float
;;
exception No_solution;;

let string_of_prod string_of_var (f, vars) = (string_of_float f)^"*"^
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
	((-1., List.sort Pervasives.compare e1)::List.map (fun prod -> f, List.sort Pervasives.compare prod) (develop e2)), 0.
;;

let equation_null_var var (sumprod, value) = 
	List.filter (fun (f, vars) -> not (List.mem var vars)) sumprod, value
;;
let equations_null_var var = List.map (equation_null_var var)
;;

let prod_apply_var_value (var, value) (f, vars) =
	let vars, a = Util.list_separate (fun v -> v <> var) vars
	in
	f *. (value ** (float_of_int (List.length a))), vars
;;
let equation_apply_var_value (var,value) (sumprod, value) =
	let sumprod = List.map (prod_apply_var_value (var, value)) sumprod
	in
	let sumprod, new_values = Util.list_separate (fun (f, vars) -> vars <> []) sumprod
	in
	let value = List.fold_left (fun value (f,vars) -> value-.f) value new_values
	in
	sumprod, value
;;
let equations_apply_var_value (var, values) =
	List.map (equation_apply_var_value (var, values))
;;

let rec equation_vars = function [],_ -> []
	| (f, vars)::q,v -> Util.list_uniq (vars @ equation_vars (q,v))
;;
let rec equations_vars = function [] -> []
	| eq::q -> Util.list_uniq ((equation_vars eq) @ equations_vars q)
;;

let sanitize_equations (eqs:'a equation list) = List.filter
	(fun eq -> match eq with [], 0. -> false
				| [], _ -> raise No_solution | _ -> true)
		eqs
;;

let rec free_equation_solve = function 
	  [], 0. -> [], ([],0.)
	| [], _ -> raise No_solution
	| (f, [])::q, value -> free_equation_solve (q, value)

	| [1.0, a::vars], value ->
		let all_a, all_r = Util.list_separate (fun v -> v = a) (a::vars)
		in
		(match all_r with
				(* a^n = value -> a = value^(1/n) *)
			  [] -> [(a, value ** (1./.(float_of_int (List.length all_a))))], ([],0.)
			| _ ->
				let valuation, eq = free_equation_solve 
										([1.0, all_r], value)
				in
				(a,1.)::valuation, eq)
	| [f,vars], value -> free_equation_solve ([(1.0, vars)], value/.f)

	| (f, vars)::sumprod, value ->
		let my_vars = equation_vars ([(f,vars)],value)
		in
		if [] = Util.list_intersection my_vars 
					(equation_vars (sumprod, value)) then
			let valuation, eq = free_equation_solve (sumprod, value-.f)
			in
			(List.map (fun var -> (var,1.)) my_vars) @ valuation, eq
		else [], ((f,vars)::sumprod, value)
;;

let equations_solve valuation eqs =
	let equation_is_free equations eq =
		let vars = equation_vars eq
		in
		1 >= List.length (List.filter 
			(fun eq -> [] <> Util.list_intersection vars (equation_vars eq))
			equations)
	in
	let free_eqs, eqs = Util.list_separate (equation_is_free eqs) eqs
	in
	let vals, free_eqs = List.split (List.map free_equation_solve free_eqs)
	in
	let valuation = valuation @ (List.flatten vals)
	and free_eqs = sanitize_equations free_eqs
	in
	valuation, sanitize_equations (free_eqs @ eqs)
;;

let valuation_of_constraints constraints =
	(* 1. extract Null and NotNull constraints, init valuation *)
	let nullvars = List.flatten
		(List.map (fun x -> match x with Constraint.Null var -> [var] | _ -> []) constraints)
	and notnullvars = List.flatten
		(List.map (fun x -> match x with Constraint.NotNull var -> [var] | _ -> []) constraints)
	in
	let valuation = List.map (fun var -> (var, 0.)) nullvars
	in

	(* 2. extract factorequals  *)
	let equations = List.map equation_of_factorequal (List.flatten
		(List.map (fun x -> match x with Constraint.FactorEqual feq -> [feq] | _ -> []) 
			constraints))
	in

	(* 3. apply null vars *)
	let equations = List.fold_left (fun eqs var -> equations_null_var var eqs)
							equations nullvars
	in

	(* 4. solve system *)
	let valuation, equations = equations_solve valuation equations
	in

	(* 5. check coherence *)
	let nullvars = nullvars @ List.flatten
		(List.map (fun (var,value) -> match value with 0. -> [var] | _ -> []) valuation)
	and notnullvars = notnullvars @ List.flatten
		(List.map (fun (var,value) -> match value with 0. -> [] | _ -> [var]) valuation)
	in
	(if [] <> Util.list_intersection nullvars notnullvars then
		raise No_solution);

	(* 6. valuate resting vars *)
	let eqvars = equations_vars equations
	in
	let valuation = valuation @
		List.map (fun var -> (var,1.0)) (List.filter
			(fun var -> not (List.mem var eqvars))
			notnullvars)
	in

	valuation, equations
;;

(*
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

*)
