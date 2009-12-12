
type 'a expr = 
	L of 'a | F | T | Not of 'a expr | And of ('a expr * 'a expr) | Or of ('a expr * 'a expr)
;;

module type LitType = sig 
	type t
	val to_string : t -> string
	val tautology : (bool * t) -> (bool * t) -> bool (* returns true if both arguments 
															are contrary *)
end;;

module Manipulator ( Lit : LitType ) =
struct
	module Lit = Lit

	module LSet = Set.Make (struct type t = (bool * Lit.t) let compare = compare end)

	let rec to_string = function
		  T -> "T" | F -> "F" | L x -> Lit.to_string x
		| Not x -> "~"^to_string x
		| And (x1, x2) -> "("^to_string x1^") AND ("^to_string x2^")"
		| Or (x1, x2) -> "("^to_string x1^") OR ("^to_string x2^")"
	;;

	let dnf_simplify dnf =
		let rec dnf_simplify = function [] -> [] |
			s1::t -> 
				if List.exists (fun s2 -> LSet.subset s2 s1) t then
					dnf_simplify t
				else
					s1::dnf_simplify t
		in
		let dnf = List.sort (fun s1 s2 -> compare (LSet.cardinal s2) (LSet.cardinal s1)) dnf
		in
		dnf_simplify dnf
	;;

	let expr_of_dnf = function
		  None -> F
		| Some [] -> T
		| Some lsets ->	
			let disj expr lset =
				let conj (pos,x) expr =
					And (expr, if pos then L x else (Not (L x)))
				in
				Or (expr, LSet.fold conj lset T)
			in
			List.fold_left disj F lsets
	;;

	let dnf expr =
		let rec dnf = function 
			  (Not _ | L _ | T | F) as l -> l
			| Or (e1, e2) -> Or (dnf e1, dnf e2)
			| And (e1, e2) -> distr (dnf e1, dnf e2) 
		and distr = function 
			  (Or (e1, e2), e) -> Or (distr (e1, e), distr (e2, e))
			| (e, Or (e1, e2)) -> Or (distr (e, e1), distr (e, e2))
			| c -> And c
		and nnf = function (* forward negations to literals *)
			  (L _ | T | F) as l -> l
			| And (e1, e2) -> And (nnf e1, nnf e2)
			| Or (e1, e2) -> Or (nnf e1, nnf e2)
			| Not (And (e1, e2)) -> Or (nnf (Not e1), nnf (Not e2))
			| Not (Or (e1, e2)) -> And (nnf (Not e1), nnf (Not e2))
			| Not (Not e) -> nnf e
			| Not T -> F | Not F -> T
			| Not e -> Not (nnf e)
		and noTF = function
			  And (e, F) | And (F, e) -> F
			| Or (e, T) | Or (T, e) -> T
			| And (e, T) | And (T, e) | Or (F, e) | Or (e, F) -> noTF e
			| And (e1, e2) -> (let e1', e2' = noTF e1, noTF e2
				in let e' = And (e1', e2')
				in match (e1',e2') with F,_ | _,F | T,_ | _,T -> noTF e'
					| _ -> e')
			| Or (e1, e2) -> (let e1', e2' = noTF e1, noTF e2
				in let e' = Or (e1', e2')
				in match (e1',e2') with F,_ | _,F | T,_ | _,T -> noTF e'
					| _ -> e')
			| e -> e
		and to_list = function
			  Or (e1, e2) -> to_list e1 @ to_list e2
			| And (e1, e2) -> [LSet.union (List.hd (to_list e1)) (List.hd (to_list e2))]
			| Not (L x) -> [LSet.singleton (false, x)]
			| L x -> [LSet.singleton (true, x)]
			| T -> []
			| _ -> raise (Invalid_argument "dnf.to_list")
		in
		let to_list = function
			  F -> None
			| expr -> Some (dnf_simplify (to_list expr))
		in
		(*DEBUG*
		let e = noTF expr
		in print_endline ". noTF done";
		let e = nnf e
		in print_endline ". nnf done";
		print_endline (to_string e);
		let e = dnf e
		in print_endline ". dnf done";
		let e = to_list e
		in print_endline ". to_list done";
		e **)
		to_list (dnf (nnf (noTF expr)))
	;;

	let string_of_dnf = function None -> "False" | Some [] -> "True" | Some dnf ->
		let lset_to_string lset = LSet.fold (fun (p,x) s -> 
				s^(if p then "" else "~")^Lit.to_string x^";"
			) lset ""
		in
		"("^(String.concat ") V (" (List.map lset_to_string dnf))^")"
	;;

	let dnf_disj dnf1 dnf2 = match dnf1,dnf2 with
		  dnf,None | None,dnf -> dnf
		| Some [],_ | _,Some [] -> Some []
		| Some lsets1, Some lsets2 -> Some (dnf_simplify (lsets1@lsets2))
	;;


	let lset_is_tautology lset =
		let tautology (harm, x) =
			LSet.mem (not harm, x) lset ||
				LSet.exists (fun (harm', x') -> Lit.tautology (harm,x) (harm',x')) lset
		in
		LSet.exists tautology lset
	;;

	let dnf_conj dnf1 dnf2 = match dnf1,dnf2 with
		  _,None | None,_ -> None
		| dnf,Some [] | Some [], dnf -> dnf
		| Some lsets1, Some lsets2 ->
			let cross1 dnf lset =
				let cross2 dnf lset' =
					let lset = LSet.union lset lset'
					in
					let dnf' = if lset_is_tautology lset then None else Some [lset]
					in
					dnf_disj dnf dnf'
				in
				List.fold_left cross2 dnf lsets2
			in
			List.fold_left cross1 None lsets1
	;;

end
;;

