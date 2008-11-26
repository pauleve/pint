
open Num;;

type var = string * int;;

module type VARSETUP =
	sig
		type t
		val compare : t -> t -> int
		val to_string : t -> string
	end
;;

module Make ( Setup : VARSETUP ) =
struct
	module VarSet = Set.Make 
		(struct 
			type t = Setup.t * int
			let compare (v1,p1) (v2,p2) =
				let vcmp = Setup.compare v1 v2
				in
				if vcmp = 0 then compare p1 p2 else vcmp
		end)

	let rec simplify = function [] -> []
		| (frac,vars)::q -> 
			let r, q = Util.list_separate (fun (frac2,vars2) -> 
							VarSet.equal vars vars2) q
			in
			let frac = List.fold_left add_num frac (List.map fst r)
			in
			(if frac =/ (Num.Int 0) then [] else [frac,vars])
			@ simplify q

	let init elements fraction =
		let varset_from_list =
			let folder set elt =
				VarSet.add elt set
			in
			List.fold_left folder VarSet.empty
		in 
		let num, den = List.split elements
		in
		let num, den = Util.cross_list num, Util.cross_list den
		in
		simplify
		((List.map (fun f -> Int 1, 
						varset_from_list (Util.count_elements f)) num) @
		List.map (fun f -> minus_num fraction,
						varset_from_list (Util.count_elements f)) den)

	let to_string_generic string_of_var =
		let string_of_vars vars =
			let string_of_varp (var,pow) =
				(string_of_var var)
				^if pow <> 1 then "^"^(string_of_int pow)^"" else ""
			in
			let folder var buf =
				buf^(if buf <> "" then "*" else "")^
				string_of_varp var
			in
			VarSet.fold folder vars ""
		in
		let string_of_term (frac, vars) =
			(if frac =/ Int 1 then "" else (string_of_num frac)^"*") ^
			string_of_vars vars
		in
		let rec string_of_terms = function [] -> "0 = 0"
			| [term] -> (string_of_term term) ^ " = 0"
			| term::q -> (string_of_term term) ^ " + " ^ string_of_terms q
		in
		string_of_terms

	let to_string = to_string_generic Setup.to_string
	let to_mapped_string map = to_string_generic (fun d -> List.assoc d map)

end
;;

