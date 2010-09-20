(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2010)

loic.pauleve@irccyn.ec-nantes.fr
morgan.magnin@irccyn.ec-nantes.fr
olivier.roux@irccyn.ec-nantes.fr

This software is a computer program whose purpose is to provide Process
Hitting related tools.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the
CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author, the holder of the
economic rights, and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.
*)

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
			let r, q = List.partition (fun (frac2,vars2) -> 
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

