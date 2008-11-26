
open Num;;

type var = string * int;;

module VarSet = Set.Make(struct type t = var 
									let compare = Pervasives.compare end);;

let rec simplify = function [] -> []
	| (frac,vars)::q -> 
		let q, r = Util.list_separate (fun (frac2,vars2) -> VarSet.equal vars vars2) q
		in
		let frac = List.fold_left add_num frac (List.map fst r)
		in
		(if frac =/ (Num.Int 0) then [] else [frac,vars])
		::simplify q
;;

let make elements fraction =
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
;;

