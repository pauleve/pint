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

let prepend s = List.map (fun e -> s::e);;

let rec cross l = function [] -> [] | h::q -> (prepend h l) @ (cross l q);;

let cross_list (x:'a list list) : 'a list list = List.fold_left cross [[]] x;;

let list_union a b = a @ (List.filter (fun x -> not (List.mem x a)) b);;

let list_prepend_if_new a l = if List.mem a l then l else a::l;;

let list_uniq (l:'a list) =
	let rec _list_uniq u = function [] -> u
		| h::q -> if List.mem h u then _list_uniq u q 
				else _list_uniq (h::u) q
	in
	_list_uniq [] l
;;
let list_uniq2 l =
	list_uniq (List.map (List.sort Pervasives.compare) l)

let rec list_intersection a = function [] -> []
	| h::q -> (if List.mem h a then [h] else []) @ list_intersection a q
;;

let list_remove v = List.filter (fun x -> x <> v)
;;

let rec list_replace v v' = function [] -> []
	| h::q -> (if h = v then v' else h)::list_replace v v' q
;;

let rec list_sub a = function [] -> a | v::b -> list_sub (list_remove v a) b
;;

let index_of v = 
	let rec index_of n = function
		  [] -> raise Not_found
		| h::q -> if h = v then n else index_of (n+1) q
	in
	index_of 0
;;

let rec subset l = function [] -> true
	| h::q -> (List.mem h l) && subset l q
;;

let rec rrange a b = if a <= b then b::rrange a (b-1) else [];;
let rec range a b = if a <= b then a::range (a+1) b else [];;

let dump_to_file filename content = 
	let fd = open_out filename in
	output_string fd content;
	close_out fd
;;

let rec count_elements = function [] -> []
	| h::q ->
		let hs, q = List.partition (fun h' -> h = h') q
		in
		(h, 1+List.length hs)::count_elements q
;;

let rec string_apply substring data = function [] -> data
	| h::t ->
		let data = Str.replace_first (Str.regexp_string substring) h data
		in
		string_apply substring data t
;;

let string_of_float0 value =
	let s = string_of_float value
	in
	s ^ if s.[String.length s - 1] = '.' then "0" else ""
;;

exception No_choice;;
let cross_forward (handler, merger, stopper) selectors =
	let rec cross_forward choice = function
	  [] -> handler choice
	| select::rest ->
		let rec foreach = function
			  [] -> raise No_choice
			| [h] -> cross_forward (h::choice) rest
			| h::t -> 
				let ret = cross_forward (h::choice) rest
				in
				if not (stopper ret) then
					merger ret (foreach t)
				else
					ret
		in
		foreach select
	in
	cross_forward [] (List.rev selectors)
;;

