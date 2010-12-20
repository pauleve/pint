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

let string_of_float0 value =
	let s = string_of_float value
	in
	s ^ if s.[String.length s - 1] = '.' then "0" else ""
;;
let string_of_list string_of_element l =
	"["^(String.concat "; " (List.map string_of_element l))^"]"
;;

let list_remove v = List.filter (fun x -> x <> v);;

let index_of v = 
	let rec index_of n = function
		  [] -> raise Not_found
		| h::q -> if h = v then n else index_of (n+1) q
	in
	index_of 0
;;

let rec rrange a b = if a <= b then b::rrange a (b-1) else [];;
let rec range a b = if a <= b then a::range (a+1) b else [];;

let rec string_apply substring data = function [] -> data
	| h::t ->
		let data = Str.replace_first (Str.regexp_string substring) h data
		in
		string_apply substring data t
;;

let prepend_all s = List.map (fun e -> s::e);;
let rec cross l = function [] -> [] | h::q -> (prepend_all h l) @ (cross l q);;

let cross_list (x:'a list list) : 'a list list = List.fold_left cross [[]] x;;

exception Empty;;
let cross_forward (handler, merger, stopper) selectors =
	let rec cross_forward choice = function
	  [] -> handler choice
	| select::rest ->
		let rec foreach = function
			  [] -> raise Empty
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

