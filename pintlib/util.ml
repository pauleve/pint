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

open PintTypes

let string_of_float0 value =
	let s = string_of_float value
	in
	s ^ if s.[String.length s - 1] = '.' then "0" else ""
;;
let string_of_list string_of_element l =
	"["^(String.concat "; " (List.map string_of_element l))^"]"
;;

let opt_default default = function Some x -> x | None -> default;;

let dump_to_file filename str = 
	let cout = open_out filename
	in
	output_string cout str;
	close_out cout
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

let rec srange a b =
	if a <= b then ISet.add a (srange (a+1) b) else ISet.empty

let hashtbl_filter_bindings h key filter =
	let values = Hashtbl.find_all h key
	in
	let rec del = function 0 -> () | n -> (Hashtbl.remove h key; del (n-1))
	in
	del (List.length (values));

	let values = List.filter filter values
	in
	List.iter (Hashtbl.add h key) values
;;

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


let rec list_lassoc b = function [] -> raise Not_found
	| (a,c)::t -> if b = c then a else list_lassoc b t
;;

let stream_permutations lst =
    let lstar = Array.of_list lst in
    let len = Array.length lstar in
    let ks = range 1 len in
	let indices = List.fold_left (fun is i -> ISet.add i is) 
					ISet.empty (range 0 (len-1))
	in
	let ith iset n =
		let track i (m, j) =
			m + 1, 
			if n = m then i else j
		in
		snd (ISet.fold track iset (0, ISet.choose iset))
	in
    let choose k (v, indices, res) =
        let ix = ith indices (v mod k)
		in
        (v / k, ISet.remove ix indices, lstar.(ix)::res)
    in
    let perm i =
        let (v, _, res) =
            List.fold_right choose ks (i, indices, [])
        in
        if v > 0 then None else Some res
    in
    Stream.from perm

let smap_remove_keys smap keys =
	SSet.fold (fun a smap -> SMap.remove a smap) keys smap

let smap_subset equal m1 m2 =
	SMap.for_all
		(fun k v -> try equal v (SMap.find k m2) with Not_found -> false)
		m1

