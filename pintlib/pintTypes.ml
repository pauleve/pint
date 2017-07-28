(*
Copyright or © or Copr. Loïc Paulevé (2013)

loic.pauleve@ens-cachan.org

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

let id x = x

module SSet = Set.Make (struct type t = string let compare = compare end)
module SMap = Map.Make (struct type t = string let compare = compare end)
module ISet = Set.Make (struct type t = int let compare = (-) end)
module IMap = Map.Make (struct type t = int let compare = (-) end)

let set_of_list set0 set_add =
	List.fold_left (fun set elt -> set_add elt set) set0

let rec iset_of_list = function [] -> ISet.empty | h::t -> ISet.add h (iset_of_list t)

let string_of_set
		?lbracket:(lb="{ ") ?rbracket:(rb=" }") ?delim:(dl=", ")
		string_of_element elements_getter set =
	let content = String.concat dl
		(List.map string_of_element (elements_getter set))
	in
	lb^content^rb

let string_of_map
	?lbracket:(lb="<") ?rbracket:(rb=">") ?delim:(dl=",")
	string_of_keyvalue folder map =
	let fold k v str =
		str ^ (if str <> "" then dl else "")
		^ string_of_keyvalue (k,v)
	in
	lb^(folder fold map "")^rb

let string_of_iset = string_of_set string_of_int ISet.elements

let string_of_sset = string_of_set id SSet.elements

let map_of_bindings add empty =
    List.fold_left (fun m (k,v) -> add k v m) empty



type ternary = True | False | Inconc
let string_of_ternary = function True -> "True" | False -> "False" | Inconc -> "Inconc"

type id = int
let prev_id = ref 0
let new_id () =
    incr prev_id;
    !prev_id

type 'a reg_t = {
    elt2id: ('a, id) Hashtbl.t;
    id2elt: (id, 'a) Hashtbl.t;
}
let new_reg n = {
    elt2id = Hashtbl.create n;
    id2elt = Hashtbl.create n;
}

let register_elt reg elt =
    try
        Hashtbl.find reg.elt2id elt
    with Not_found ->
        let eltid = new_id ()
        in (
        Hashtbl.add reg.elt2id elt eltid;
        Hashtbl.add reg.id2elt eltid elt;
        eltid)

type stochatime =
	  Instantaneous
	| RateSA of (float * int)
	| FiringInterval of (float*float*float)

let json_of_ternary = function
	  True -> "true"
	| False -> "false"
	| Inconc -> "null"

let json_of_str s =
	"\""^s^"\""

let json_of_int i =
	string_of_int i

let json_of_list json_of_elt l =
	"["^(String.concat ", " (List.map json_of_elt l))^"]"

let json_of_bindings json_of_key_elt b =
	let json_of_binding (k,v) =
        let k, v = json_of_key_elt k v
        in
        k^": "^v
	in
	"{"^(String.concat ", " (List.map json_of_binding b))^"}"

