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

type 'a state = 'a list

type 'a subst = 'a * 'a
;;

let apply_subst state (a, a') = List.map (fun b -> if b = a then a' else b) state
;;

let extract_subst s s' =
	let all_a = Util.list_sub s s'
	and all_a' = Util.list_sub s' s
	in assert (List.length all_a = 1); assert (List.length all_a' = 1);
	List.hd all_a, List.hd all_a'
;;

let rec string_of_substs string_of_state state =  function
	  [] -> "("^(string_of_state state)^")"
	| subst::q -> "("^(string_of_state state)^") -> "^
					string_of_substs string_of_state (apply_subst state subst) q
;;

let contains_subst graph state subst =
	let next = apply_subst state subst
	in
	List.mem next (Graph.next_vertices graph state)
;;
let contains_path graph state substs =
	let folder state subst =
		if contains_subst graph state subst then
			apply_subst state subst
		else
			raise Not_found
	in
	try 
		ignore(List.fold_left folder state substs);
		true
	with Not_found -> false
;;

let count graph =
	let sum acc state =
		acc + List.length (Graph.next_vertices graph state)
	in
	List.fold_left sum 0 (Graph.vertices graph)
;;

let project dyn pred =
	let project_state state = List.filter pred state
	in
	let dyn' = Graph.create 0
	in
	let register_transition s (l, s') =
		Graph.add dyn' (project_state s) (l, project_state s')
	in
	Graph.iter register_transition dyn;
	dyn'
;;

let extract_strict dyn states =
	let dyn' = Graph.create (List.length states)
	in
	let register_transition s (l, s') =
		if List.mem s states && List.mem s' states then
			Graph.add dyn' s (l, s')
	in
	Graph.iter register_transition dyn;
	dyn'
;;
let extract dyn states =
	let dyn' = Graph.create (List.length states)
	in
	let register_transition s (l, s') =
		if List.mem s states then
			Graph.add dyn' s (l, s')
	in
	Graph.iter register_transition dyn;
	dyn'
;;


