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

type t_trace = Trace of (Spig.t_state list)
			| Stable of (Spig.t_state list * Spig.t_state list)

type t_prop = float
;;

let extract_tracename = List.map (fun (name, dest) -> name);;

let transition_exists (name, dest) = Constraint.NotNull name;;
let transition_never (name, dest) = Constraint.Null name;;

let string_of_trace string_of_state trace = 
	let string_of_states states = "["^(String.concat "];["
		(List.map string_of_state states))^"]"
	in
	match trace with
		  Trace states -> "Trace("^(string_of_states states)^")"
		| Stable (states, cycle) -> "Stable("^(string_of_states states) ^
			","^(string_of_states cycle)^")"
;;

let rec _always spig = function [] | [_] -> []
	| s1::s2::q ->
		let rset = List.filter (fun (tr,d) -> d <> s2) (Spig.next spig s1)
		in
		let cs = List.map transition_never rset
		in
		cs @ (_always spig (s2::q))
;;

let rec exists spig = function
	  Trace([]) | Trace([_]) -> []
	| Trace(s1::s2::q) -> 
		let rset = List.filter (fun (tr, d) -> d = s2) (Spig.next spig s1)
		in
		if s1 = s2 && List.length rset = 0 then exists spig (Trace (s2::q))
		else (
		assert (List.length rset = 1);
		let cs = transition_exists (List.hd rset)
		in
		cs::(exists spig (Trace (s2::q)))
		)

	| Stable(trace, []) -> exists spig (Trace trace)
	| Stable(trace, h::q) ->
		let cycle = h::q @ [h]
		in
		(exists spig (Trace (trace@cycle))) @ (_always spig cycle)
;;

let rec proportion spig p = function
	  Stable (trace, h::cycle) -> 
	  	(proportion spig p (Trace (trace@[h]))) @ 
			(exists spig (Stable ([], h::cycle)))
	| Stable (trace, []) -> proportion spig p (Trace trace)
	| Trace [] | Trace [_] -> invalid_arg "trace must have at least 2 states!"
	| Trace trace ->
		let rec filter_trace pred = function [] | [_] -> []
			| s1::s2::q ->
				let trs = Spig.next spig s1
				in
				(if List.length trs > 1 then [List.filter (pred s2) trs] else [])
				@ (filter_trace pred (s2::q))
		in

		(* deviant *)
		let deviant = Constraint.ProdSum (List.map extract_tracename 
						(filter_trace (fun s (tr,d) -> d <> s) trace))

		(* following *)
		and following_l = List.map extract_tracename
						(filter_trace (fun s (tr,d) -> d = s) trace)
		in
		let following = List.flatten following_l
		in
		assert ((List.length following) = (List.length following_l));
		let following = Constraint.Prod following

		(* factor *)
		and factor = p /. (1. -. p)

		in
		(Constraint.FactorEqual (following, factor, deviant))
		::exists spig (Trace trace)
;;

let never_transition_matching_from_state spig pred state =
	let trs = Spig.next spig state
	in
	let trs = List.filter (fun (tr, dest) -> pred state dest) trs
	in
	List.map transition_never trs
;;

