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

open Debug;;
open PintTypes;;
open Ph_types;;

(** Process Hitting **)

module P2Map = Map.Make (struct type t = process * process let compare = compare end);;

type env = (Ph_types.PSet.t * Ph_types.rate * Ph_types.PSet.t) list P2Map.t

type plotter = (float -> Ph_types.process -> unit)

let get_key ai bj = if ai < bj then (ai,bj) else (bj,ai)
;;

let init_random () =
	Random.self_init ();
	R.set_seed (Random.bits ()) (Random.bits ())
;;

(*
let string_of_reaction (j,f,j') = string_of_procs j^ " -> "^string_of_procs j;;
let string_of_reactions l = "{ "^String.concat ", " (List.map string_of_reaction l)^"}";;
*)

let create_env (ps,hits) =
	let index bj ((ai,stoch),k) idx =
		let i = get_key ai bj
		in
		let reactions = try P2Map.find i idx with Not_found -> []
		in
		let j = PSet.add bj (PSet.singleton ai)
		and j' = PSet.add (fst bj, k) (PSet.singleton ai)
		in
		let f = rsa_of_stochatime stoch
		in
		(*
		dbg (string_of_proc (fst i)^","^string_of_proc (snd i)^": adding reaction "^
			string_of_reaction (j,f,j'));
		*)
		let reactions = (j,f,j')::reactions
		in
		P2Map.add i reactions idx
	in
	Hashtbl.fold index hits P2Map.empty
;;

let reactions env ai s =
	let fold b j reactions =
		let i = get_key ai (b,j)
		in
		try P2Map.find i env@reactions
		with Not_found -> reactions
	in
	SMap.fold fold s []
;;

(** NM-NRM **)

let init l (t,s,r) =
	let init (j,f,j') =
		let t' = match f with
			  None -> t
			| Some (r,sa) ->
				let step = Param.random_firing_time r sa
				in
				(*dbg ("t' = "^string_of_float t^" + "^string_of_float step);*)
				t +. step
		in
		Hashtbl.add r (j,f,j') t'
	in
	List.iter init l;
	(t,s,r)
;;

let next (_,_,r) =
	let get_next o t = function
		  None -> Some (o,t)
		| Some (o',t') -> if t < t' then Some (o,t) else Some (o',t')
	in
	Hashtbl.fold get_next r None
;;


(** generic abstract machine **)

let species_init env (a,i) (t,s,r) = 
	let i' = try SMap.find a s with Not_found -> -1
	in
	assert (i' <> i);
	let s = SMap.add a i s
	in
	let l = reactions env (a,i) s
	in
	(*dbg ("initialising new reactions "^string_of_reactions l);*)
	init l (t,s,r)
;;

let get_affected_reactions i (t,s,r) =
	let folder (j,f,j') _ aff =
		if PSet.mem i j then (j,f,j')::aff else aff
	in
	Hashtbl.fold folder r []
;;
let species_remove species (t,s,r) =
	let l = get_affected_reactions species (t,s,r)
	in
	(*dbg ("removing reactions "^string_of_reactions l);*)
	List.iter (Hashtbl.remove r) l;
	(t,s,r)
;;

let initial env state =
	let fold a i term =
		species_init env (a,i) term
	in
	SMap.fold fold state (0., SMap.empty, Hashtbl.create 20)
;;

exception Halt;;

let reduce env term =
	match next term with
	  None -> raise Halt
	| Some ((j,f,j'), t) ->
		let target = PSet.min_elt (if PSet.cardinal j == 1 then j else PSet.diff j j')
		and bounce = PSet.min_elt (PSet.diff j' j)
		in
		let (_,s,r) = species_remove target term
		in
		let term = species_init env bounce (t,s,r)
		in
		term, (target,bounce)
;;

let execute env state duration plotter =
	let term = initial env state
	in
	let rec execute (t,s,r) =
		try
			let (t,s,r), (bj,bk) = reduce env (t,s,r)
			in
			(if t > duration then raise Halt);
			dbg_noendl (string_of_float t^"\r");
			plotter t bk;
			execute (t,s,r)
		with Halt -> s
	in
	execute term
;;

