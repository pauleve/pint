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

type bs_cache = {
	_BS : (objective, action list list) Hashtbl.t;
	aBS : (objective, (PSet.t * ISet.t) list) Hashtbl.t;
	tBS : (objective * PSet.t, process * PSet.t) Hashtbl.t;
}

let new_bs_cache () =
	{
		_BS = Hashtbl.create 50;
		aBS = Hashtbl.create 50;
		tBS = Hashtbl.create 50;
	}
;;

let actions_target_sort (ps,hits) a =
	let l = List.assoc a ps
	in
	let register map i =
		let actions = Hashtbl.find_all hits (a,i)
		in
		IMap.add i actions map
	in
	List.fold_left register IMap.empty (Util.range 0 l)
;;

let __compute_bounce_sequence push_path push_action longer_path init init_path ph obj =
	let a, i, __to_reach = obj
	in
	let sort_actions = actions_target_sort ph a
	in
	let rec walk history results i visited =
		if i = __to_reach then
			(* we reached our objective, remove larger results *)
			push_path results history
		else
			let visited = ISet.add i visited
			and actions = IMap.find i sort_actions
			in
			let folder results = function (bj,_), k ->
				if ISet.mem k visited then (* ignore loops *)
					results
				else 
					let history = push_action history (Hit (bj,(a,i),k))
					in
					(* ensure we are the shortest known path *)
					if longer_path results history then
						results
					else
						walk history results k visited
			in
			List.fold_left folder results actions
	in
	walk init_path init i ISet.empty
;;

(** BS **)

let string_of_BS _BS_obj = "[ "^(String.concat "; " 
		(List.map string_of_actions _BS_obj))^" ]"
;;
let compute_BS ph obj =
	let push_path results ps = ps::results
	and push_action ps action = ps@[action]
	and longer_path results ps = false
	in
	dbg_noendl ("- computing BS("^string_of_obj obj^")...");
	let _BS_obj = __compute_bounce_sequence push_path push_action longer_path [] [] ph obj
	in
	(if !dodebug then dbg (" "^string_of_BS _BS_obj));
	_BS_obj
;;
let get_BS ph cache obj =
	try Hashtbl.find cache._BS obj
	with Not_found -> (
		let r = compute_BS ph obj
		in
		Hashtbl.add cache._BS obj r;
		r
	)
;;

let lasthitters cache ph ?filter:(filter = fun _ -> true) obj =
	let fold_actions ps = function [] -> PSet.empty
		| actions -> 
			PSet.add (hitter (List.nth actions (List.length actions - 1))) ps
	in
	let bs = List.filter filter (get_BS ph cache obj)
	in
	List.fold_left fold_actions PSet.empty bs
;;

let targets cache ph obj =
	let fold_actions ps actions =
		let fold_action ps action =
			PSet.add (target action) ps
		in
		List.fold_left fold_action ps actions
	in
	let bs = get_BS ph cache obj
	in
	List.fold_left fold_actions PSet.empty bs




(** BS^ **)

let string_of_aBS aBS_obj = "[ "^(String.concat "; " 
		(List.map (fun (ps,_) -> string_of_procs ps) aBS_obj))^" ]"
;;
let compute_aBS ph obj =
	let g = obj_bounce obj
	in
	let push_path results psi =
		let ps = fst psi in
		psi::List.filter (fun (ps',_) -> not(PSet.subset ps ps')) results
	and push_action (ps,interm)  = function Hit ((a,i),_,k) ->
		(* register hitter iff has different sort *)
		(if a <> obj_sort obj then (PSet.add (a,i) ps) else ps),
		(if g <> k then ISet.add k interm else interm)
	and longer_path results psi =
		let ps = fst psi in
		List.exists (fun (ps',_) -> PSet.subset ps' ps) results
	in
	dbg_noendl ("- computing aBS("^string_of_obj obj^")...");
	let aBS_obj = __compute_bounce_sequence push_path push_action longer_path [] 
					(PSet.empty, ISet.empty) ph obj
	in
	(if !dodebug then dbg (" "^string_of_aBS aBS_obj));
	aBS_obj
;;
let get_aBS ph cache obj =
	try Hashtbl.find cache.aBS obj
	with Not_found -> (
		let r = compute_aBS ph obj
		in
		Hashtbl.add cache.aBS obj r;
		r
	)
;;

(** targets_ai **)

(* returns the targets that ai may hits when resolving obj *)
let tBS ph cache obj restr_procs ai =
	let full_tBS = Hashtbl.find_all cache.tBS (obj,restr_procs)
	in
	try
		List.assoc ai full_tBS
	with Not_found -> (
		dbg "tBS: exact (expensive) computation.";
		let a = obj_sort obj
		in
		let fold_actions ps bs =
			if List.exists (fun h -> let (b,i) = hitter h
					in
					b <> a && not (PSet.mem (b,i) restr_procs)) bs then
				ps
			else 
			let fold_action ps = function Hit (h,t,_) ->
				if h = ai then PSet.add t ps else ps
			in
			List.fold_left fold_action ps bs
		in
		let _BS = get_BS ph cache obj
		in
		let ps = List.fold_left fold_actions PSet.empty _BS
		in
		Hashtbl.add cache.tBS (obj,restr_procs) (ai,ps);
		ps
	)
;;

