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

open Ph_types;;

let ph_actions (ps,hits) =
	let folder bj ((ai,p),j') actions =
		Hit (ai,bj,j')::actions
	in
	Hashtbl.fold folder hits []
;;
let ph_count_actions (ps,hits) =
	Hashtbl.length hits
;;

let ph_index index actions =
	let hactions = Hashtbl.create 0
	in
	let register action = 
		Hashtbl.add hactions (index action) action
	in
	List.iter register actions;
	hactions
;;
		

let subph (ps,hits) sigma' =
	List.filter (fun (a,la) -> List.mem a sigma') ps,
	Hashtbl.copy hits
;;

let ph_from_actions actions =
	let hits = Hashtbl.create 0
	in
	let update_ps ps (a,i) =
		try
			let i' = List.assoc a ps
			in
			if i > i' then (a,i)::List.remove_assoc a ps else ps
		with Not_found -> (a,i)::ps
	in
	let folder ps = function Hit(ai,bj,k) ->
		Hashtbl.add hits bj ((ai,None),k);
		update_ps (update_ps (update_ps ps ai) bj) (fst bj,k)
	in
	let ps = List.fold_left folder [] actions
	in
	ps,hits
;;


