(*
Copyright or © or Copr. Loïc Paulevé (2015)

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

open Facile
open Easy

open PintTypes
open AutomataNetwork

let fixpoints forcelss an =
	let v_arrays = Hashtbl.create (Hashtbl.length an.automata)
	in
	let register_automata a lsdefs goal =
		let vs = Array.init (List.length lsdefs) (fun _ -> Fd.create Domain.boolean)
		in
		Hashtbl.add v_arrays a vs;

		(* Constraint: exactly one state per automata *)
		Cstr.post (Arith.sum_fd vs =~ i2e 1);

		goal &&~ Goals.Array.labeling vs
	in
	let goal = Hashtbl.fold register_automata an.automata Goals.success
	in
	let lsvar (a,i) =
		let vs = Hashtbl.find v_arrays a
		in
		vs.(i)
	in
	let e ai =
		fd2e (lsvar ai)
	in

	let register_restrict ai =
		Cstr.post (e ai =~ i2e 1)
	in
	List.iter register_restrict forcelss;

	let register_sync_tr (trs,cond) =
		let precond = SMap.bindings cond
				@ (List.map (fun (a,i,_) -> (a,i)) trs)
		in
		let fold sume ai = sume +~ e ai
		in
		let sume = List.fold_left fold
				(e (List.hd precond)) (List.tl precond)
		in
		(* Constraint: tr is not applicable *)
		Cstr.post (sume <~ i2e (SMap.cardinal cond + List.length trs))
	in
	let register_tr tr cond =
		register_sync_tr ([tr],cond)
	in
	Hashtbl.iter register_tr an.conditions;
	List.iter register_sync_tr an.sync_transitions;

	let sols = ref []
	in
	let register_sol () =
		let fold_vs a vs ps =
			let i = List.find (fun i -> Fd.int_value vs.(i) == 1)
				(Util.range 0 (Array.length vs))
			in
			LSSet.add (a,i) ps
		in
		let lss = Hashtbl.fold fold_vs v_arrays LSSet.empty
		in
		(*prerr_endline (string_of_procs ps);*)
		sols := lss::!sols
	in
	ignore(Goals.solve ((goal &&~ Goals.atomic register_sol &&~ Goals.fail) ||~ Goals.success));
	(*prerr_newline ();*)
	!sols

let fixpoints ?restrict:(lss=[]) args =
	try fixpoints lss args with Stak.Fail _ -> []

