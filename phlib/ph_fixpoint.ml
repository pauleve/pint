(*
Copyright or © or Copr. Loïc Paulevé (2012)

loic.pauleve@irccyn.ec-nantes.fr

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

open Facile;;
open Easy;;

open Ph_types;;

let _fixpoints pl (sorts_def, hits) =
	let v_arrays = Hashtbl.create (List.length sorts_def)
	in
	let register_sort goal (a,l) =
		let vs = Array.init (l+1) (fun _ -> Fd.create Domain.boolean)
		in
		Hashtbl.add v_arrays a vs;

		(* Constraint: exactly one node per sort *)
		Cstr.post (Arith.sum_fd vs =~ i2e 1);

		goal &&~ Goals.Array.labeling vs
	in
	let goal = List.fold_left register_sort Goals.success sorts_def
	in
	let var_of_proc (a,i) =
		let vs = Hashtbl.find v_arrays a
		in
		vs.(i)
	in
	let register_restrict ai =
		Cstr.post (fd2e (var_of_proc ai) =~ i2e 1)
	in
	List.iter register_restrict pl;

	let register_hit (hitter, target) =
		let v1, v2 = var_of_proc hitter, var_of_proc target
		in
		(* Constraint: at most one node per hit *)
		Cstr.post (fd2e v1 +~ fd2e v2 <=~ i2e 1)
	in
	List.iter register_hit hits;

	let sols = ref []
	in
	let register_sol () =
		let fold_vs a vs ps =
			let i = List.find (fun i -> Fd.int_value vs.(i) == 1)
				(Util.range 0 (Array.length vs))
			in
			PSet.add (a,i) ps
		in
		let ps = Hashtbl.fold fold_vs v_arrays PSet.empty
		in
		(*prerr_endline (string_of_procs ps);*)
		sols := ps::!sols
	in
	ignore(Goals.solve ((goal &&~ Goals.atomic register_sol &&~ Goals.fail) ||~ Goals.success));
	(*prerr_newline ();*)
	!sols
;;
let fixpoints ?restrict:(pl=[]) args =
	try _fixpoints pl args with Stak.Fail _ -> []
;;
	

