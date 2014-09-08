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

open PintTypes;;
open Ph_types;;

let resolve register ctx =
	let rec resolve (sorts, f) =
		let values_list = List.map resolve_sort sorts
		in
		let values_list = Util.cross_list (List.rev values_list)
		in
		List.map f values_list
	and resolve_sort a =
		try
			let cfg = SMap.find a register
			in
			resolve cfg
		with Not_found ->
			ISet.elements (ctx_get a ctx)
	in
	resolve_sort
;;

let local_fixed_points ?level1:(level1=false) register (ps, hits) =
	let state_empty = SMap.empty
	and state_add_proc (a,i) = SMap.add a i
	and state_inter =
		let inter a i s =
			try assert (SMap.find a s = i); s with Not_found -> SMap.add a i s
		in
		SMap.fold inter
	in
	let rec min_conds level1 (a,i) = 
		if level1 || SMap.mem a register then (
			let register_hit ctx ((hitter,_),_) = ctx_add_proc hitter ctx
			in
			let ai_hits = Hashtbl.find_all hits (a,i)
			in
			let unstable_ctx = List.fold_left register_hit ctx_empty ai_hits
			in
			let complement_states b is states =
				let lb = List.assoc b ps
				in
				(* levels of b that do not hit ai *)
				let js = List.filter (fun j -> not (ISet.mem j is)) (Util.range 0 lb)
				in
				let register_cond conds j =
					let bj_conds = List.map (state_add_proc (b,j)) (min_conds false (b,j))
					in
					let merge cond conds bj_cond =
						try
							state_inter bj_cond cond::conds
						with Assert_failure _ ->
							conds
					in
					let cross_conds conds cond = List.fold_left (merge cond) conds bj_conds
					in
					List.fold_left cross_conds [] conds
				in
				List.flatten (List.map (register_cond states) js)
			in
			SMap.fold complement_states unstable_ctx [state_empty]
		) else
			[state_empty]
	in
	min_conds level1
;;

