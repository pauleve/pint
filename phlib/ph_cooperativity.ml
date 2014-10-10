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

let reflection_name sigma = String.concat "" sigma

let build_reflection ?coop_label:(coop_label=None) ?rsa:(rsa=Instantaneous) get_sort_max = function
  [a] -> (a, ([a], (function [s] -> s | _ -> invalid_arg "idx_from_state singleton"))), None
| sigma -> 
	let sigma_n = reflection_name sigma ^ (match coop_label with None -> "" | Some l -> "__"^l)
	in
	if SMap.mem sigma_n !Ph_instance.cooperativities then
		((sigma_n, SMap.find sigma_n !Ph_instance.cooperativities), None)
	else (
		let sigma_len = List.length sigma
		in
		let rec build_idx_sizes n prec_size =
			let my_size = if n = sigma_len-1 then 1 
				else ((1+get_sort_max (List.nth sigma (n+1))) * prec_size)
			and n' = n-1
			in
			(if n' < -1 then [] else build_idx_sizes (n-1) my_size)
			@ [my_size]
		in
		let idx_sizes = build_idx_sizes (sigma_len-1) 1
		in
		let lsigma = List.hd idx_sizes - 1
		and idx_sizes = List.tl idx_sizes
		in
		let sigma_p = (sigma_n, lsigma)
		in
		let idx_from_state state =
			let rec idx_from_state n = function
				  [] -> 0
				| i::tail -> i*(List.nth idx_sizes n) + idx_from_state (n+1) tail
			in
			idx_from_state 0 state
		in

		let get_sort_processes a = Util.range 0 (get_sort_max a)
		in
		let _S = Util.cross_list (List.map get_sort_processes (List.rev sigma))
		in

		let folder hsigma z =
			let n = Util.index_of z sigma
			in
			let folder hsigma i =
				let my_S = List.filter (fun state -> List.nth state n <> i) _S
				in
				let make_hit state =
					let shift = (i - List.nth state n)*(List.nth idx_sizes n)
					and state_id = idx_from_state state
					in
					let state'_id = state_id + shift
					in
					Hit ((z,i), (sigma_n,state_id), state'_id), rsa
				in
				hsigma @ List.map make_hit my_S
			in
			List.fold_left folder hsigma (Util.range 0 (get_sort_max z))
		in
		let hsigma = List.fold_left folder [] sigma
		in
		let append = Some (sigma_p, hsigma)
		and record = sigma_n, (sigma, idx_from_state)
		in
		Ph_instance.cooperativities := SMap.add (fst record) (snd record)
											!Ph_instance.cooperativities;
		(record, append)
	)

let new_coop () =
	let n = SMap.cardinal !Ph_instance.cooperativities
	in
	"__coop" ^ (string_of_int n)

let separate_levels sigma_n n idx_from_state top =
	let idx_top = List.map idx_from_state top
	in
	let idx_bot = List.filter (fun i -> not (List.mem i idx_top)) (Util.range 0 n)
	in
	(idx_top, idx_bot)

let build_cooperation ?coop_label:(coop_label=None) ?rsa:(rsa=Instantaneous) get_sort_max sigma =
	let rec cooperative_matching patch op =
		match op with
		  SM (sigma, top) -> 
		  	let (sigma_n, (sigma, idx_from_state)), append = 
					build_reflection ~coop_label ~rsa get_sort_max sigma
			in
			let n = match append with None -> 
									(try List.assoc sigma_n (fst patch) with
										Not_found -> get_sort_max sigma_n)
							| Some ((_,n),_) -> n
			in
			let (bot, top) = separate_levels sigma_n n idx_from_state top
			in
			sigma_n, (bot, top), (match append with None -> patch
									| Some (ai,actions) ->
										(ai::fst patch,actions@snd patch))
		| SM_Not sm ->
			let sigma_n, (top,bot), patch = cooperative_matching patch sm
			in
			sigma_n, (bot, top), patch
		| SM_And (sm1, sm2) 
		| SM_Or (sm1, sm2) ->
			let sig1, (top1,bot1), patch = cooperative_matching patch sm1
			in
			let sig2, (top2,bot2), patch = cooperative_matching patch sm2
			in
			let sigma_n = new_coop ()
				^ (match coop_label with None -> "" | Some l -> "__"^l)
			in
			let dirs = [
				(sig1, top1, 0, 2);(sig1, top1, 1, 3);
				(sig1, bot1, 2, 0);(sig1, bot1, 3, 1);
				(sig2, top2, 0, 1);(sig2, top2, 2, 3);
				(sig2, bot2, 1, 0);(sig2, bot2, 3, 2);
			]
			in
			let register sigN j k actions s =
				(Hit ((sigN,s), (sigma_n,j), k), rsa)::actions
			in
			let folder actions (sigN, levels, j, k) =
				List.fold_left (register sigN j k) actions levels
			in
			let patch = (sigma_n, 3)::fst patch, 
							List.fold_left folder (snd patch) dirs
			in
			Ph_instance.cooperativities := SMap.add sigma_n ([sig1;sig2], 
				function [s1;s2] ->
					let r1 = if List.mem s1 top1 then 2 else 0
					and r2 = if List.mem s2 top2 then 1 else 0
					in
					r1 + r2
				| _ -> invalid_arg "__coop idx_from_state")
					!Ph_instance.cooperativities;
			let (top, bot) = match op with
				  SM_And _ -> ([3], [0;1;2])
				| SM_Or _ -> ([1;2;3], [0])
				| _ -> invalid_arg "match op"
			in
			sigma_n, (top, bot), patch
	in
	cooperative_matching ([],[]) sigma

