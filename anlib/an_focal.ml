
open PintTypes
open AutomataNetwork

let restrict_conditions ctx conds =
	let extend a is cond =
		try if ISet.mem (SMap.find a cond) is then [cond] else []
		with Not_found ->
		List.map (fun i -> SMap.add a i cond) (ISet.elements is)
	in
	let extend_conds a is rconds = rconds @
		List.flatten (List.map (extend a is) conds)
	in
	SMap.fold extend_conds ctx []

let restrict_focal an a ctx values =
	let bad_trconds i trconds =
		let active_transitions j trconds =
			let conds = Hashtbl.find_all an.conditions (a,i,j)
			in
			let pconds = List.partition (condition_matches ctx) conds
			in
			match fst pconds with [] -> trconds | _ ->
				((a,i,j),pconds)::trconds
		in
		let js = try Hashtbl.find an.transitions (a,i)
					with Not_found -> ISet.empty
		in
		let js = ISet.diff js values
		in
		ISet.fold active_transitions js trconds
	in
	let is = try SMap.find a ctx with Not_found -> automaton_sdomain an a
	in
	let bad_trconds = ISet.fold bad_trconds is []
	and good_ctx = an_compl_ctx an ctx
	in
	let fix_trconds (tr, (bad_conds, conds)) =
		let good_conds = restrict_conditions good_ctx bad_conds
		in
		let conds = good_conds @ conds
		in
		an_replace_trconditions an tr conds
	in
	List.iter fix_trconds bad_trconds

