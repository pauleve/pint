
open PintTypes;;
open Ph_types;;


let active_actions ph ctx a =
	let a_is = try ctx_get a ctx with Not_found -> 
					iset_of_list (Util.range 0 (List.assoc a (fst ph)))
	in
	(* fetch all actions on a *)
	let fetch i actions =
		let hs = Hashtbl.find_all (snd ph) (a,i)
		in
		List.map (fun ((bk,_),j) -> Hit (bk,(a,i),j)) hs
			@ actions
	in
	let actions = ISet.fold fetch a_is []
	in
	let sorts = ctx_sorts ctx
	in
	(* fetch context for cooperative sorts *)
	let register_sort (sorts, ctx) = function Hit ((b,_),_,_) ->
		if SSet.mem b sorts then (sorts, ctx)
		else 
			let b_is = Ph_cooperativity.resolve !Ph_instance.cooperativities ctx b
			in
			let ctx = SMap.add b (iset_of_list b_is) ctx
			in
			(SSet.add b sorts, ctx)
	in
	let ctx = snd (List.fold_left register_sort (sorts, ctx) actions)
	in
	(* filter actions in the context *)
	List.filter (function Hit (bk, _, _) -> ctx_has_proc bk ctx) actions


let constrain_ph ph a res values =
	let ig = !Ph_instance.interaction_graph
	in
	let ctx = InteractionGraph.IG.ctx_for_resources ig (fst ph) a res
	in
	let actions = active_actions ph ctx a
	in
	let rm_actions = List.filter (function Hit (_, _, i) ->
									not (List.mem i values)) actions
	in
	if rm_actions <> [] then
		(* build cooperative sort for res if not already existing *)
		(* follow rules for removing each action *)
		failwith "Not implemented yet"
	else ph


