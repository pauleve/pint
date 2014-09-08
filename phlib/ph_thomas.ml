
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

let ph_remove_actions (ps, hits) rm_actions =
	failwith "Not implememented yet"

let constrain_ph ph a res values =
	let ig = !Ph_instance.interaction_graph
	in
	let ctx = InteractionGraph.IG.ctx_for_resources ig (fst ph) a res
	and regs = InteractionGraph.IG.regulators ig a
	in
	let actions = active_actions ph ctx a
	in
	let rm_actions = List.filter (function Hit (_, _, i) ->
									not (List.mem i values)) actions
	in
	if rm_actions <> [] then
		let ph = ph_remove_actions ph rm_actions
		in
		if SSet.cardinal regs = 1 then ph else
		(* group actions per hitter *)
		let group_actions g = function Hit (bk, aj, i) ->
			let actions = try PMap.find bk g with Not_found -> []
			in
			PMap.add bk (Hit (bk, aj, i)::actions) g
		in
		let g_rm_actions = List.fold_left group_actions PMap.empty rm_actions
		in
		(* build cooperative hit (hitter ^ not (res)) -> a i j
			ensure that there is one solution *)
		let fold_rm_actions bk actions ph =
			failwith "Not implemented yet"
		in
		PMap.fold fold_rm_actions g_rm_actions ph
	else ph


