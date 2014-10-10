
open PintTypes
open Ph_types

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


let group_actions getkey =
	let group g a =
		let key = getkey a
		in
		let actions = try PMap.find key g with Not_found -> []
		in
		PMap.add key (a::actions) g
	in
	List.fold_left group PMap.empty

let ph_filter_actions (ps, hits) ai filter =
	let all = Hashtbl.find_all hits ai
	in
	List.iter (fun _ -> Hashtbl.remove hits ai) all;
	let actions = List.filter filter all
	in
	List.iter (fun v -> Hashtbl.add hits ai v) actions

let ph_remove_actions (ps, hits) rm_actions =
	let gactions = group_actions (function Hit (_, ai, _) -> ai) rm_actions
	in
	let handle ai rm_actions =
		let filter ((bk,_),j) = not (List.mem (Hit (bk, ai, j)) rm_actions)
		in
		ph_filter_actions (ps, hits) ai filter
	in
	PMap.iter handle gactions

let ph_compl_ctx (ps, hits) ctx =
	let fold a is ctx =
		let js = iset_of_list (Util.range 0 (List.assoc a ps))
		in
		let js = ISet.diff js is
		in
		SMap.add a js ctx
	in
	SMap.fold fold ctx ctx_empty

let ph_cooperation (ps, hits) sm =
	let sigma, (top, bot), patch = 
		Ph_cooperativity.build_cooperation (fun a -> List.assoc a ps) sm
	in
	let ps = fst patch @ ps
	in
	List.iter (function (Hit (bk, ai, j), rsa) ->
							Hashtbl.add hits ai ((bk,rsa),j)) (snd patch);
	(ps, hits), (sigma, top)


let constrained_ph ph a res values =
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
	if rm_actions = [] then ph else (
		ph_remove_actions ph rm_actions;
		if SSet.cardinal regs = 1 then ph else
		let g_rm_actions = group_actions (function Hit (bk, _, _) -> bk) rm_actions
		in
		(* build cooperative hit (hitter ^ not (res)) -> a i j
			ensure that there is one solution *)
		let fold_rm_actions (b,k) actions ph =
			let rb = Ph_cooperativity.regulators !Ph_instance.cooperativities b
			in
			let ctx = ph_compl_ctx ph ctx
			in
			let fold_ctx d is ph =
				if SSet.mem d rb then ph else
				let sm = 
					if compare b d <= 0 then
					SM ([b;d], Util.cross_list [ISet.elements is;[k]])
					else
					SM ([d;b], Util.cross_list [[k];ISet.elements is])
				in
				let ph, (c, top) = ph_cooperation ph sm
				in
				(* 2. add hits *)
				List.iter (fun k -> List.iter (function Hit (_, ai, j) ->
									Hashtbl.add (snd ph) ai (((c,k),Instantaneous),j)) actions) top;
				ph
			in
			SMap.fold fold_ctx ctx ph
		in
		PMap.fold fold_rm_actions g_rm_actions ph)


