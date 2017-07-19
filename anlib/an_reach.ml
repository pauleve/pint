
open Debug

open Ph_types

open PintTypes

open AutomataNetwork
open LocalCausalityGraph

type env = {
	an : AutomataNetwork.t;
	ctx : ctx;
	goal : process list;
	sol_cache : An_localpaths.cache;
}

let init_env an ctx goal =
	let cache = An_localpaths.create_cache ()
	in {
		an = an;
		ctx = ctx;
		goal = goal;
		sol_cache = cache;
	}


let inject_goal_automaton (an, ctx) sig_goals =
    let ng = List.map List.length sig_goals
    in
    let ig = List.fold_left (+) 1 ng - List.length sig_goals
    in
	let a = "_pint_goal"
	and sigstates = List.map (fun i -> StateId i) (Util.range 0 ig)
	in
    declare_automaton an a sigstates;
    let fold_goals i0 sig_goal =
        let n = List.length sig_goal
        in
        let register_step i sig_cond =
            let iorig = if i == 0 then 0 else i0+i
            and idest = if i == n-1 then ig else i0+i+1
            in
            declare_transition an a (StateId iorig) (StateId (idest)) sig_cond
        in
        List.iteri register_step sig_goal;
        n-1
    in
    ignore (List.fold_left fold_goals 0 sig_goals);
	let ia0 = get_automaton_state_id an a (StateId 0)
	and itop = get_automaton_state_id an a (StateId ig)
	in
	(an, SMap.add a (ISet.singleton ia0) ctx), (a, itop)



let color_nodes_connected_to_trivial_sols (gA:LSSet.t #glc) =
	(** each node is associated to a couple
			(green, nm) 
		where nm is the cached value of childs *)

	let is_green nm n = try NodeMap.find n nm with Not_found -> false
	in
	let ls_is_green nm ls = is_green nm (NodeProc ls)
	in

	let init = function
		  NodeSol (obj, ps, _) -> (LSSet.is_empty ps, NodeMap.empty)
		| NodeSyncSol (obj, states, _) -> (StateSet.is_empty states, NodeMap.empty)
		| _ -> (false, NodeMap.empty)

	(* the node n with value v receives update from node n' with value v' *)
	and push n (v,nm) = 
		let new_v = match n with
		  NodeProc _ -> (* at least one child is green *)
		  	let exists_green n' g r = r || g
			in
			NodeMap.fold exists_green nm false
		| NodeSol (obj, ps, _) -> (* all childs are green *)
			LSSet.for_all (ls_is_green nm) ps
		| NodeSyncSol (obj, states, _) -> (* all children are green *)
			let state_is_green s =
				SMap.for_all (fun a i -> ls_is_green nm (a,i)) s
			in
			StateSet.for_all state_is_green states
		| NodeObj _ -> (* all NodeObj childs are green; at least one NodeSol is green *)
			let exists_sol_green = function
				  NodeSol _ | NodeSyncSol _ -> fun g r -> r || g
				| _ -> fun _ r -> r
			and all_obj_green = function
				| NodeObj _ -> fun g r -> g && r
				| _ -> fun _ r -> r
			in
			let r = NodeMap.fold exists_sol_green nm false
			in
			if r then NodeMap.fold all_obj_green nm true else false
		in
		new_v
	in

	let values = gA#rflood (=) init default_flooder_update_cache push gA#leafs
	in
	(*let dbg_value n (g,_) =
		dbg ("Green("^string_of_node n^") = "^string_of_bool g)
	in
	Hashtbl.iter dbg_value values;*)
	let folder n (coloured, _) green =
		if coloured then NodeSet.add n green else green
	in
	Hashtbl.fold folder values NodeSet.empty

let restricted_sols_factory all_sols nodes =
	let register_node n rsols = match n with
		  NodeSol (obj, ps, interm) -> 
		  	ObjMap.add obj ((ps,interm)::try ObjMap.find obj rsols 
									with Not_found -> []) rsols
		| NodeSyncSol _ ->
			failwith "restricted_sols_factory with NodeSyncSol	not implemented."
		| _ -> rsols
	in
	let rsols = NodeSet.fold register_node nodes ObjMap.empty
	in
	fun obj -> try ObjMap.find obj rsols with Not_found -> all_sols obj

let unordered_oa env sols =
	let gA = new glc oa_glc_setup env.ctx env.goal sols make_unord_unsync_sol
	in
	gA#build;
	gA#debug ();
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	List.for_all (fun ai -> NodeSet.mem (NodeProc ai) nodes) env.goal, 
	restricted_sols_factory sols nodes

let unordered_oa' env sols =
	dbg ~level:1 ". unordered over-approximation";
	let make_domain nodes =
		let register_node n rsols = match n with
			  NodeSol (obj, ps, _) ->
				ObjMap.add obj (ps::try ObjMap.find obj rsols with Not_found -> []) rsols
			| NodeSyncSol _ ->
				failwith "make_domain with NodeSyncSol	not implemented."
			| _ -> rsols
		in
		NodeSet.fold register_node nodes ObjMap.empty
	in
	let gA = new glc oa_glc_setup env.ctx env.goal sols make_unord_unsync_sol
	in
	gA#build;
	gA#debug ();
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	List.for_all (fun ai -> NodeSet.mem (NodeProc ai) nodes) env.goal, 
	make_domain nodes

let bot_trimmed_lcg env sols gA =
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	let gA' = new glc gA#setup env.ctx env.goal sols make_unord_unsync_sol
	in
	gA#iter (fun node child ->
				if NodeSet.mem node nodes && NodeSet.mem child nodes then
					gA'#add_child node child);
	gA'#set_trivial_nsols (NodeSet.inter (gA#get_trivial_nsols ()) nodes);
	gA'#set_auto_conts (gA#auto_conts);
	gA'#commit ();
	gA'


let nodes_connected_to_procs (gA: #graph) pl =
	let update_value _ _ = true
    and init n = true, NodeMap.empty
	and update_cache _ nm _ _ = nm
	and leafs = List.fold_left (fun ns ai -> NodeSet.add (NodeProc ai) ns) NodeSet.empty pl
    in
	gA#rflood ~reversed:true (=) init update_cache update_value leafs


let top_trimmed_lcg env gA =
	let values = nodes_connected_to_procs gA env.goal
	in
	let check_node n = if not (Hashtbl.mem values n) then gA#remove_node n
	in
	NodeSet.iter check_node gA#nodes



(**** Local reachability ****)

let local_reachability env =
	assert_async_an env.an;
	let cache = env.sol_cache
	in
	let sols = An_localpaths.MinUnordUnsyncSol.solutions cache env.an
	in
	let uoa, oadom = unordered_oa' env sols
	in
	if not uoa then
		False
	else
		let sols = An_localpaths.MinUnordSol.filtered_solutions cache oadom env.an
		in
		if An_reach_asp.unordered_ua env.an env.ctx env.goal sols then
			True
		else
			Inconc





(**************************************************
		CUTSETS / REQUIREMENTS
***************************************************)

(**
 * Cutsets
 *)

module PSSet = KSets.Make(struct type t = int let compare = compare end);;

let scc_dead_rel childs scc =
	let c = List.length scc
	in
	if c > 2 then (
		dbg ("|SCC|="^string_of_int c);
		let scc_idx = List.fold_left (fun s n -> NodeSet.add n s) NodeSet.empty scc
		in
		let entry_objectives n =
			match n with
			  NodeObj _ | NodeProc _ ->
			  	[] <> List.filter (fun n -> not (NodeSet.mem n scc_idx)) (childs n)
			| _ -> false
		and dead_childs n =
			let dead_node n' =
				match n' with NodeSol _ | NodeSyncSol _ -> NodeSet.mem n' scc_idx | _ -> false
			in
			List.filter dead_node (childs n)
		in
		let objs = List.filter entry_objectives scc
		in
		match objs with 
		  [n] -> (
				let cs = dead_childs n in
				match cs with [] -> None | _ -> Some (n, cs)
			)
		| _ -> None
	) else None
;;

let rec cleanup_gA_for_cutsets gA =
	dbg ("--");
	let sccs = gA#tarjan_SCCs false gA#leafs
	in
	let handle_scc todel scc =
		match scc_dead_rel gA#childs scc with
		  Some x -> x::todel
		| None -> todel
	in
	let todel = List.fold_left handle_scc [] sccs
	in
	let apply (n, dead_childs) =
		List.iter (fun c -> gA#remove_child c n) dead_childs
	in
	match todel with [] -> gA
	| _ -> (List.iter apply todel; cleanup_gA_for_cutsets gA)
;;

let indexer size =
	let proc_index = Hashtbl.create size
	and index_proc = Hashtbl.create size
	and next_index = ref 0
	in
	let get_index ai =
		try
			Hashtbl.find proc_index ai
		with Not_found -> (
			let idx = !next_index
			in
			Hashtbl.add proc_index ai idx;
			Hashtbl.add index_proc idx ai;
			next_index := idx + 1;
			idx
		)
	in
	get_index, index_proc

let cutsets (gA:#graph) max_nkp ignore_proc leafs =
	let get_proc_index, index_proc = indexer (gA#count_procs)
	in
	let psset_product a b =
		if b = PSSet.full then a else (
		let na = PSSet.cardinal a
		and nb = PSSet.cardinal b
		in
		(*prerr_string ("<"^string_of_int na^"x"^string_of_int nb);
		flush stderr;*)
		let a = if na < nb then PSSet.product max_nkp b a else PSSet.product max_nkp b a 
		in
		(*prerr_string (">");
		flush stderr;*)
		let a = PSSet.simplify max_nkp a
		in
		(*prerr_string (" ");
		flush stderr;*)
		a)
	in
	let nm_union nm =
		let c = NodeMap.cardinal nm
		in
		match c with 
			  0 -> PSSet.empty
			| 1 -> snd (NodeMap.choose nm)
			| _ -> NodeMap.fold (fun _ -> PSSet.union) nm PSSet.empty
	and nm_cross nm =
		NodeMap.fold (fun _ -> psset_product) nm PSSet.full
	in
	let total_count = ref 0
	in
	let update_value n (_,nm) =
		total_count := !total_count + 1;
		match n with
		  NodeSol _ | NodeSyncSol _ -> PSSet.simplify max_nkp (nm_union nm)

		| NodeProc ai -> 
			if ignore_proc ai then
				nm_cross nm
			else
				let aisingle = PSSet.singleton (get_proc_index ai)
				in
				let nm = NodeMap.map (PSSet.rm_sursets max_nkp aisingle) nm
				in
				let pss = nm_cross nm
				in
				PSSet.union pss aisingle

		| NodeObj (a,j,i) -> (
			let r1 = NodeMap.fold (function NodeSol _ | NodeSyncSol _ -> psset_product
										| _ -> (fun _ c -> c)) nm PSSet.full
			in
			r1
			(*
			and r2 = NodeMap.fold (function 
				  NodeObj obj' ->
						let my_obj = (a,j,obj_bounce obj')
						in
						let ctx2 = try fst (Hashtbl.find flood_values (NodeObj my_obj))
										with Not_found -> PSSet.empty
						in
						(fun c1 c2 -> PSSet.union (PSSet.union c1 c2) ctx2)
				| _ -> (fun _ c2 -> c2)) nm PSSet.empty
			in
			psset_simplify (PSSet.union r1 r2)*)
		)
	in
    let init n =
		let register_child nm n' =
			NodeMap.add n' PSSet.empty nm
		in
		let nm0 = List.fold_left register_child NodeMap.empty (gA#childs n)
		in
		PSSet.empty, nm0
    in  
	let t0 = Sys.time ()
	in
    let flood_values = gA#rflood PSSet.equal init default_flooder_update_cache update_value leafs
	in
	dbg ("****** systime: "^string_of_float (Sys.time () -. t0)^"s");
	dbg ("Visited nodes: "^string_of_int !total_count);
	(flood_values, index_proc)

let lcg_for_cutsets env =
	assert_async_an env.an;
	let sols = An_localpaths.MinUnordUnsyncSol.solutions env.sol_cache env.an
	in
	let gA = new glc oa_glc_setup env.ctx env.goal sols make_unord_unsync_sol
	in
    gA#set_auto_conts false;
    gA#build;
    let gA = bot_trimmed_lcg env sols gA
    in
	let gA = cleanup_gA_for_cutsets gA
	in
	top_trimmed_lcg env gA;
	gA


let requirements (gA:#graph) automata leafs universal =
	let max_card = SSet.cardinal automata
	and get_proc_index, index_proc = indexer (gA#count_procs)
	in
	let psset_product a b =
		if b = PSSet.full then a else (
		let na = PSSet.cardinal a
		and nb = PSSet.cardinal b
		in
		let a = if na < nb then PSSet.product max_card b a else PSSet.product max_card b a
		in
		let a = PSSet.simplify max_card a
		in
		a)
	in
	let nm_union nm =
		let c = NodeMap.cardinal nm
		in
		match c with
			  0 -> PSSet.empty
			| 1 -> snd (NodeMap.choose nm)
			| _ -> NodeMap.fold (fun _ -> PSSet.union) nm PSSet.empty
	and nm_cross nm =
		NodeMap.fold (fun _ -> psset_product) nm PSSet.full
	in
	let total_count = ref 0
	in
	let update_value n (_,nm) =
		total_count := !total_count + 1;
		match n with
		  NodeSol _ | NodeSyncSol _ ->
		  	NodeMap.fold (fun _ -> psset_product) nm PSSet.full

		| NodeProc ai ->
			if SSet.mem (fst ai) automata then
				let aisingle = PSSet.singleton (get_proc_index ai)
				in
				(*let nm = NodeMap.map (PSSet.rm_sursets max_card aisingle) nm
				in
				let pss = (if universal then nm_cross else nm_union) nm
				in
				psset_product pss aisingle
				*)
				aisingle
			else
				(if universal then nm_cross else nm_union) nm

		| NodeObj (a,j,i) ->
			NodeMap.fold (function NodeSol _ | NodeSyncSol _ -> PSSet.union | _ -> (fun _ c -> c)) nm PSSet.empty
	in
    let init n =
		let register_child nm n' =
			NodeMap.add n' PSSet.full nm
		in
		let nm0 = List.fold_left register_child NodeMap.empty (gA#childs n)
		in
		PSSet.full, nm0
    in
	let t0 = Sys.time ()
	in
    let flood_values = gA#rflood PSSet.equal init default_flooder_update_cache update_value leafs
	in
	dbg ("****** systime: "^string_of_float (Sys.time () -. t0)^"s");
	dbg ("Visited nodes: "^string_of_int !total_count);
	(flood_values, index_proc)

let lcg_for_requirements env =
	assert_async_an env.an;
	let sols = An_localpaths.MinUnordUnsyncSol.solutions env.sol_cache env.an
	in
	let gA = new glc oa_glc_setup env.ctx env.goal sols make_unord_unsync_sol
	in
    gA#set_auto_conts false;
    gA#build;
	top_trimmed_lcg env gA;
	gA



let worth_lcg ?(skip_oa=false) env =
	assert_async_an env.an;
	let bool_automata = boolean_automata env.an
	in
	let saturate_procs_by_objs =
		let fold_obj obj ps =
			if SSet.mem (obj_sort obj) bool_automata then ps else
			LSSet.union ps (An_localpaths.intermediates env.sol_cache env.an obj)
		in
		ObjSet.fold fold_obj
	in
	let glc_setup = {oa_glc_setup with
		saturate_procs_by_objs = saturate_procs_by_objs}
	and sols = An_localpaths.complete_abstract_solutions env.sol_cache env.an
	in
	let uoa, sols = if skip_oa then true, sols else unordered_oa env sols
	in
	let gB = new glc glc_setup env.ctx env.goal sols make_unord_unsync_sol
	in
	if uoa then (
		gB#set_auto_conts false;
		gB#build;
		gB#saturate_ctx;
		if skip_oa then gB else
		let gB = bot_trimmed_lcg env sols gB
		in
		(top_trimmed_lcg env gB; gB)
	) else gB

let is_localstate_worth gB ls = LSSet.mem ls gB#all_procs

let reduced_an ?(skip_oa=false) env =
	let csols = An_localpaths.concrete_solutions env.sol_cache env.an
	in
	let fold_fpath =
		List.fold_left (fun trs tr -> TRSet.add tr trs)
	in
	let fold_asol trs (obj, conds, interm) =
		let fpaths = csols obj (conds, interm)
		in
		List.fold_left fold_fpath trs fpaths
	in
	let lcg = worth_lcg ~skip_oa env
	in
	let trs = List.fold_left fold_asol TRSet.empty lcg#extract_sols
	in
	let nb_tr = Hashtbl.length env.an.transitions / 2
	and nb_conds = TRSet.cardinal trs
	in
	let an' = { automata = Hashtbl.copy env.an.automata;
				transitions = Hashtbl.create nb_tr;
				conditions = Hashtbl.create nb_conds;
				sync_transitions = []}
	in
	let register_ftr ((a,i,j), cond) =
		let trs = try Hashtbl.find an'.transitions (a,i)
					with Not_found -> ISet.empty
		in
		Hashtbl.add an'.transitions (a,i) (ISet.add j trs);
		Hashtbl.add an'.conditions (a,i,j) cond
	in
	TRSet.iter register_ftr trs;
	an'


(*

let ordered_ua
		?saveGLC:(saveGLC = ref NullGLC)
		?validate:(validate = fun _ -> true)
		env get_Sols glc_setup =
	let build_next_ctx glc ps =
		let ctx = procs_to_ctx (glc#all_procs)
		in
		let ctx = ctx_override ctx ps
		in
		ctx_override_by_ctx glc#ctx ctx
	in
	let rec _ordered_ua ctx = function [] -> true
	| aj::pl ->
		let validate_oua glc =
			if validate glc then
				match pl with [] -> true
				| _ ->
					let lps = glc#lastprocs aj
					in
					dbg ("lastprocs("^string_of_proc aj^") = {"^
								(String.concat "}, {" 
									(List.map string_of_procs lps))^" }");
					assert (lps <> []);
					let next_ctxs = List.map (build_next_ctx glc) lps
					in
					List.for_all (fun ctx -> _ordered_ua ctx pl) next_ctxs
			else false
		in
		dbg ("// underapprox for "^string_of_proc aj^" from "^string_of_ctx ctx);
		unordered_ua ~validate:validate_oua ~saveGLC:saveGLC 
				{env with ctx = ctx; pl = [aj]} get_Sols glc_setup
	in
	_ordered_ua env.ctx env.pl

*)
