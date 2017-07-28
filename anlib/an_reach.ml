
open Debug

open PintTypes

open AutomataNetwork
open An_localpaths
open LocalCausalityGraph

type env = {
	an : AutomataNetwork.t;
	ctx : ctx;
	goal : ls list;
    ac : An_localpaths.abstract_collection;
}

let init_env an ctx goal =
    let na = count_automata an
    in
    let nbobj = na
    and nblp = 2*na
    in
    let lpc = An_localpaths.new_lp_collection nblp nbobj
    in
    {
		an = an;
		ctx = ctx;
		goal = goal;
		ac = An_localpaths.new_abstract_collection lpc nblp nbobj;
	}


let inject_goal_automaton (an, ctx) sig_goals =
    let ng = List.map List.length sig_goals
    in
    let ig = List.fold_left (+) 1 ng - List.length sig_goals
    in
	let aname = "_pint_goal"
	and sigstates = List.map (fun i -> StateId i) (Util.range 0 ig)
	in
    declare_automaton an aname sigstates;
    let fold_goals i0 sig_goal =
        let n = List.length sig_goal
        in
        let register_step i sig_cond =
            let iorig = if i == 0 then 0 else i0+i
            and idest = if i == n-1 then ig else i0+i+1
            in
            declare_transition an [aname, StateId iorig, StateId idest] sig_cond
        in
        List.iteri register_step sig_goal;
        n-1
    in
    ignore (List.fold_left fold_goals 0 sig_goals);
    let a, itop = Hashtbl.find an.sig2ls (aname, StateId ig)
    and _, ia0 = Hashtbl.find an.sig2ls (aname, StateId 0)
	in
	(an, IMap.add a (ISet.singleton ia0) ctx), (a, itop), [a]


let color_nodes_connected_to_trivial_sols (gA:#lcg) =
	(** each node is associated to a couple
			(green, nm)
		where nm is the cached value of children *)

	let is_green nm n = try NodeMap.find n nm with Not_found -> false
	in
	let ls_is_green nm ls = is_green nm (NodeLS ls)
	in

	let init = function
		  NodeSol (_, alp) -> (StateSet.is_empty alp.conds, NodeMap.empty)
		| _ -> (false, NodeMap.empty)

	(* the node n with value v receives update from node n' with value v' *)
	and push n (v,nm) =
		let new_v = match n with
		  NodeLS _ -> (* at least one child is green *)
		  	let exists_green n' g r = r || g
			in
			NodeMap.fold exists_green nm false
		| NodeSol (_, alp) -> (* all children are green *)
			let state_is_green s =
				IMap.for_all (fun a i -> ls_is_green nm (a,i)) s
			in
			StateSet.for_all state_is_green alp.conds
		| NodeObj _ -> (* all NodeObj children are green; at least one NodeSol is green *)
			let exists_sol_green = function
				  NodeSol _ -> fun g r -> r || g
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

let restrict_sols ?(update=true) overlay lcg valid =
    let apply_obj obj = if update || not (Hashtbl.mem overlay obj) then (
        let nobj = NodeObj obj
        in
        if NodeSet.mem nobj valid then
            let sols = List.filter (fun node ->
                match node with NodeSol (id, _) -> NodeSet.mem node valid
                  | _ -> false) (lcg#children nobj)
            in
            let ialps = List.map (function NodeSol (id, _) -> id
                                    | _ -> failwith "invalid argument") sols
            in
            Hashtbl.replace overlay obj ialps
        else
            Hashtbl.replace overlay obj [])
    in
    ObjSet.iter apply_obj lcg#objs

let unordered_oa env sols =
	dbg ~level:1 ". unordered over-approximation";
    let oa_lcg = build_oa_lcg env.an env.ctx env.goal sols
	in
	let valid = color_nodes_connected_to_trivial_sols oa_lcg
	in
	List.for_all (fun ai -> NodeSet.mem (NodeLS ai) valid) env.goal,
    (oa_lcg, valid)


(**** Local reachability ****)

let local_reachability env =
	let sols = An_localpaths.abstract_local_paths env.ac env.an
	in
	let uoa, (oa_lcg, valid) = unordered_oa env sols
	in
	if not uoa then
		False
	else (
        assert_async_an env.an;
        let overlay = Hashtbl.create (NodeSet.cardinal valid)
        in
        restrict_sols overlay oa_lcg valid;
        let sols = An_localpaths.restricted_abstract_local_paths env.ac env.an overlay
		in
		if An_reach_asp.unordered_ua env.an env.ctx env.goal sols then
			True
		else
			Inconc
    )





(**************************************************
		CUTSETS / REQUIREMENTS
***************************************************)

let bot_trimmed_lcg env sols lcg =
	let valid = color_nodes_connected_to_trivial_sols lcg
	in
	let lcg' = new lcg lcg#setup env.an env.ctx env.goal sols
	in
	lcg#iter (fun node child ->
				if NodeSet.mem node valid && NodeSet.mem child valid then
					lcg'#add_child node child);
	lcg'#set_trivial_nsols (NodeSet.inter lcg#trivial_nsols valid);
	lcg'#set_auto_conts lcg#auto_conts;
	lcg'#commit ();
	lcg'

let nodes_connected_to_procs (gA: #graph) pl =
	let update_value _ _ = true
    and init n = true, NodeMap.empty
	and update_cache _ nm _ _ = nm
	and leafs = List.fold_left (fun ns ai -> NodeSet.add (NodeLS ai) ns) NodeSet.empty pl
    in
	gA#rflood ~reversed:true (=) init update_cache update_value leafs

let top_trimmed_lcg env gA =
	let values = nodes_connected_to_procs gA env.goal
	in
	let check_node n = if not (Hashtbl.mem values n) then gA#remove_node n
	in
	NodeSet.iter check_node gA#nodes



(**
 * Cutsets
 *)

module PSSet = KSets.Make(struct type t = int let compare = compare end);;

let scc_dead_rel children scc =
	let c = List.length scc
	in
	if c > 2 then (
		dbg ("|SCC|="^string_of_int c);
		let scc_idx = List.fold_left (fun s n -> NodeSet.add n s) NodeSet.empty scc
		in
		let entry_objectives n =
			match n with
			  NodeObj _ | NodeLS _ ->
			  	[] <> List.filter (fun n -> not (NodeSet.mem n scc_idx)) (children n)
			| _ -> false
		and dead_children n =
			let dead_node n' =
				match n' with NodeSol _ -> NodeSet.mem n' scc_idx | _ -> false
			in
			List.filter dead_node (children n)
		in
		let objs = List.filter entry_objectives scc
		in
		match objs with
		  [n] -> (
				let cs = dead_children n in
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
		match scc_dead_rel gA#children scc with
		  Some x -> x::todel
		| None -> todel
	in
	let todel = List.fold_left handle_scc [] sccs
	in
	let apply (n, dead_children) =
		List.iter (fun c -> gA#remove_child c n) dead_children
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
		  NodeSol _ -> PSSet.simplify max_nkp (nm_union nm)

		| NodeLS ai ->
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
			let r1 = NodeMap.fold (function NodeSol _ -> psset_product
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
		let nm0 = List.fold_left register_child NodeMap.empty (gA#children n)
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
	let sols = An_localpaths.abstract_local_paths env.ac env.an
	in
	let lcg = new lcg default_lcg_setup env.an env.ctx env.goal sols
	in
    lcg#set_auto_conts false;
    lcg#build;
    let lcg = bot_trimmed_lcg env sols lcg
    in
	let lcg = cleanup_gA_for_cutsets lcg
	in
	top_trimmed_lcg env lcg;
	lcg

let requirements (gA:#graph) automata leafs universal =
	let max_card = ISet.cardinal automata
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
		  NodeSol _ ->
		  	NodeMap.fold (fun _ -> psset_product) nm PSSet.full

		| NodeLS ai ->
			if ISet.mem (fst ai) automata then
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
			NodeMap.fold (function NodeSol _ -> PSSet.union | _ -> (fun _ c -> c)) nm PSSet.empty
	in
    let init n =
		let register_child nm n' =
			NodeMap.add n' PSSet.full nm
		in
		let nm0 = List.fold_left register_child NodeMap.empty (gA#children n)
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
	let sols = An_localpaths.abstract_local_paths env.ac env.an
	in
	let lcg = new lcg default_lcg_setup env.an env.ctx env.goal sols
	in
    lcg#set_auto_conts false;
    lcg#build;
	top_trimmed_lcg env lcg;
	lcg



let gored_lcg ?(skip_oa=false) env =
	assert_async_an env.an; (* TODO *)

    let valid_solutions = if skip_oa then An_localpaths.abstract_local_paths env.ac env.an else
        let sols = An_localpaths.abstract_local_paths env.ac env.an
        in
        let overlay = Hashtbl.create 42
        and oa_lcg = build_oa_lcg env.an env.ctx env.goal sols
        in
        fun obj -> (
            (if not (Hashtbl.mem overlay obj) then
                let _ = oa_lcg#build_obj obj
                in
                let valid = color_nodes_connected_to_trivial_sols oa_lcg
                in
                restrict_sols ~update:false overlay oa_lcg valid);
            An_localpaths.restricted_abstract_local_paths env.ac env.an overlay obj
        )
    in
	let saturate_lss_by_nodes nodes lss =
        let fold node lss = match node with
          NodeSol (_, alp) ->
                let a = obj_a alp.obj
                in
                ISet.fold (fun i -> LSSet.add (a,i)) alp.interm lss
        | _ -> lss
        in
        NodeSet.fold fold nodes lss
    in
	let lcg_setup = {default_lcg_setup with
		saturate_lss_by_nodes = saturate_lss_by_nodes}
    in
    let rlcg = new lcg lcg_setup env.an env.ctx env.goal valid_solutions
    in
    rlcg#set_auto_conts false;
    rlcg#build;
    rlcg#saturate_ctx;
    rlcg

let gored_trs ?(skip_oa=false) env =
    let rlcg = gored_lcg ~skip_oa env
    in
    let pull_trs node trs = match node with
          NodeSol (id, _) -> extract_transitions_from_ialp ~trs env.ac id
        | _ -> trs
    in
    NodeSet.fold pull_trs rlcg#nodes ISet.empty

let reduced_an ?(skip_oa=false) env =
    let trs = gored_trs ~skip_oa env
    in
    let nb_trs = ISet.cardinal trs
    and filter trid _ = ISet.mem trid trs
    in
    An_transformers.an_with_filtered_transitions ~nb_trs env.an filter


