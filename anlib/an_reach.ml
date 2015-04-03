(*
Copyright or © or Copr. Loïc Paulevé (2010-2011)

lp@inzenet.org

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

let color_nodes_connected_to_trivial_sols (gA: #glc) =
	(** each node is associated to a couple
			(green, nm) 
		where nm is the cached value of childs *)

	let init = function
		  NodeSol (obj, ps, _) -> (LSSet.is_empty ps, NodeMap.empty)
		| _ -> (false, NodeMap.empty)

	(* the node n with value v receives update from node n' with value v' *)
	and push n (v,nm) = 
		let new_v = match n with
		  NodeProc _ -> (* at least one child is green *)
		  	let exists_green n' g r = r || g
			in
			NodeMap.fold exists_green nm false
		| NodeSol (obj, ps, _) -> (* all childs are green *)
			let proc_is_green p =
				try NodeMap.find (NodeProc p) nm
				with Not_found -> false
			in
			LSSet.for_all proc_is_green ps
		| NodeObj _ -> (* all NodeObj childs are green; at least one NodeSol is green *)
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

let restricted_sols_factory all_sols nodes =
	let register_node n rsols = match n with
		  NodeSol (obj, ps, interm) -> 
		  	ObjMap.add obj ((ps,interm)::try ObjMap.find obj rsols 
									with Not_found -> []) rsols
		| _ -> rsols
	in
	let rsols = NodeSet.fold register_node nodes ObjMap.empty
	in
	fun obj -> try ObjMap.find obj rsols with Not_found -> all_sols obj

let unordered_over_approx env sols =
	let gA = new glc oa_glc_setup env.ctx env.goal sols
	in
	gA#build;
	gA#debug ();
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	List.for_all (fun ai -> NodeSet.mem (NodeProc ai) nodes) env.goal, 
	restricted_sols_factory sols nodes

let bot_trimmed_lcg env sols gA =
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	let gA' = new glc gA#setup env.ctx env.goal sols
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

let lcg_for_cutsets env =
	let sols = An_localpaths.min_abstract_solutions env.sol_cache env.an
	in
	let gA = new glc oa_glc_setup env.ctx env.goal sols
	in
    gA#set_auto_conts false;
    gA#build;
    let gA = bot_trimmed_lcg env sols gA
    in
	let gA = cleanup_gA_for_cutsets gA
	in
	top_trimmed_lcg env gA;
	gA

let worth_lcg env =
	let folder a def bools =
		if List.length def = 2 then SSet.add a bools else bools
	in
	let bool_automata = Hashtbl.fold folder env.an.automata SSet.empty
	in
	let saturate_procs_by_objs =
		let fold_obj obj ps =
			if SSet.mem (obj_sort obj) bool_automata then ps else
			LSSet.union ps (An_localpaths.intermediates env.sol_cache env.an obj)
		in
		ObjSet.fold fold_obj
	in
	let glc_setup = {ua_glc_setup with
		saturate_procs_by_objs = saturate_procs_by_objs}
	and sols = An_localpaths.complete_abstract_solutions env.sol_cache env.an
	in
	let uua, sols = unordered_over_approx env sols
	in
	let gB = new glc glc_setup env.ctx env.goal sols
	in
	if uua then (
		gB#build;
		gB#saturate_ctx;
		let gB = bot_trimmed_lcg env sols gB
		in
		top_trimmed_lcg env gB;
		gB
	) else gB

let is_localstate_worth gB ls = LSSet.mem ls gB#all_procs

let reduced_an env =
	let module TRSet = Set.Make (struct
				type t = transition * automaton_state SMap.t
				let compare (tr, conds) (tr', conds') =
					let ctr = compare tr tr'
					in
					if ctr = 0 then SMap.compare compare conds conds' else ctr
			end)
	in
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
	let lcg = worth_lcg env
	in
	let trs = List.fold_left fold_asol TRSet.empty lcg#extract_sols
	in
	let nb_tr = Hashtbl.length env.an.transitions / 2
	and nb_conds = TRSet.cardinal trs
	in
	let an' = { automata = Hashtbl.copy env.an.automata;
				transitions = Hashtbl.create nb_tr;
				conditions = Hashtbl.create nb_conds }
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


let local_reachability (*?saveGLC:(saveGLC = ref NullGLC) *) env =
	let sols = An_localpaths.min_abstract_solutions env.sol_cache env.an
	in
	let uua, restr_sols = unordered_over_approx env sols
	in
	if not uua then
		False
	else (
		prerr_endline ("! Warning, under-approximation is not implemented yet!");
		(*else if ordered_ua ~saveGLC:saveGLC env restr_sols ua_glc_setup then
			True
		else match env.pl with [] | [_] -> Inconc | _ ->
			if unordered_ua ~saveGLC:saveGLC env restr_sols ua_glc_setup then
				True
			else *)
				Inconc
	)

(*

type refGLC = NullGLC | GLC of glc;;

let unordered_ua ?validate:(validate = fun _ -> true) 
			?saveGLC:(saveGLC = ref NullGLC)
			env get_Sols glc_setup =
	let gB_iterator = new glc_generator glc_setup env.ctx env.pl env.concrete get_Sols
	in
	(*let i = ref 0 in*)
	let rec __check gB =
		if gB#has_impossible_objs then (
			dbg ~level:1 ("has_impossible_objs! "^
				(String.concat ";" (List.map string_of_obj gB#get_impossible_objs)));
			(*
			let cout = open_out ("/tmp/glc"^string_of_int !i^".dot")
			in
			i := !i + 1;
			output_string cout gB#to_dot;
			close_out cout;*)
			let ms_objs =  gB_iterator#multisols_objs
			in
			let seq_objs = gB#avoid_impossible_objs ms_objs
			in
			List.iter (fun objs -> gB_iterator#change_objs (ObjSet.elements objs))
						(List.rev seq_objs);
			false
		) else if gB#has_loops then (
			dbg ~level:1 "has_loops!";
			let seq_objs = gB#avoid_loop gB_iterator#multisols_objs gB#last_loop
			in
			List.iter (fun objs -> gB_iterator#change_objs (ObjSet.elements objs))
						seq_objs;
			false
		) else (
			if not gB#auto_conts then (
				gB#set_auto_conts true;
				gB#commit ();
				__check gB
			) else 
				validate gB
		)
	in
	let rec iter_gBs () =
		if gB_iterator#has_next then (
			let gB = gB_iterator#next
			in
			dbg "!! unordered underapprox";
			gB#debug ();
			if __check gB then (
				saveGLC := GLC gB;
				raise Found
			) else iter_gBs ()
		)
	in
	try iter_gBs (); false with Found -> true
;;

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
;;


let local_reachability 
		?saveGLC:(saveGLC = ref NullGLC) env =
	let get_Sols = Ph_bounce_seq.get_aBS env.ph env.bs_cache
	in
	let uua, restr_sols = unordered_over_approx env get_Sols
	in
	if not uua then
		False
	else if ordered_ua ~saveGLC:saveGLC env restr_sols ua_glc_setup then
		True
	else match env.pl with [] | [_] -> Inconc | _ ->
		if unordered_ua ~saveGLC:saveGLC env restr_sols ua_glc_setup then
			True
		else
			Inconc
;;

let merge_sort_indexes ps a =
	let fold_index ps j =
		PSet.add (a,j) ps
	in
	List.fold_left fold_index ps
;;

let coop_priority_augment_procs ctx ps =
	(* for each cooperative sort, add processes corresponding to 
			parent processes *)
	let ctx = (ctx_union ctx (procs_to_ctx ps))
	in
	let fold_cooperativity a _ ps =
		let js = Ph_cooperativity.resolve !Ph_instance.cooperativities ctx a
		in
		merge_sort_indexes ps a js
	in
	SMap.fold fold_cooperativity !Ph_instance.cooperativities ps
;;

let coop_priority_ua_glc_setup = {ua_glc_setup with 
	saturate_procs = coop_priority_augment_procs}
;;


let coop_priority_reachability ?saveGLC:(saveGLC = ref NullGLC) env =
	let is_cooperative a = SMap.mem a !Ph_instance.cooperativities
	in
	let validate_ua_glc (glc:#glc) =
		(* associate to each nodes the children processes *)
		let child_procs = glc#call_rflood allprocs_flooder glc#leafs
		in
		(* iter over cooperative objectives and check their solutions *)
		let validate_obj obj =
			if is_cooperative (obj_sort obj) then (
				(* TODO: conds bj is static; use cache *)
				let conds = Ph_cooperativity.local_fixed_points !Ph_instance.cooperativities
								env.ph (obj_bounce_proc obj)
				in
				dbg ("+ conds for "^string_of_proc (obj_bounce_proc obj)^" = [" 
									^ (String.concat " ; " (List.map string_of_state conds))
									^ " ]");
				let validate_sol nsol =
					dbg ("checking "^string_of_node nsol);
					let (obj, ps, _) = match nsol with NodeSol x -> x | _ -> assert false
					and allprocs_sol = fst (Hashtbl.find child_procs nsol)
					in
					dbg (". allprocs_sol = " ^ string_of_ctx allprocs_sol);
					(* check only with conds that are coherent with ps *)
					let cond_select cond = PSet.for_all (fun (a,i) -> try SMap.find a cond == i
						with Not_found -> failwith "invalid cond (coop_priority_reachability)") ps
					in
					let conds = List.filter cond_select conds
					in
					dbg (". conds = [" 
										^ (String.concat " ; " (List.map string_of_state conds))
										^ " ]");
					(* check that cond includes allprocs_sol *)
					let cond_match cond =
						let cap a i =
							try
								let js = ctx_get a allprocs_sol
								in
								ISet.cardinal js <= 1 && ISet.choose js == i
							with Not_found -> true
						in
						SMap.for_all cap cond
					in
					match conds with [] -> false | _ -> List.for_all cond_match conds
				in
				let sols = List.filter (function NodeSol _ -> true | _ -> false)
					(glc#childs (NodeObj obj))
				in
				List.for_all validate_sol sols
			) else true
		in
		ObjSet.for_all validate_obj glc#objs
	in
	let get_Sols = Ph_bounce_seq.get_aBS env.ph env.bs_cache
	and concrete = {env.concrete with process_cond =
		Ph_cooperativity.local_fixed_points !Ph_instance.cooperativities env.ph
	} in
	let env = {env with concrete = concrete}
	in
	let uua, get_Sols = unordered_over_approx env get_Sols
	in
	if not uua then
		False
	else if ordered_ua ~validate:validate_ua_glc ~saveGLC:saveGLC 
				env get_Sols coop_priority_ua_glc_setup then
		True
	else match env.pl with [] | [_] -> Inconc | _ ->
		if unordered_ua ~validate:validate_ua_glc ~saveGLC:saveGLC
				env get_Sols coop_priority_ua_glc_setup then
			True
		else
			Inconc
;;

*)
