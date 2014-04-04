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

open Debug;;

open Ph_types;;

open Ph_glc;;

type env = {
	ph : ph;
	ctx : ctx;
	pl : process list;
	bs_cache : Ph_bounce_seq.bs_cache;
}

let init_env ph ctx pl = {
		ph = ph;
		ctx = ctx;
		pl = pl;
		bs_cache = Ph_bounce_seq.new_bs_cache ();
};;

let update_env env ctx pl = {
		ph = env.ph;
		ctx = ctx;
		pl = pl;
		bs_cache = env.bs_cache;
};;

let color_nodes_connected_to_trivial_sols (gA: #glc) =
	(** each node is associated to a couple
			(green, nm) 
		where nm is the cached value of childs *)

	let init = function
		  NodeSol (obj, ps) -> (PSet.is_empty ps, NodeMap.empty)
		| _ -> (false, NodeMap.empty)

	(* the node n with value v receives update from node n' with value v' *)
	and push n (v,nm) = 
		let new_v = match n with
		  NodeProc _ -> (* at least one child is green *)
		  	let exists_green n' g r = r || g
			in
			NodeMap.fold exists_green nm false
		| NodeSol (obj, ps) -> (* all childs are green *)
			let proc_is_green p =
				try NodeMap.find (NodeProc p) nm
				with Not_found -> false
			in
			PSet.for_all proc_is_green ps
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
;;

let __unordered_over_approx env (gA: #glc) =
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	List.for_all (fun ai -> NodeSet.mem (NodeProc ai) nodes) env.pl
;;


let unordered_over_approx env get_Sols =
	let gA = new glc oa_glc_setup env.ctx env.pl get_Sols
	in
	gA#build;
	gA#debug ();
	__unordered_over_approx env gA
;;

type refGLC = NullGLC | GLC of glc;;

let unordered_ua ?validate:(validate = fun _ -> true) 
			?saveGLC:(saveGLC = ref NullGLC)
			env get_Sols glc_setup =
	let gB_iterator = new glc_generator glc_setup env.ctx env.pl get_Sols
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

let local_reachability ?saveGLC:(saveGLC = ref NullGLC) env =
	let get_Sols = Ph_bounce_seq.get_aBS env.ph env.bs_cache
	in
	if not (unordered_over_approx env get_Sols) then
		False
	else if unordered_ua ~saveGLC:saveGLC env get_Sols ua_glc_setup then
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
				prerr_endline ("+ conds for "^string_of_proc (obj_bounce_proc obj)^" = [" 
									^ (String.concat " ; " (List.map string_of_state conds))
									^ " ]");
				let validate_sol nsol =
					prerr_endline ("checking "^string_of_node nsol);
					let (obj, ps) = match nsol with NodeSol x -> x | _ -> assert false
					and allprocs_sol = fst (Hashtbl.find child_procs nsol)
					in
					prerr_endline (". allprocs_sol = " ^ string_of_ctx allprocs_sol);
					(* check only with conds that are coherent with ps *)
					let cond_select cond = PSet.for_all (fun (a,i) -> try SMap.find a cond == i
						with Not_found -> failwith "invalid cond (coop_priority_reachability)") ps
					in
					let conds = List.filter cond_select conds
					in
					prerr_endline (". conds = [" 
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
	in
	if not (unordered_over_approx env get_Sols) then
		False
	else if unordered_ua ~validate:validate_ua_glc ~saveGLC:saveGLC env get_Sols 
							coop_priority_ua_glc_setup then
		True
	else
		Inconc
;;

let get_Sols env = Ph_bounce_seq.get_aBS env.ph env.bs_cache
;;

let bot_trimmed_cwA env gA =
	let nodes = color_nodes_connected_to_trivial_sols gA
	in
	let gA' = new glc gA#setup env.ctx env.pl (get_Sols env)
	in
	gA#iter (fun node child -> 
				if NodeSet.mem node nodes && NodeSet.mem child nodes then
					gA'#add_child node child);
	gA'#set_trivial_nsols (NodeSet.inter (gA#get_trivial_nsols ()) nodes);
	gA'#set_auto_conts (gA#auto_conts);
	gA'#commit ();
	gA'
;;

let nodes_connected_to_procs (gA: #graph) pl =
	let update_value _ _ = true
    and init n = true, NodeMap.empty
	and update_cache _ nm _ _ = nm
	and leafs = List.fold_left (fun ns ai -> NodeSet.add (NodeProc ai) ns) NodeSet.empty pl
    in  
	gA#rflood ~reversed:true (=) init update_cache update_value leafs
;;

let top_trimmed_cwA env gA =
	let values = nodes_connected_to_procs gA env.pl
	in
	let check_node n = if not (Hashtbl.mem values n) then gA#remove_node n
	in
	NodeSet.iter check_node gA#nodes
;;

