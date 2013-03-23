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

	(*dbg ("Leafs: "^(String.concat ";" (List.map string_of_node (NodeSet.elements (gA#get_leafs ())))));*)
	let values = Hashtbl.create 50
	in
	gA#deprecated_rflood init update_cache push values gA#leafs;
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



let unordered_ua env get_Sols glc_setup =
	let gB_iterator = new glc_generator glc_setup env.ctx env.pl get_Sols
	in
	let rec __check gB =
		if gB#has_impossible_objs then (
			prerr_endline ("has_impossible_objs! "^
				(String.concat ";" (List.map string_of_obj gB#get_impossible_objs)));
			let objs = gB#analyse_impossible_objs gB_iterator#multisols_objs
			in
			gB_iterator#change_objs objs;
			false
		) else if gB#has_loops then (
			dbg "has_loops!";
			let objs = gB#analyse_loop gB#last_loop gB_iterator#multisols_objs
			in
			gB_iterator#change_objs objs;
			false
		) else (
			if not gB#auto_conts then (
				gB#set_auto_conts true;
				gB#commit ();
				__check gB
			) else true
		)
	in
	let rec iter_gBs () =
		if gB_iterator#has_next then
			let gB = gB_iterator#next
			in
			dbg "!! unordered underapprox";
			gB#debug ();
			(if __check gB then raise Found);
			iter_gBs ()
		else false
	in
	try iter_gBs () with Found -> true
;;

let local_reachability env =
	let get_Sols = Ph_bounce_seq.get_aBS env.ph env.bs_cache
	in
	if not (unordered_over_approx env get_Sols) then
		False
	else if unordered_ua env get_Sols ua_glc_setup then
		True
	else
		Inconc
;;

let coop_priority_reachability env =
	let get_Sols = Ph_bounce_seq.get_aBS env.ph env.bs_cache
	in
	if not (unordered_over_approx env get_Sols) then
		False
	else if unordered_ua env get_Sols coop_priority_ua_glc_setup then
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

let test = local_reachability;;

