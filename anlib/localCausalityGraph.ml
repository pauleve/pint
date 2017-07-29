
open Debug

open PintTypes
open AutomataNetwork
open An_localpaths

type node =
	  NodeLS of ls
	| NodeSol of (id * abstract_local_path)
	| NodeObj of objective

module NodeOrd = struct type t = node let compare = compare end
module NodeSet = Set.Make (NodeOrd)
module NodeMap = Map.Make (NodeOrd)

module RankedNodeSet = Set.Make(struct type t = int * node let compare = compare end)

let string_of_node an = function
	  NodeSol (idsol, alp) -> "Sol["^string_of_int idsol^":"
                        ^ string_of_abstract_local_path an alp^"]"
	| NodeObj obj -> "Obj["^string_of_obj an obj^"]"
	| NodeLS p -> "LS["^string_of_ls an p^"]"

let string_of_nodeset an = string_of_set (string_of_node an) NodeSet.elements;;

(**
	flooder setup
**)
type ('a, 'b) flooder_setup = {
	equality: 'a -> 'a -> bool;	
	node_init: node -> 'a * 'b;
	update_cache: node -> 'a * 'b -> node -> 'a * 'b  -> 'a * 'b;
	update_value: node -> 'a * 'b -> 'a;
}

(**
   shared functions related to rflood algorithms 

   each node is associated to a couple
			(ctx, nm) 
		where nm is the cached value of children
 **)

let default_flooder_node_init empty_value = 
	fun n -> empty_value, NodeMap.empty

let default_flooder_update_cache n (v,nm) n' (v',_) =
	(v, NodeMap.add n' v' nm)

let union_value nm =
	NodeMap.fold (fun _ -> ctx_union) nm ctx_empty

let inter_value nm =
		let r = NodeMap.fold (fun _ c1 -> function
			  None -> Some c1
			| Some c2 -> Some (ctx_inter c1 c2)) nm None
		in
		match r with
		  None -> ctx_empty
		| Some c -> c

(**
	generic graph
*)

class graph an =
object(self)
	val edges = Hashtbl.create 1000
	val rev_edges = Hashtbl.create 1000

	val mutable nodes = NodeSet.empty

	val mutable procs = LSSet.empty
	val mutable objs = ObjSet.empty
	val mutable all_procs = LSSet.empty
	val mutable self_loops = NodeSet.empty

	method nodes = nodes
	method procs = procs
	method all_procs = all_procs
	method objs = objs
	method has_ls p = LSSet.mem p procs
	method has_obj obj = ObjSet.mem obj objs
	method has_child c n = List.mem c (self#children n)
	method count_procs = LSSet.cardinal procs
	method count_objs = ObjSet.cardinal objs
	method count_nodes = NodeSet.cardinal nodes

	method debug () = if !Debug.dodebug then (
		let sol = "#aS# "
		and eol = "\n"
		in
		let register_proc p =
			let rels = Hashtbl.find_all edges (NodeLS p)
			in
			sol^"Req("^string_of_ls an p^") = [ "^
				(String.concat "; " (List.map (function NodeObj obj -> string_of_obj an obj
						| _ -> failwith "invalid graph (debug/register_proc)") rels))^" ]"^eol
		and register_obj (sols,conts) obj =
			let rels = self#children (NodeObj obj)
			in
			let solrels, contrels = List.partition (function
							NodeSol _ -> true
							| NodeObj _ -> false | _ ->
								failwith "invalid graph (debug/register_obj)") rels
			in
			let sols = if solrels == [] then sols else
				(sol^"Sol("^string_of_obj an obj^") = [ "^
					(String.concat "; " (List.map (string_of_node an) solrels))^" ]"^eol)::sols
			and conts = if contrels == [] then conts else
				(sol^"Cont("^string_of_obj an obj^") = [ "^
					(String.concat "; " (List.map (string_of_node an) contrels))^" ]"^eol)::conts
			in
			sols, conts
		in
		let reqs = List.map register_proc (LSSet.elements procs)
		and sols, conts = List.fold_left register_obj ([],[]) (ObjSet.elements objs)
		in
		let sols, conts = List.rev sols, List.rev conts
		in
		let buf =
			 sol^"local states = "^string_of_lsset an procs^eol
			^sol^"objs = "^(string_of_set (string_of_obj an) ObjSet.elements objs)^eol
			^(String.concat "" reqs)
			^(String.concat "" sols)
			^(String.concat "" conts)
			^sol^"procs count = "^string_of_int self#count_procs
				^"; objs count = "^string_of_int self#count_objs
		in
		dbg buf)

	method to_dot =
		let dot_of_obj (a,i,j) =
			"O_"^string_of_int a^"_"^string_of_int i ^"_"^string_of_int j
		and dot_of_proc = string_of_ls an
		in
		let synccounter = ref 0
		in
		let dot_of_sol idsol =
			let node = "pintsol"^string_of_int idsol
			in
			let dot = node^"[label=\"\",shape=circle,fixedsize=true,width=0.1,height=0.1];\n"
			in
			(node, dot)
		and dot_of_sync = function
              [] -> failwith "oops (empty sync condition)"
            | [ls] ->
				let dproc = dot_of_proc ls
				in
				dproc, ""
            | state ->
				let _ = incr synccounter
                in
				let node = "pintsync"^string_of_int !synccounter
				in
				let dot = node^"[label=\"\",shape=square,fixedsize=true,width=0.1,height=0.1];\n"
				in
				let dot = dot ^ String.concat "" (List.map (fun p ->
						node^" -> "^(dot_of_proc p)^";\n") state)
				in
				node, dot
		in

		let register_proc p =
			let dproc = dot_of_proc p
			in
			let dot_of_rel = function
				NodeObj obj -> dproc ^ " -> " ^ dot_of_obj obj ^";"
				| _ -> failwith "invalid graph (to_dot/register_proc)"
			in
			let rels = Hashtbl.find_all edges (NodeLS p)
			in
			let def = dproc^"[shape=box];\n"
			and edges = String.concat "\n" (List.map dot_of_rel rels)
			in
			def ^ edges ^ "\n"

		and register_obj obj =
			let dobj = dot_of_obj obj
			in
			let dot_of_rel = function
				NodeSol (idsol, alp) ->
					let dsol, def = dot_of_sol idsol
					in
					let dsyncs, defs = List.split
						(List.map dot_of_sync alp.conds)
					in
					def
					^ dobj ^" -> "^dsol^";\n"
					^ (String.concat "" defs)
					^ String.concat "" (List.map (fun dsync ->
						dsol^" -> "^dsync^";\n") dsyncs)
				| NodeObj obj' -> dobj ^" -> "^dot_of_obj obj'^";"
				| _ -> failwith "invalid graph (to_dot/register_obj)"
			in
			let rels = self#children (NodeObj obj)
			in
			let def = dobj^"[label=\""^string_of_obj an obj^"\"];\n"
			and edges = String.concat "\n" (List.map dot_of_rel rels)
			in
			def ^ edges ^ "\n"
		in
		let procs = List.map register_proc (LSSet.elements procs)
		and objs = List.map register_obj (ObjSet.elements objs)
		in
		let content = String.concat "\n" procs ^ String.concat "\n" objs
		in
		"digraph { \n" ^ content ^ "}\n"

	method register_node = function
		  NodeLS p -> (procs <- LSSet.add p procs;
		  				all_procs <- LSSet.add p all_procs)
		| NodeObj (a,i,j) -> 
				(procs <- LSSet.add (a,j) procs;
				all_procs <- LSSet.add (a,j) (LSSet.add (a,i) all_procs);
				objs <- ObjSet.add (a,i,j) objs)
		| _ -> ()

	method children n =
		Hashtbl.find_all edges n
	method parents n = 
		Hashtbl.find_all rev_edges n

	method add_child c n =
		nodes <- NodeSet.add c (NodeSet.add n nodes);
		self#register_node n;
		self#register_node c;
		Hashtbl.add edges n c;
		Hashtbl.add rev_edges c n;
		if c = n then
			self_loops <- NodeSet.add c self_loops
	
	method remove_child c n =
		Util.hashtbl_filter_bindings edges n (fun m -> c <> m);
		Util.hashtbl_filter_bindings rev_edges c (fun m -> n <> m)
	
	method remove_node n =
		nodes <- NodeSet.remove n nodes;
		(match n with
		  NodeLS p -> procs <- LSSet.remove p procs
		| NodeObj (a,i,j) -> 
			(if not (NodeSet.mem (NodeLS (a,j)) nodes) then
				procs <- LSSet.remove (a,j) procs);
			objs <- ObjSet.remove (a,i,j) objs
		| _ -> ());
		let cs = self#children n
		and ps = self#parents n
		in
		Util.hashtbl_filter_bindings edges n (fun _ -> false);
		Util.hashtbl_filter_bindings rev_edges n (fun _ -> false);
		List.iter (fun p -> Util.hashtbl_filter_bindings edges p (fun m -> n <> m)) ps;
		List.iter (fun c -> Util.hashtbl_filter_bindings rev_edges c (fun m -> n <> m)) cs
	
	method iter f = Hashtbl.iter f rev_edges
	
	method call_rflood
		: 'a 'b. ?reversed:bool -> ('a,'b) flooder_setup -> NodeSet.t 
			-> (node, 'a * 'b) Hashtbl.t
			= fun ?reversed:(desc=false) cfg ->
		self#rflood ~reversed:desc cfg.equality cfg.node_init cfg.update_cache
						cfg.update_value

	method rflood
		: 'a 'b. ?reversed:bool -> ('a -> 'a -> bool) -> (node -> 'a * 'b) 
			-> (node -> 'a * 'b -> node -> 'a * 'b -> 'a * 'b) (* update_cache *)
			-> (node -> 'a * 'b -> 'a) (* update_value *)
			-> NodeSet.t -> (node, 'a * 'b) Hashtbl.t
		= fun ?reversed:(desc=false) equality init update_cache update_value ns ->
		let parents, children = if desc then (self#children, self#parents)
										else (self#parents, self#children)
		in

		let values = Hashtbl.create self#count_nodes
		in

		(* compute strongly connected components *)
		let sccs = self#tarjan_SCCs desc ns
		in
		let sccs_id = Hashtbl.create self#count_nodes
		in
		let register_scc id scc =
			List.iter (fun node -> Hashtbl.add sccs_id node id) scc;
			id+1
		in
		let default_id = List.fold_left register_scc 0 sccs
		in
		dbg (string_of_int default_id^" SCCs");
		let get_scc_id n =
			try Hashtbl.find sccs_id n with Not_found -> default_id
		in
		let handle i (ready, news) n = 
			let ready = RankedNodeSet.remove (i,n) ready
			and isnew, news =
				if NodeSet.mem n news then
					(true, NodeSet.remove n news)
				else
					(false, news)
			in
			(* compute value *)
			let v, nm = try Hashtbl.find values n with Not_found -> init n
			in
			let v' = update_value n (v,nm)
			in
			let changed = isnew || not (equality v v')
			in
			if not changed then 
				(ready, news)
			else
				let nv = (v',nm)
				in
				Hashtbl.replace values n nv;

				(* update cached values of children *)
				let forward (ready, news) n' =
					let n'v, isnew = try Hashtbl.find values n', false
										with Not_found -> (init n', true)
					in
					let n'v = update_cache n' n'v n nv
					and news = if isnew then NodeSet.add n' news else news
					in
					Hashtbl.replace values n' n'v;
					(RankedNodeSet.add (get_scc_id n', n') ready, news)
				in
				List.fold_left forward (ready, news) (parents n)
		in
		let rec flood j (ready, news) = 
			let (i,n) = RankedNodeSet.min_elt ready
			in
			dbg_noendl ("\r["^string_of_int i^"] ");
			let (ready, news) = handle i (ready, news) n
			in
			if not (RankedNodeSet.is_empty ready) then
				flood i (ready, news)
		in
		let fold_n n ins =
			RankedNodeSet.add (get_scc_id n, n) ins
		in
		let ins = NodeSet.fold fold_n ns RankedNodeSet.empty
		in
		if not (RankedNodeSet.is_empty ins) then
			(flood (-1) (ins, ns);
			dbg_noendl "\n")
		else dbg "skip";
		values

	method tarjan_SCCs desc leafs =
		let get_children = if desc then self#children else self#parents
		in
		let index = Hashtbl.create self#count_nodes
		and lowlink = Hashtbl.create self#count_nodes
		and next_index = ref 0
		and stack = ref []
		in
		let rec strongconnect v sccs =
			Hashtbl.add index v !next_index;
			Hashtbl.add lowlink v !next_index;
			next_index := !next_index + 1;
			stack := v::!stack;
			let handle_child sccs w =
				if not(Hashtbl.mem index w) then (
					let sccs = strongconnect w sccs
					in
					let l_v = Hashtbl.find lowlink v
					and l_w = Hashtbl.find lowlink w
					in
					Hashtbl.replace lowlink v (min l_v l_w);
					sccs
				) else (
					if List.mem w !stack then (
						let l_v = Hashtbl.find lowlink v
						and i_w = Hashtbl.find index w
						in
						Hashtbl.replace lowlink v (min l_v i_w);
					);
					sccs
				)
			in
			let sccs = List.fold_left handle_child sccs (get_children v)
			in

			let rec unroll () =
				match !stack with
				  [] -> failwith "unroll empty stack!"
				| a::q ->
					stack := q;
					if a = v then [a] else (a::unroll ())
			in
			let l_v = Hashtbl.find lowlink v
			and i_v = Hashtbl.find index v
			in
			if l_v = i_v then 
				let scc = unroll ()
				in
				let scc = List.rev scc
				in
				scc::sccs
			else sccs

		in
		let fold_node v sccs =
			if not(Hashtbl.mem index v) then strongconnect v sccs else sccs
		in
		NodeSet.fold fold_node leafs []

end;;


(**
	GLC
*)

(*
type _concrete_ph = {
	lasthitters: ?filter:(action list -> bool) -> objective -> LSSet.t;
	process_cond: process -> state list;
}
*)

type lcg_setup = {
	conts_flooder: (ctx, ctx NodeMap.t) flooder_setup;
	conts: ctx -> objective -> ctx -> ISet.t;
	saturate_procs: ctx -> LSSet.t -> LSSet.t;
	saturate_lss_by_nodes: NodeSet.t -> LSSet.t -> LSSet.t;
}

class lcg lcg_setup an ctx pl solutions
    = object(self) inherit graph an as g

	method setup = lcg_setup

	val mutable current_ctx = ctx
	val mutable new_objs = []
	val mutable trivial_nsols = NodeSet.empty
	val mutable impossible_nobjs = NodeSet.empty
	method impossible_nobjs = impossible_nobjs
	method trivial_nsols = trivial_nsols
    method set_trivial_nsols t = trivial_nsols <- t
	method leafs = NodeSet.union trivial_nsols impossible_nobjs
	method has_impossible_objs = not (NodeSet.is_empty impossible_nobjs)
	method ctx = current_ctx
	method get_impossible_objs = NodeSet.fold (function NodeObj obj -> fun objs -> obj::objs
													| _ -> fun objs -> objs) impossible_nobjs []

	val mutable auto_conts = true
	method set_auto_conts t = auto_conts <- t
	method auto_conts = auto_conts

	method conts_flooder = self#call_rflood lcg_setup.conts_flooder
	method conts = lcg_setup.conts self#ctx

	method build =
		List.iter self#push_ls pl;
		self#commit ()

    method build_obj obj =
        self#push_obj obj (NodeObj obj);
        self#commit ()

	method commit () =
		(*self#debug ();*)
		if self#auto_conts then (
		dbg "Automatically pushing conts";
		(* update conts_flood with new objectives *)
		let conts_flood = self#conts_flooder self#leafs
		in
		(* we assume the minCont grows *)
		let register_cont obj = 
			let nobj = NodeObj obj
			in
			try
				let ctx = fst (Hashtbl.find conts_flood nobj)
				and a,i,j = obj
				in
				let make_cont i' =
					let obj' = (a,i',j)
					in
					let nto = NodeObj obj'
					in
					(if not (self#has_obj obj') then self#push_obj obj' nto);
					if not (self#has_child nto nobj) then
						self#add_child nto nobj
				in
				let ais = self#conts obj ctx
				in
				let ais = ISet.remove j ais
				in
				dbg ("cont("^string_of_obj an (a,i,j)^")="
                        ^string_of_cls an a ais);
				ISet.iter make_cont ais
			with Not_found -> ()
		in
		let my_objs = new_objs
		in
		if Hashtbl.length conts_flood > 0 then (
		new_objs <- [];
		List.iter register_cont my_objs;
		if new_objs <> [] then self#commit ()))

	method push_obj obj nobj =
        let isols, sols = solutions obj
        in
		(if isols == [] then
            impossible_nobjs <- NodeSet.add nobj impossible_nobjs);
		let register_sol id alp =
            let nsol = NodeSol (id, alp)
            and lss = extract_local_states alp
            in
			self#add_child nsol nobj;
			if lss = [] then
                trivial_nsols <- NodeSet.add nsol trivial_nsols
            else
                let register_ls ai =
                    let np = NodeLS ai
                    in
                    self#push_ls ai;
                    self#add_child np nsol
                in
                List.iter register_ls lss
		in
		List.iter2 register_sol isols sols;
		new_objs <- obj::new_objs

	method _push_ls (a,i) js =
		let np = NodeLS (a,i)
		in
		let register_init j =
			let obj = (a,j,i)
			in
			let nobj = NodeObj obj
			in
			(if not (self#has_obj obj) then self#push_obj obj nobj);
			self#add_child nobj np;
		in
		ISet.iter register_init js

	method push_ls (a,i) =
		if not (self#has_ls (a,i)) then (
			self#_push_ls (a,i) (ctx_get a current_ctx)
		)

	method increase_ctx procs =
		let is_new_proc p = not (ctx_has_ls p current_ctx)
		in
		let new_procs = LSSet.filter is_new_proc procs
		in
		if not (LSSet.is_empty new_procs) then (
			let ctx' = ctx_of_lsset new_procs
			in
			current_ctx <- ctx_union current_ctx ctx';
			(* re-init procs having sort in new_procs *)
			let reinit_proc (a,i) =
				try
					let is = ctx_get a ctx'
					in
					let is = ISet.remove i is
					in
					self#_push_ls (a,i) is
				with Not_found -> ()
			in
			LSSet.iter reinit_proc self#procs;
			self#commit ();
			true
		) else false

	method saturate_ctx =
		let procs = lcg_setup.saturate_procs self#ctx procs
		in
		let procs = lcg_setup.saturate_lss_by_nodes self#nodes procs
		in
		let procs = match pl with [ai] -> LSSet.remove ai procs | _ -> procs
		in
		if self#increase_ctx procs then
			self#saturate_ctx

end


(**
	Simple flooders
**)

(** gather all connected local states *)
let allprocs_flooder =
	let update_value n (ps, nm) = match n with
		  NodeSol _ | NodeObj _ -> union_value nm
		| NodeLS ai -> ctx_add_ls ai (union_value nm)
	in {
	equality = ctx_equal;
	node_init = default_flooder_node_init ctx_empty;
	update_cache = default_flooder_update_cache;
	update_value = update_value;
}

let top_localstates_flooder =
	let update_value n (ps, nm) = match n with
		  NodeSol _ | NodeObj _ -> union_value nm
		| NodeLS (a,i) -> IMap.add a (ISet.singleton i) (union_value nm)
	in {
	equality = ctx_equal;
	node_init = default_flooder_node_init ctx_empty;
	update_cache = default_flooder_update_cache;
	update_value = update_value;
}



(**
	Continuity
**)

let min_conts_flooder =
	let update_cache n v n' v' =
		match n, n' with
		  NodeSol _, NodeLS _
		| NodeObj _, NodeSol _
		| NodeLS _, NodeObj _ -> default_flooder_update_cache n v n' v'
		| NodeObj _, NodeObj _ -> v (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	and update_value n (ctx, nm) =
		match n with
		  NodeSol _ -> union_value nm
		| NodeObj (a,i,_) ->
			(** do not allow self-loops *)
			ctx_rm_ls (a,i) (inter_value nm)
		| NodeLS (a,i) ->
			let ctx' = inter_value nm
			in
			IMap.add a (ISet.singleton i) ctx'
	in {
	equality = ctx_equal;
	node_init = default_flooder_node_init ctx_empty;
	update_cache = update_cache;
	update_value = update_value;
}

let max_conts_flooder boolean_automata =
	let update_value n (ctx, nm) =
		let ctx = top_localstates_flooder.update_value n (ctx, nm)
		in
		match n with
		  NodeObj (a,i,_) ->
		  	if ISet.mem a boolean_automata then
				ctx_rm_ls (a,i) ctx
			else ctx
		| _ -> ctx
	in {
	equality = min_conts_flooder.equality;
	node_init = min_conts_flooder.node_init;
	update_cache = min_conts_flooder.update_cache;
	update_value = update_value;
}


(**
	GLC setups
**)

let default_lcg_setup = {
	conts_flooder = min_conts_flooder;
	conts = (fun _ (a,_,_) ctx -> try ctx_get a ctx with Not_found -> ISet.empty);
	saturate_procs = (fun _ a -> a);
	saturate_lss_by_nodes = (fun _ a -> a);
}


(**
    predefined LCGs
**)
let full_lcg ac an =
	let sols = abstract_local_paths ac an
	and ctx = full_ctx an
	in
	let goal = LSSet.elements (lsset_of_ctx ctx)
	in
	let lcg = new lcg default_lcg_setup an ctx goal sols
	in
	lcg#set_auto_conts false;
	lcg#build;
	lcg

let build_oa_lcg an ctx goal sols =
	let lcg = new lcg default_lcg_setup an ctx goal sols
	in
    lcg#build;
    lcg

