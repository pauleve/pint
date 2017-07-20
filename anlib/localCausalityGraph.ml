
open Debug;;

open PintTypes;;
open AutomataNetwork;;
open Ph_types;;

type node =
	  NodeProc of process
	| NodeSol of (objective * LSSet.t * ISet.t)
	| NodeSyncSol of (objective * StateSet.t * ISet.t)
	| NodeObj of objective

let string_of_node = function
	  NodeSol (obj,ps,interm) ->
	  	"Sol["^string_of_obj obj^"/"^string_of_lsset ps
						^" via "^string_of_iset interm^"]"
	| NodeSyncSol (obj,states,interm) ->
	  	"SyncSol["^string_of_obj obj^"/"
			^string_of_set string_of_state StateSet.elements states
					^" via "^string_of_iset interm^"]"
	| NodeObj obj -> "Obj["^string_of_obj obj^"]"
	| NodeProc p -> "Proc["^string_of_proc p^"]"
;;

let obj_from_node = function
	  NodeObj obj -> obj
	| _ -> raise (Invalid_argument "obj_from_node")
;;

module NodeOrd = struct type t = node let compare = compare end
module NodeSet = Set.Make (NodeOrd)
module NodeMap = Map.Make (NodeOrd)

module RankedNodeSet = Set.Make(struct type t = int * node let compare = compare end)

let rec nodeset_of_list = function [] -> NodeSet.empty
							| h::t -> NodeSet.add h (nodeset_of_list t);;

let string_of_nodeset = string_of_set string_of_node NodeSet.elements;;

exception Found


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
		where nm is the cached value of childs
 **)

let default_flooder_node_init empty_value = 
	fun n -> empty_value, NodeMap.empty
;;
let default_flooder_update_cache n (v,nm) n' (v',_) =
	(v, NodeMap.add n' v' nm)
;;

let union_value nm =
	NodeMap.fold (fun _ -> ctx_union) nm ctx_empty
;;
let inter_value nm =
		let r = NodeMap.fold (fun _ c1 -> function
			  None -> Some c1
			| Some c2 -> Some (ctx_inter c1 c2)) nm None
		in
		match r with
		  None -> ctx_empty
		| Some c -> c
;;




(**
	generic graph
*)

class graph =
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
	method has_proc p = LSSet.mem p procs
	method has_obj obj = ObjSet.mem obj objs
	method has_child c n = List.mem c (self#childs n)
	method count_procs = LSSet.cardinal procs
	method count_objs = ObjSet.cardinal objs
	method count_nodes = NodeSet.cardinal nodes

	method debug () = if !Debug.dodebug then (
		let sol = "#aS# "
		and eol = "\n"
		in
		let register_proc p =
			let rels = Hashtbl.find_all edges (NodeProc p)
			in
			sol^"Req("^string_of_proc p^") = [ "^
				(String.concat "; " (List.map (function NodeObj obj -> string_of_obj obj
						| _ -> failwith "invalid graph (debug/register_proc)") rels))^" ]"^eol
		and register_obj (sols,conts) obj =
			let rels = self#childs (NodeObj obj)
			in
			let solrels, contrels = List.partition (function
							NodeSol _ | NodeSyncSol _ -> true
							| NodeObj _ -> false | _ ->
								failwith "invalid graph (debug/register_obj)") rels
			in
			let sols = if solrels == [] then sols else
				(sol^"Sol("^string_of_obj obj^") = [ "^
					(String.concat "; " (List.map string_of_node solrels))^" ]"^eol)::sols
			and conts = if contrels == [] then conts else
				(sol^"Cont("^string_of_obj obj^") = [ "^
					(String.concat "; " (List.map string_of_node contrels))^" ]"^eol)::conts
			in
			sols, conts
		in
		let reqs = List.map register_proc (LSSet.elements procs)
		and sols, conts = List.fold_left register_obj ([],[]) (ObjSet.elements objs)
		in
		let sols, conts = List.rev sols, List.rev conts
		in
		let buf =
			 sol^"procs = "^string_of_lsset procs^eol
			^sol^"objs = "^(string_of_set string_of_obj ObjSet.elements objs)^eol
			^(String.concat "" reqs)
			^(String.concat "" sols)
			^(String.concat "" conts)
			^sol^"procs count = "^string_of_int self#count_procs
				^"; objs count = "^string_of_int self#count_objs
		in
		dbg buf)

	method to_dot =
		let dot_of_obj (a,i,j) =
			"O_"^a^"_"^string_of_int i ^"_"^string_of_int j
		and dot_of_proc p = string_of_proc  p
		in
		let solcounter = ref 0
		and synccounter = ref 0
		in
		let dot_of_sol () =
			incr solcounter;
			let node = "pintsol"^string_of_int !solcounter
			in
			let dot = node^"[label=\"\",shape=circle,fixedsize=true,width=0.1,height=0.1];\n"
			in
			(node, dot)
		and dot_of_sync state =
			if SMap.cardinal state >= 2 then (
				incr synccounter;
				let node = "pintsync"^string_of_int !synccounter
				in
				let dot = node^"[label=\"\",shape=square,fixedsize=true,width=0.1,height=0.1];\n"
				in
				let dot = dot ^ String.concat "" (List.map (fun p ->
						node^" -> "^(dot_of_proc p)^";\n") (SMap.bindings state))
				in
				node, dot)
			else
				let dproc = dot_of_proc (SMap.min_binding state)
				in
				dproc, ""
		in

		let register_proc p =
			let dproc = dot_of_proc p
			in
			let dot_of_rel = function
				NodeObj obj -> dproc ^ " -> " ^ dot_of_obj obj ^";"
				| _ -> failwith "invalid graph (to_dot/register_proc)"
			in
			let rels = Hashtbl.find_all edges (NodeProc p)
			in
			let def = dproc^"[shape=box];\n"
			and edges = String.concat "\n" (List.map dot_of_rel rels)
			in
			def ^ edges ^ "\n"

		and register_obj obj =
			let dobj = dot_of_obj obj
			in
			let dot_of_rel = function
				NodeSol (_,ps,_) ->
					let dsol, def = dot_of_sol ()
					in
					let edges = dobj ^" -> "^dsol^";\n" 
						^ (String.concat "\n" (List.map (fun p -> dsol^" -> "^(dot_of_proc p)^";") (LSSet.elements ps)))
					in
					def ^ edges
				| NodeSyncSol (_,states,_) ->
					let dsol, def = dot_of_sol ()
					in
					let dsyncs, defs = List.split
						(List.map dot_of_sync (StateSet.elements states))
					in
					def
					^ dobj ^" -> "^dsol^";\n"
					^ (String.concat "" defs)
					^ String.concat "" (List.map (fun dsync ->
						dsol^" -> "^dsync^";\n") dsyncs)
				| NodeObj obj' -> dobj ^" -> "^dot_of_obj obj'^";"
				| _ -> failwith "invalid graph (to_dot/register_obj)"
			in
			let rels = self#childs (NodeObj obj)
			in
			let def = dobj^"[label=\""^string_of_obj obj^"\"];\n"
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
		  NodeProc p -> (procs <- LSSet.add p procs;
		  				all_procs <- LSSet.add p all_procs)
		| NodeObj (a,i,j) -> 
				(procs <- LSSet.add (a,j) procs;
				all_procs <- LSSet.add (a,j) (LSSet.add (a,i) all_procs);
				objs <- ObjSet.add (a,i,j) objs)
		| _ -> ()

	method childs n =
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
		  NodeProc p -> procs <- LSSet.remove p procs
		| NodeObj (a,i,j) -> 
			(if not (NodeSet.mem (NodeProc (a,j)) nodes) then
				procs <- LSSet.remove (a,j) procs);
			objs <- ObjSet.remove (a,i,j) objs
		| _ -> ());
		let cs = self#childs n
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
		let parents, childs = if desc then (self#childs, self#parents)
										else (self#parents, self#childs)
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

				(* update cached values of childs *)
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

	val mutable last_loop = []
	method last_loop = last_loop
	method has_loop_from n =
		let rec forward (visited,path) n =
			if NodeSet.mem n visited then (
				let rec pop_until = function [] -> []
					| m::q -> if n = m then n::q else pop_until q
				in
				last_loop <- pop_until (List.rev path);
				raise Found
			);
			let visited = NodeSet.add n visited
			and path' = n::path
			in
			List.iter (forward (visited,path')) (self#childs n)
		in
		try
			forward (NodeSet.empty, []) n;
			false
		with Found -> true

	method tarjan_SCCs desc leafs =
		let get_childs = if desc then self#childs else self#parents
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
			let sccs = List.fold_left handle_child sccs (get_childs v)
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

type _concrete_ph = {
	lasthitters: ?filter:(action list -> bool) -> objective -> LSSet.t;
	process_cond: process -> state list;
}

type glc_setup = {
	conts_flooder: (ctx, ctx NodeMap.t) flooder_setup;
	conts: ctx -> objective -> ctx -> ISet.t;
	saturate_procs: ctx -> LSSet.t -> LSSet.t;
	saturate_procs_by_objs: ObjSet.t -> LSSet.t -> LSSet.t;
}

class ['a] glc glc_setup ctx pl (*concrete_ph*)
(get_Sols:Ph_types.objective -> ('a * PintTypes.ISet.t) list)
(make_sol:Ph_types.objective -> 'a * ISet.t -> LSSet.t * node)
= object(self) inherit graph as g

	method setup = glc_setup

	val mutable current_ctx = ctx
	val mutable new_objs = []
	val mutable trivial_nsols = NodeSet.empty
	val mutable impossible_nobjs = NodeSet.empty
	method impossible_nobjs = impossible_nobjs
	method get_trivial_nsols () = trivial_nsols
	method leafs = NodeSet.union trivial_nsols impossible_nobjs
	method has_impossible_objs = not (NodeSet.is_empty impossible_nobjs)
	method ctx = current_ctx
	method get_impossible_objs = NodeSet.fold (function NodeObj obj -> fun objs -> obj::objs
													| _ -> fun objs -> objs) impossible_nobjs []
	method extract_sols =
		let register_node = function
			  NodeSol sol -> fun sols -> sol::sols
			| NodeSyncSol _ -> failwith "glc#register_node not implemented with NodeSyncSol"
			| _ -> fun sols -> sols
		in
		NodeSet.fold register_node nodes []

	method set_trivial_nsols t =
		trivial_nsols <- t

	method has_loops =
		if not (NodeSet.is_empty self_loops) then (
			last_loop <- [NodeSet.choose self_loops];
			true
		) else
		let ns = nodeset_of_list (List.map (fun p -> NodeProc p) pl)
		in
		let sccs = self#tarjan_SCCs true ns
		in
		List.exists (function 
			[] | [_] -> false | scc -> (last_loop <- scc; true)) sccs

	val mutable auto_conts = true
	method set_auto_conts t = auto_conts <- t
	method auto_conts = auto_conts

	method conts_flooder = self#call_rflood glc_setup.conts_flooder
	method conts = glc_setup.conts self#ctx

	method build = 
		List.iter self#init_proc pl;
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
					(if not (self#has_obj obj') then self#init_obj obj' nto);
					if not (self#has_child nto nobj) then
						self#add_child nto nobj
				in
				let ais = self#conts obj ctx
				in
				let ais = ISet.remove j ais
				in
				dbg ("cont("^string_of_obj (a,i,j)^")="
						^a^"_"^string_of_iset ais);(*^ " ("^string_of_ctx ctx^")");*)
				ISet.iter make_cont ais
			with Not_found -> ()
		in
		let my_objs = new_objs
		in
		if Hashtbl.length conts_flood > 0 then (
		new_objs <- [];
		List.iter register_cont my_objs;
		if new_objs <> [] then self#commit ()))

	method init_obj obj nobj =
		let aBS = List.map (make_sol obj) (get_Sols obj)
		in
		if aBS == [] then impossible_nobjs <- NodeSet.add nobj impossible_nobjs;
		let register_sol (ps, nsol) =
			self#add_child nsol nobj;
			if LSSet.is_empty ps then (trivial_nsols <- NodeSet.add nsol trivial_nsols);
			let register_proc p =
				let np = NodeProc p
				in
				self#init_proc p;
				self#add_child np nsol
			in
			LSSet.iter register_proc ps
		in
		List.iter register_sol aBS;
		new_objs <- obj::new_objs

	method _init_proc (a,i) js =
		let np = NodeProc (a,i)
		in
		let register_init j =
			let obj = (a,j,i)
			in
			let nobj = NodeObj obj
			in
			(if not (self#has_obj obj) then self#init_obj obj nobj);
			self#add_child nobj np;
		in
		ISet.iter register_init js

	method init_proc (a,i) =
		if not (self#has_proc (a,i)) then (
			self#_init_proc (a,i) (ctx_get a current_ctx)
		)

	method increase_ctx procs =
		let is_new_proc p = not (ctx_has_proc p current_ctx)
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
					self#_init_proc (a,i) is
				with Not_found -> ()
			in
			LSSet.iter reinit_proc self#procs;
			self#commit ();
			true
		) else false

	method saturate_ctx =
		let procs = glc_setup.saturate_procs self#ctx procs
		in
		let procs = glc_setup.saturate_procs_by_objs self#objs procs
		in
		let procs = match pl with [ai] -> LSSet.remove ai procs | _ -> procs
		in
		if self#increase_ctx procs then
			self#saturate_ctx

	method ancestors candidates objs =
		let sources = ObjSet.fold (fun obj -> NodeSet.add (NodeObj obj)) objs NodeSet.empty
		in
		let is_candidate = function NodeObj obj -> ObjSet.mem obj candidates | _ -> false
		and is_source n = NodeSet.mem n sources
		in
		let init n = if is_source n then (true, false) else (false, is_candidate n)
		and update_cache n (coloured, is_ms) n' (coloured', is_ms') =
			if (not is_ms') && (not coloured) && coloured' then
				(true,is_ms)
			else
				(coloured,is_ms)
		and update_value n (coloured, is_ms) = coloured
		in
		let flood_values = self#rflood (=) init update_cache update_value sources
		in
		let get_responsibles n (coloured, is_ms) r =
			if coloured && is_ms then ObjSet.add (obj_from_node n) r else r
		in
		let r = Hashtbl.fold get_responsibles flood_values ObjSet.empty
		in
		dbg ("Ancestors: "^String.concat ", " (List.map string_of_obj (ObjSet.elements r)));
		r
	

	method first_objs_may_avoid_nodes ms_objs bad_nodes =
		(** return objectives in ms_objs that may avoid the given nodes *)
		(* algorithm:
			rflood from nodes:
			- colors parents
			- TODO: really? failure if exists bounce(P) \in pl coloured and not in ms_objs 
			- fetch coloured ms_objs
			TODO: improve by checking there exists a choice actually avoidings the nodes
		*)
		dbg ("multisols objs: "^string_of_set string_of_obj ObjSet.elements ms_objs);
		let is_ms_objs = function NodeObj obj -> ObjSet.mem obj ms_objs | _ -> false
		in
		(* the node n with value v receive update from node n' with value v' *)
		let update_cache n (coloured, is_ms) n' (coloured', is_ms') =
			if (not is_ms') && (not coloured) && coloured' then
				(true,is_ms)
			else
				(coloured,is_ms)
		and update_value n (coloured, is_ms) = coloured
		in
		let init n = (NodeSet.mem n bad_nodes, is_ms_objs n)
		in
		let flood_values = self#rflood (=) init update_cache update_value bad_nodes
		in
		let get_responsibles n (coloured, is_ms) r =
			if coloured && is_ms then ObjSet.add (obj_from_node n) r else r 
		in
		Hashtbl.fold get_responsibles flood_values ObjSet.empty

	method objs_may_avoid_nodes ms_objs bad_nodes =
		let rec push_ancestors ms_objs objs =
			let ms_objs = ObjSet.diff ms_objs objs
			in
			let objs = self#ancestors ms_objs objs
			in
			if not (ObjSet.is_empty objs) then ( 
				objs::push_ancestors ms_objs objs;
			) else [objs]
		in
		let objs = self#first_objs_may_avoid_nodes ms_objs bad_nodes
		in
		objs::push_ancestors ms_objs objs

	method avoid_impossible_objs ms_objs =
		self#objs_may_avoid_nodes ms_objs self#impossible_nobjs
	
	method avoid_loop ms_objs loop =
		let loop = nodeset_of_list loop
		in
		let objs0 = NodeSet.fold (fun n objs -> match n with
									  NodeObj obj -> if ObjSet.mem obj ms_objs 
														  then ObjSet.add obj objs
														  else objs
									| _ -> objs) loop ObjSet.empty
		in
		objs0::self#objs_may_avoid_nodes ms_objs loop
	
	(* TODO - restore
	method lastprocs aj =
		dbg ("/lastprocs "^string_of_proc aj^"/");
		let visited_nodes = ref NodeSet.empty
		in
		let rec admissible_sols n =
			if NodeSet.mem n !visited_nodes then
				[]
			else (
				visited_nodes := NodeSet.add n !visited_nodes;
				match n with
				  NodeSol (_, ps,_) -> [ps]
				| NodeObj _ ->
					let nodes = self#childs n
					in
					List.flatten (List.map admissible_sols nodes)
				| _ -> []
			)
		in
		let asols = List.flatten (List.map admissible_sols 
									(self#childs (NodeProc aj)))
		and a = fst aj
		in
		dbg ("  admissible solutions: "^
			String.concat " ; " (List.map string_of_procs asols));
		let fold_hitter ps action =
			let bj = hitter action
			in
			if fst bj <> a then LSSet.add bj ps else ps
		in
		let filter_actions actions =
			let sol = List.fold_left fold_hitter LSSet.empty actions
			in
			List.exists (LSSet.equal sol) asols
		in
		let fold_objectives n ps = match n with 
			  NodeObj obj ->
			  	let lhs = concrete_ph.lasthitters ~filter:filter_actions obj
				in
			  	LSSet.union ps lhs
			| _ -> ps
		in
		let lhs = NodeSet.fold fold_objectives !visited_nodes LSSet.empty
		in
		let merge_cond ps state =
			let state = merge_state_with_ps state ps
			in
			procs_of_state state
		in
		let fold_hitter ai lps =
			let conds = concrete_ph.process_cond ai
			in
			List.map (merge_cond [ai;aj]) conds @ lps
		in
		LSSet.fold fold_hitter lhs []
		*)

end;;


let string_of_choices choices =
	let string_of_choice obj n =
		string_of_obj obj ^"#"^string_of_int n
	in
	let fold_choice obj n acc =
		(string_of_choice obj n)::acc
	in
	let acc = ObjMap.fold fold_choice choices []
	in
	"<"^(String.concat "; " acc)^">"
;;

module ObjMapSet = Set.Make (struct type t = int ObjMap.t let compare = ObjMap.compare compare end)

class ['a] lcg_generator lcg_setup ctx pl (*concrete_ph*)
(get_Sols:Ph_types.objective -> ('a * PintTypes.ISet.t) list)
(make_sol:Ph_types.objective -> 'a * ISet.t -> LSSet.t * node)
=
	let queue0 =
		let stack = Stack.create ();
		in
		Stack.push ObjMap.empty stack;
		stack
	in

	object(self)

	val queue = queue0
	method has_next = not(Stack.is_empty queue)

	val mutable current_choices = ObjMap.empty

	val mutable multisols_objs = ObjSet.empty
	method multisols_objs = multisols_objs

	val mutable known_choices = ObjMapSet.add ObjMap.empty ObjMapSet.empty

	method get_Sols choices obj =
		let aBS = get_Sols obj
		in
		match aBS with [] | [_] -> aBS
		| _ -> (
			multisols_objs <- ObjSet.add obj multisols_objs;
			(try
				let n = ObjMap.find obj choices
				in
				List.nth aBS n
			with Not_found ->
				List.hd aBS
			)::[]
		)

	method push_choices choices =
		if not (ObjMapSet.mem choices known_choices) then (
			known_choices <- ObjMapSet.add choices known_choices;
			dbg ~level:1 ("::: pushing choices "^string_of_choices choices);
			Stack.push choices queue
		) else
			dbg ~level:1 ("skip "^string_of_choices choices)

	method change_objs objs =
		let change_obj choices obj =
			try
				let n = ObjMap.find obj choices
				in
				(* TODO: handle overflow *)
				ObjMap.add obj ((n+1) mod List.length (get_Sols obj)) choices
			with Not_found ->
				ObjMap.add obj 1 choices
		in
		let next_choices = List.fold_left change_obj current_choices objs
		in
		self#push_choices next_choices

	method next =
		let choices = Stack.pop queue
		in
		dbg ~level:1 ("::: playing choices "^string_of_choices choices);
		current_choices <- choices;
		let gB = new glc lcg_setup ctx pl (self#get_Sols choices) make_sol
		in
		gB#build;
		gB#saturate_ctx;
		gB

end

(**
	Simple flooders
**)

(** gather all connected local states *)
let allprocs_flooder =
	let update_value n (ps, nm) = match n with
		  NodeSol _ | NodeSyncSol _ | NodeObj _ -> union_value nm
		| NodeProc ai -> ctx_add_proc ai (union_value nm)
	in {
	equality = ctx_equal;
	node_init = default_flooder_node_init ctx_empty;
	update_cache = default_flooder_update_cache;
	update_value = update_value;
}

let top_localstates_flooder =
	let update_value n (ps, nm) = match n with
		  NodeSol _ | NodeSyncSol _ | NodeObj _ -> union_value nm
		| NodeProc (a,i) -> SMap.add a (ISet.singleton i) (union_value nm)
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
		  NodeSol _, NodeProc _
		| NodeSyncSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeObj _, NodeSyncSol _
		| NodeProc _, NodeObj _ -> default_flooder_update_cache n v n' v'
		| NodeObj _, NodeObj _ -> v (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	and update_value n (ctx, nm) =
		match n with
		  NodeSol _ -> union_value nm
		| NodeSyncSol _ -> union_value nm
		| NodeObj (a,i,_) ->
			(** do not allow self-loops *)
			ctx_rm_proc (a,i) (inter_value nm)
		| NodeProc (a,i) ->
			let ctx' = inter_value nm
			in
			SMap.add a (ISet.singleton i) ctx'
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
		  	if SSet.mem a boolean_automata then
				ctx_rm_proc (a,i) ctx
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

let oa_glc_setup = {
	conts_flooder = min_conts_flooder;
	conts = (fun _ (a,_,_) ctx -> try ctx_get a ctx with Not_found -> ISet.empty);
	saturate_procs = (fun _ a -> a);
	saturate_procs_by_objs = (fun _ a -> a);
};;

let make_unord_unsync_sol obj (tr,interm) =
	tr,
	NodeSol (obj, tr, interm)

let make_unord_sol obj (tr,interm) =
	An_localpaths.UnordTrace.abstr tr,
	NodeSyncSol (obj, tr, interm)



(**
    predefined LCGs
**)
let full_lcg an =
	let cache = An_localpaths.create_cache ()
	in
	let sols = An_localpaths.MinUnordSol.solutions cache an
	and ctx = full_ctx an
	in
	let goal = PSet.elements (procs_of_ctx ctx)
	in
	let lcg = new glc oa_glc_setup ctx goal sols make_unord_sol
	in
	lcg#set_auto_conts false;
	lcg#build;
	lcg
