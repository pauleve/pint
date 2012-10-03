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

type node =
	  NodeObj of objective
	| NodeProc of process
	| NodeSol of (objective * PSet.t)

let string_of_node = function                                         
	  NodeSol (obj,ps) -> "Sol["^string_of_obj obj^"/"^string_of_procs ps^"]"
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

let string_of_nodeset = string_of_set string_of_node NodeSet.elements;;

exception Found

class graph = 
object(self)
	val edges = Hashtbl.create 1000
	val rev_edges = Hashtbl.create 1000

	val mutable nodes = NodeSet.empty

	val mutable procs = PSet.empty
	val mutable objs = ObjSet.empty

	method procs = procs
	method objs = objs
	method has_proc p = PSet.mem p procs
	method has_obj obj = ObjSet.mem obj objs
	method has_child c n = List.mem c (self#childs n)
	method count_procs = PSet.cardinal procs
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
			let solrels, contrels = List.partition (function NodeSol _ -> true |
								NodeObj _ -> false | _ -> 
									failwith "invalid graph (debug/register_obj)") rels
			in
			let sols = if solrels == [] then sols else 
				(sol^"Sol("^string_of_obj obj^") = [ "^
					(String.concat "; " (List.map (function NodeSol (_,ps) -> string_of_procs ps
												| _ -> failwith "invalid solrels") solrels))^" ]"^eol)::sols
			and conts = if contrels == [] then conts else
				(sol^"Cont("^string_of_obj obj^") = [ "^
					(String.concat "; " (List.map (function NodeObj obj -> string_of_obj obj
												| _ -> failwith "invalid contrels") contrels))^" ]"^eol)::conts
			in
			sols, conts
		in
		let reqs = List.map register_proc (PSet.elements procs)
		and sols, conts = List.fold_left register_obj ([],[]) (ObjSet.elements objs)
		in
		let sols, conts = List.rev sols, List.rev conts
		in
		let buf =
			 sol^"procs = "^string_of_procs procs^eol
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
				NodeSol (_,ps) ->
					let dsol = "pintsol"^string_of_int !solcounter
					in
					let def = dsol^"[label=\"\",shape=circle,fixedsize=true,width=0.1,height=0.1];\n"
					in
					let edges = dobj ^" -> "^dsol^";\n" 
						^ (String.concat "\n" (List.map (fun p -> dsol^" -> "^(dot_of_proc p)^";") (PSet.elements ps)))
					in
					solcounter := !solcounter + 1;
					def ^ edges
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
		let procs = List.map register_proc (PSet.elements procs)
		and objs = List.map register_obj (ObjSet.elements objs)
		in
		let content = String.concat "\n" procs ^ String.concat "\n" objs
		in
		"digraph { \n" ^ content ^ "}\n"

	method register_node = function
		  NodeProc p -> (procs <- PSet.add p procs)
		| NodeObj (a,i,j) -> 
				(procs <- PSet.add (a,j) procs;
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
		Hashtbl.add rev_edges c n
	
	method iter f = Hashtbl.iter f rev_edges

	method private _flood
		: 'a 'b. bool -> (node -> 'a * 'b) 
			-> (node -> 'a * 'b -> node -> 'a * 'b -> 'a * 'b) (* update_cache *)
			-> (node -> 'a * 'b-> 'a) (* update_value *)
			-> (node, 'a * 'b) Hashtbl.t -> NodeSet.t -> unit
		= fun desc init update_cache update_value values ns ->
		let _childs, _parents = if desc then (self#childs, self#parents)
										else (self#parents, self#childs)
		in
		let rec flood chgs = 
			(* 1. update cached values of childs *)
			let update_childs n (nexts, news) =
				let childs = _childs n
				and nv = Hashtbl.find values n
				in
				let forward_to (nexts, news) n' =
					let n'v, isnew = try Hashtbl.find values n', false
						with Not_found -> (init n', true)
					in
					let n'v = update_cache n' n'v n nv
					and news = if isnew then NodeSet.add n' news else news
					in
					Hashtbl.replace values n' n'v;
					(NodeSet.add n' nexts, news)
				in
				List.fold_left forward_to (nexts, news) childs
			in
			let nexts, news = NodeSet.fold update_childs chgs (NodeSet.empty, NodeSet.empty)
			in

			(* 2. compute child values *)
			let forward n chgs = 
				let (v, nm) = Hashtbl.find values n
				in
				let v' = update_value n (v,nm)
				in
				let changed = v' <> v
				and n'v = (v',nm)
				in
				let chgs = if changed then NodeSet.add n chgs else chgs
				in
				Hashtbl.replace values n n'v;
				chgs
			in
			let chgs = NodeSet.fold forward nexts news
			in
			if not (NodeSet.is_empty chgs) then (
				(*dbg ("_flood: "^string_of_int (NodeSet.cardinal chgs)^" changes");*)
				flood chgs
			)

		(*
		and setup n =
			let push n nv n' n'v =
				let nv' = update_cache n nv n' n'v
				in
				(update_value n nv', snd nv')
			in
			let forward v p =
				try 
					let pv = Hashtbl.find values p
					in
					push n v p pv
				with Not_found -> v
			in
			let v = List.fold_left forward (init n) (_parents n)
			in
			Hashtbl.add values n v
		in*)
		and setup n =
			Hashtbl.add values n (init n)
		in
		NodeSet.iter setup ns;
		flood ns

	method flood 
		: 'a 'b. (node -> 'a * 'b) 
			-> (node -> 'a * 'b -> node -> 'a * 'b -> 'a * 'b) (* update_cache *)
			-> (node -> 'a * 'b-> 'a) (* update_value *)
			-> (node, 'a * 'b) Hashtbl.t -> NodeSet.t -> unit
		= self#_flood true
	method rflood 
		: 'a 'b. (node -> 'a * 'b) 
			-> (node -> 'a * 'b -> node -> 'a * 'b -> 'a * 'b) (* update_cache *)
			-> (node -> 'a * 'b-> 'a) (* update_value *)
			-> (node, 'a * 'b) Hashtbl.t -> NodeSet.t -> unit
		= self#_flood false

	method rflood2
		: 'a 'b. ?reversed:bool -> ('a -> 'a -> bool) -> (node -> 'a * 'b NodeMap.t) 
			-> (node -> 'a * 'b NodeMap.t -> node -> 'a * 'b NodeMap.t -> 'a * 'b NodeMap.t) (* update_cache *)
			-> (node -> 'a * 'b NodeMap.t -> 'a) (* update_value *)
			-> (node, 'a * 'b NodeMap.t) Hashtbl.t -> NodeSet.t -> unit
		= fun ?reversed:(desc=false) equality init update_cache update_value values ns ->
		let _childs, _parents = if desc then (self#childs, self#parents)
										else (self#parents, self#childs)
		in

		(*
		let debug_node n =
			print_endline (string_of_node n^" -> "^
				String.concat " + " (List.map string_of_node (_childs n)))
		in
		NodeSet.iter debug_node nodes;
		*)

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
		prerr_endline (string_of_int default_id^" SCCs");

		let get_scc_id n =
			try Hashtbl.find sccs_id n with Not_found -> default_id
		in

		let pop src =
			let (i, n) = RankedNodeSet.min_elt src
			in
			prerr_string ("\r["^string_of_int i^"] ");
			let tail = RankedNodeSet.remove (i,n) src
			in
			n, tail
		in

		let rec flood (ready, news) = (
			let n, ready = pop ready
			in
			let isnew, news =
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
			let (ready, news) = if not changed then (ready, news) else 
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
				List.fold_left forward (ready, news) (_childs n)
			in
			if not (RankedNodeSet.is_empty ready) then
				flood (ready, news)
		) in
		let fold_n n ins =
			RankedNodeSet.add (get_scc_id n, n) ins
		in
		let ins = NodeSet.fold fold_n ns RankedNodeSet.empty
		in
		flood (ins, ns)

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
				scc::sccs
				(* if List.length scc > 1 then scc::sccs else sccs*)
			else sccs

		in
		let fold_node v sccs =
			if not(Hashtbl.mem index v) then strongconnect v sccs else sccs
		in
		NodeSet.fold fold_node leafs []

end;;

(**
   shared functions related to rflood algorithms 

   each node is associated to a couple
			(ctx, nm) 
		where nm is the cached value of childs
 **)
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
let update_cache n (v,nm) n' (v',_) =
	(v, NodeMap.add n' v' nm)
;;

let run_rflood update_cache update_value (gA : #graph) flood_values from_objs =
	let init n = update_value n (ctx_empty, NodeMap.empty), NodeMap.empty
	in
	let fold_obj ns obj = NodeSet.add (NodeObj obj) ns
	in
	let ns = List.fold_left fold_obj NodeSet.empty from_objs
	in
	gA#rflood init update_cache update_value flood_values ns
;;
(**  **)

let min_conts (gA : #graph) =
	let update_value n (ctx,nm) = match n with
		  NodeSol _ -> union_value nm
		| NodeObj _ -> inter_value nm
		| NodeProc (a,i) -> 
			let ctx' = inter_value nm
			in
			SMap.add a (ISet.singleton i) ctx'
	in
	(* the node n with value v receive update from node n' with value v' *)
	let push n v n' v' = (* if SMap.is_empty (fst v') then (v, false) else*)
		match n, n' with
		  NodeSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeProc _, NodeObj _ -> update_cache n v n' v'
		| NodeObj _, NodeObj _ -> v (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	in
	run_rflood push update_value gA
;;

let max_conts (gA : #graph) =
	let update_value n (ctx,nm) = match n with
		  NodeSol _ -> union_value nm
		| NodeObj _ -> union_value nm
		| NodeProc (a,i) -> 
			let ctx' = union_value nm
			in
			SMap.add a (ISet.singleton i) ctx'
	in
	(* the node n with value v receive update from node n' with value v' *)
	let push n v n' v' = (* if SMap.is_empty (fst v') then (v, false) else*)
		match n, n' with
		  NodeSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeProc _, NodeObj _ -> update_cache n v n' v'
		| NodeObj _, NodeObj _ -> v (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	in
	gA#debug ();
	run_rflood push update_value gA;
;;

let min_procs (gA : #graph) flood_values =
	let update_value n (ctx,nm) = match n with
		  NodeSol _ -> union_value nm

		| NodeObj (a,j,i) -> 
			let r1 = NodeMap.fold (function 
				  NodeSol _ -> (fun c1 -> function
									  None -> Some c1
									| Some c2 -> Some (ctx_inter c1 c2))
				| _ -> (fun _ c2 -> c2)) nm None
			in
			let r1 = Util.opt_default ctx_empty r1
			and r2 = NodeMap.fold (function 
				  NodeObj obj' ->
						let my_obj = (a,j,obj_bounce obj')
						in
						let ctx2 = fst (Hashtbl.find flood_values (NodeObj my_obj))
						in
						(fun c1 c2 -> ctx_union (ctx_union c1 c2) ctx2)
				| _ -> (fun _ c2 -> c2)) nm ctx_empty
			in
			let ctx' = ctx_union r1 r2
			in
			SMap.add a (ISet.union (ctx_safe_get a ctx') (ISet.singleton i)) ctx'

		| NodeProc _ -> inter_value nm (* TODO: ignore Obj without sols *)
	in
	run_rflood update_cache update_value gA flood_values
;;

(**
 * Key processes
 *)
module PSSet = Set.Make(struct type t = PSet.t 
		let compare e1 e2 = 
			let c = compare (PSet.cardinal e1) (PSet.cardinal e2)
			in
			if c == 0 then PSet.compare e1 e2 else c end);;

let psset_has_subset ps =
	PSSet.exists (fun ps' -> PSet.subset ps' ps)
;;

let psset_simplify pss = 
	let fold ps pss =
		if psset_has_subset ps pss then pss else PSSet.add ps pss
	in
	PSSet.fold fold pss PSSet.empty
;;

let psset_cross max_nkp pss1 pss2 =
	let inter = PSSet.inter pss1 pss2
	in
	let pss1 = PSSet.diff pss1 inter
	and pss2 = PSSet.diff pss2 inter
	in
	let filter_size pss =
		PSSet.filter (fun pss -> PSet.cardinal pss < max_nkp) pss
	in
	let pss1 = filter_size pss1
	and pss2 = filter_size pss2
	in
	let c1 = PSSet.cardinal pss1
	and c2 = PSSet.cardinal pss2
	in
	if c1 = 0 || c2 = 0 then
		inter
	else (
	let fold1 pss2 ps1 pss =
		let fold2 ps2 pss' =
			let ps = PSet.union ps2 ps1
			in
			if PSet.cardinal ps > max_nkp then 
				pss'
			else
				PSSet.add ps pss'
		in
		PSSet.fold fold2 pss2 pss
	in
	prerr_string ("<"^string_of_int c1^"x"^string_of_int c2);
	flush stderr;
	let r = PSSet.fold (fold1 pss2) pss1 inter
	in
	(*print_endline ("     -> "^string_of_int (PSSet.cardinal r)); *)
	
	let r = 
	psset_simplify r
	in
	prerr_string(">");
	flush stderr;
	(*print_endline ("     => "^string_of_int (PSSet.cardinal r));*)
	r)
;;

let key_procs (gA:#graph) max_nkp ignore_proc flood_values leafs =
	let psset_cross = psset_cross max_nkp
	in
	let nm_union nm =
		NodeMap.fold (fun _ -> PSSet.union) nm PSSet.empty
	and nm_cross nm =
		let r = NodeMap.fold (fun _ c1 -> function
			  None -> Some c1
			| Some c2 -> Some (psset_cross c1 c2)) nm None
		in
		match r with
		  None -> PSSet.empty
		| Some c -> c
	in
	let total_count = ref 0
	in
	let update_value n (_,nm) =
		total_count := !total_count + 1;
		match n with
		  NodeSol _ -> psset_simplify (nm_union nm)

		| NodeProc ai -> 
			let pss = nm_cross nm (* TODO: ignore Obj without sols *)
			in
			if ignore_proc ai then pss else
				let psai = PSet.singleton ai 
				in
				PSSet.add psai (PSSet.filter (fun ps -> not(PSet.mem ai ps)) pss)

		| NodeObj (a,j,i) -> (
			let r1 = NodeMap.fold (function 
				  NodeSol _ -> (fun c1 -> function
									  None -> Some c1
									| Some c2 -> Some (psset_cross c1 c2))
				| _ -> (fun _ c2 -> c2)) nm None
			in
			let r1 = Util.opt_default PSSet.empty r1
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
    let init n = PSSet.empty, NodeMap.empty
    in  
    gA#rflood2 PSSet.equal init update_cache update_value flood_values leafs;
	!total_count
;;


class cwA ctx pl get_Sols = object(self) inherit graph

	val mutable actual_ctx = ctx
	val mutable new_objs = []
	val mutable trivial_nsols = NodeSet.empty
	val mutable impossible_nobjs = NodeSet.empty
	method impossible_nobjs = impossible_nobjs
	method get_trivial_nsols () = trivial_nsols
	method get_leafs () = NodeSet.union trivial_nsols impossible_nobjs
	method has_impossible_objs = not (NodeSet.is_empty impossible_nobjs)
	method ctx = actual_ctx
	method get_impossible_objs = NodeSet.fold (function NodeObj obj -> fun objs -> obj::objs
													| _ -> fun objs -> objs) impossible_nobjs []
	
	method set_trivial_nsols t =
		trivial_nsols <- t

	method has_loops =
		List.exists (fun ai -> self#has_loop_from (NodeProc ai)) pl

	val mutable conts_flood = Hashtbl.create 50

	method conts = min_conts

	val mutable auto_conts = true
	method set_auto_conts t = auto_conts <- t
	method auto_conts = auto_conts

	method build = 
		List.iter self#init_proc pl;
		self#commit ()
	
	method commit () =
		(*self#debug ();*)
		if self#auto_conts then (
		dbg "Automatically pushing conts";
		(* update conts_flood with new objectives *)
		self#conts self conts_flood new_objs;
		(* we assume the minCont grows *)
		let register_cont obj = 
			let nobj = NodeObj obj
			in
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
			let ais = try ISet.remove i (ctx_get a ctx) 
				with Not_found -> ISet.empty
			in
			dbg ("cont("^string_of_obj (a,i,j)^")="
					^a^"_"^string_of_iset ais);(*^ " ("^string_of_ctx ctx^")");*)
			ISet.iter make_cont ais
		in
		let my_objs = new_objs
		in
		new_objs <- [];
		List.iter register_cont my_objs;
		if new_objs <> [] then self#commit ())

	method init_obj obj nobj =
		let aBS = get_Sols obj
		in
		if aBS == [] then impossible_nobjs <- NodeSet.add nobj impossible_nobjs;
		let register_sol ps =
			let nsol = NodeSol (obj, ps)
			in
			self#add_child nsol nobj;
			if PSet.is_empty ps then (trivial_nsols <- NodeSet.add nsol trivial_nsols);
			let register_proc p =
				let np = NodeProc p
				in
				self#init_proc p;
				self#add_child np nsol
			in
			PSet.iter register_proc ps;
		in
		List.iter register_sol aBS;
		new_objs <- obj::new_objs

	method _init_proc (a,i) js =
		let np = NodeProc (a,i)
		in
		let objs = ISet.fold (fun j objs -> (a,j,i)::objs) js []
		in
		List.iter (fun obj ->
			let nobj = NodeObj obj
			in
			self#add_child nobj np;
			self#init_obj obj nobj) objs

	method init_proc (a,i) =
		if not (self#has_proc (a,i)) then (
			self#_init_proc (a,i) (ctx_get a actual_ctx)
		)

	method increase_ctx procs =
		let is_new_proc p = not (ctx_has_proc p actual_ctx)
		in
		let new_procs = PSet.filter is_new_proc procs
		in
		if not (PSet.is_empty new_procs) then (
			let ctx' = procs_to_ctx new_procs
			in
			actual_ctx <- ctx_union actual_ctx ctx';
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
			PSet.iter reinit_proc self#procs;
			self#commit ();
			true
		) else false
	
	method saturate_ctx =
		if self#increase_ctx self#procs then self#saturate_ctx
	
end;;


class cwB ctx pl get_Sols = object(self) inherit (cwA ctx pl get_Sols)
	method conts = max_conts

	method analyse_impossible_objs ms_objs =
		prerr_endline ("multisols objs: "^string_of_set string_of_obj ObjSet.elements ms_objs);
		let im_nobjs = self#impossible_nobjs
		in
		(*
			rflood from impossible_nobjs:
				- colors parents
				- stop if parent is an ms_objs
				- failure if exists bounce(P) \in pl coloured and not in ms_objs 
				- fetch coloured ms_objs
		*)
		let is_ms_objs = function NodeObj obj -> ObjSet.mem obj ms_objs
			| _ -> false
		in
		(* the node n with value v receive update from node n' with value v' *)
		let update_cache n (coloured, is_ms) n' (coloured', is_ms') =
			if (not is_ms') && (not coloured) && coloured' then
				(true,is_ms)
			else
				(coloured,is_ms)
		and update_value n (coloured, is_ms) = coloured
		in
		let init n = if NodeSet.mem n im_nobjs then (true, false) else (false, is_ms_objs n)
		(*and push n v n' v' = (* if SMap.is_empty (fst v') then (v, false) else*)
			match n, n' with
			  NodeSol _, NodeProc _
			| NodeObj _, NodeSol _
			| NodeProc _, NodeObj _ 
			| NodeObj _, NodeObj _ -> (update n v n' v')
			| _ -> failwith "wrong abstract structure graph."
			*)
		and flood_values = Hashtbl.create 50
		in
		self#rflood init update_cache update_value flood_values im_nobjs;
		let get_responsibles n (coloured, is_ms) r =
			if coloured && is_ms then n::r 
			else (
				(if coloured then match n with
					    NodeObj obj -> if List.mem (obj_bounce_proc obj) pl then 
												failwith "ROOT is coloured!!"
					  | _ -> ());
				r
			)
		in
		let r = Hashtbl.fold get_responsibles flood_values []
		in
		prerr_endline ("Responsibles: "^String.concat ", " (List.map string_of_node r));
		List.map obj_from_node r
	
	method analyse_loop loop ms_objs =
		let r = List.filter (function NodeObj obj -> ObjSet.mem obj ms_objs | _ -> false) loop
		in
		prerr_endline ("Loop responsibles: "^String.concat ", " (List.map string_of_node r));
		List.map obj_from_node r
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

let empty_choices_queue () =
	let q = Queue.create ()
	in
	Queue.push ObjMap.empty q;
	q
;;

module ObjMapSet = Set.Make (struct type t = int ObjMap.t let compare = compare end)

class cwB_generator ctx pl get_Sols = object(self)
	val mutable has_next = true
	(*val queue = empty_choices_queue ()*)
	val mutable queue = [ObjMap.empty]
	val mutable current_choices = ObjMap.empty
	method has_next = queue <> [] (*not (Queue.is_empty queue)*)

	val mutable multisols_objs = ObjSet.empty
	method multisols_objs = multisols_objs

	val mutable known_choices = ObjMapSet.empty

	method get_Sols choices obj =
		let aBS = get_Sols obj
		in
		match aBS with [] | [_] -> aBS
		| _ -> (
			multisols_objs <- ObjSet.add obj multisols_objs;
			let sol =
				try
					let n = ObjMap.find obj choices
					in
					List.nth aBS n
				with Not_found -> (
					(*
					let register_choice n =
						let choices' = ObjMap.add obj n choices
						in
						self#push_choices choices'
					in
					List.iter register_choice (Util.range 1 (List.length aBS - 1)); *)
					List.hd aBS
				)
			in
			[sol]
		)
	
	method push_choices choices =
		if not (ObjMapSet.mem choices known_choices) then (
			known_choices <- ObjMapSet.add choices known_choices;
			prerr_endline ("::: pushing choices "^string_of_choices choices);
			queue <- choices::queue
			(*Queue.push choices queue*)
		) else
			dbg ("skip "^string_of_choices choices)

	method change_objs objs =
		let change_obj choices obj =
			try
				let n = ObjMap.find obj choices
				in
				(* TODO: handle overflow *)
				ObjMap.add obj (n+1) choices
			with Not_found ->
				ObjMap.add obj 1 choices
		in
		let next_choices = List.fold_left change_obj current_choices objs
		in
		self#push_choices next_choices

	method next =
		(*let choices = Queue.pop queue*)
		let choices = List.hd queue
		in
		prerr_endline ("::: playing choices "^string_of_choices choices);
		current_choices <- choices;
		queue <- List.tl queue;
		let gB = new cwB ctx pl (self#get_Sols choices)
		in 
		gB#build;
		gB#saturate_ctx;
		gB

end;;

