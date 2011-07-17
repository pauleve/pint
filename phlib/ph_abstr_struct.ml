(*
Copyright or © or Copr. Loïc Paulevé (2010-2012)

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
	  NodeSol (obj,_) -> "Sol("^string_of_obj obj^"]"
	| NodeObj obj -> "Obj["^string_of_obj obj^"]"
	| NodeProc p -> "Proc["^string_of_proc p^"]"
;;

module NodeOrd = struct type t = node let compare = compare end
module NodeSet = Set.Make (NodeOrd)
module NodeMap = Map.Make (NodeOrd)

class graph = 
object(self)
	val edges = Hashtbl.create 50
	val rev_edges = Hashtbl.create 50

	val mutable procs = PSet.empty
	val mutable objs = ObjSet.empty

	method procs = procs
	method objs = objs
	method has_proc p = PSet.mem p procs
	method has_obj obj = ObjSet.mem obj objs
	method has_child c n = List.mem c (self#childs n)
	method count_procs () = PSet.cardinal procs
	method count_objs () = ObjSet.cardinal objs

	method debug () = if !Debug.dodebug then (
		let sol = "#aS# "
		and eol = "\n"
		in
		let register_proc p = 
			let rels = Hashtbl.find_all edges (NodeProc p)
			in
			sol^"Req("^string_of_proc p^") = [ "^
				(String.concat "; " (List.map (function NodeObj obj -> string_of_obj obj
													| _ -> failwith "invalid graph") rels))^" ]"^eol
		and register_obj (sols,conts) obj =
			let rels = self#childs (NodeObj obj)
			in
			let solrels, contrels = List.partition (function NodeSol _ -> true |
								NodeObj _ -> false | _ -> failwith "invalid graph") rels
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
		in
		dbg buf)

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
		self#register_node n;
		self#register_node c;
		Hashtbl.add edges n c;
		Hashtbl.add rev_edges c n

	method private _flood
		: 'a. bool -> (node -> 'a) 
			-> (node -> 'a -> node -> 'a -> 'a * bool)
			-> (node, 'a) Hashtbl.t -> NodeSet.t -> unit
		= fun desc init push values ns ->
		let _childs, _parents = if desc then (self#childs, self#parents)
										else (self#parents, self#childs)
		in
		let rec flood chgs = 
			let forward n chgs = 
				(* forward the new value v of n to its childs *)
				let v = Hashtbl.find values n
				and nexts = _childs n
				in
				let forward_to chgs n' =
					let n'v, isnew = try Hashtbl.find values n', false
						with Not_found -> (init n', true)
					in
					let n'v, changed = push n' n'v n v
					in
					let chgs = if changed || isnew then
							NodeSet.add n' chgs else chgs
					in
					Hashtbl.replace values n' n'v;
					chgs
				in
				List.fold_left forward_to chgs nexts
			in
			let chgs = NodeSet.fold forward chgs NodeSet.empty
			in
			if not (NodeSet.is_empty chgs) then flood chgs

		and setup n =
			let forward v p =
				try 
					let pv = Hashtbl.find values p
					in
					fst (push n v p pv)
				with Not_found -> v
			in
			let v = List.fold_left forward (init n) (_parents n)
			in
			Hashtbl.add values n v

		in
		NodeSet.iter setup ns;
		flood ns

	method flood 
		: 'a. (node -> 'a) 
			-> (node -> 'a -> node -> 'a -> 'a * bool)
			-> (node, 'a) Hashtbl.t -> NodeSet.t -> unit
		= self#_flood true
	method rflood 
		: 'a. (node -> 'a) 
			-> (node -> 'a -> node -> 'a -> 'a * bool)
			-> (node, 'a) Hashtbl.t -> NodeSet.t -> unit
		= self#_flood false

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
let update update_value n (v,nm) n' (v',_) =
	(* 1. update cache map *)
	let nm = NodeMap.add n' v' nm
	in
	(* 2. update value *)
	let v' = update_value n (v,nm)
	in
	(v',nm), v<>v'
;;
let run_rflood update_value push (gA : #graph) flood_values from_objs =
	let init n = update_value n (ctx_empty, NodeMap.empty), NodeMap.empty
	in
	let fold_obj ns obj = NodeSet.add (NodeObj obj) ns
	in
	let ns = List.fold_left fold_obj NodeSet.empty from_objs
	in
	gA#rflood init push flood_values ns
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
	let update = update update_value
	in
	(* the node n with value v receive update from node n' with value v' *)
	let push n v n' v' = (* if SMap.is_empty (fst v') then (v, false) else*)
		match n, n' with
		  NodeSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeProc _, NodeObj _ -> (update n v n' v')
		| NodeObj _, NodeObj _ -> (v, false) (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	in
	run_rflood update_value push gA
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
	let update = update update_value
	in
	(* the node n with value v receive update from node n' with value v' *)
	let push n v n' v' = (* if SMap.is_empty (fst v') then (v, false) else*)
		match n, n' with
		  NodeSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeProc _, NodeObj _ -> (update n v n' v')
		| NodeObj _, NodeObj _ -> (v, false) (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	in
	run_rflood update_value push gA
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
	let update = update update_value
	in
	(* the node n with value v receive update from node n' with value v' *)
	let push n v n' v' = (* if SMap.is_empty (fst v') then (v, false) else*)
		match n, n' with
		  NodeSol _, NodeProc _
		| NodeObj _, NodeSol _
		| NodeProc _, NodeObj _ 
		| NodeObj _, NodeObj _ -> (update n v n' v')
		| _ -> failwith "wrong abstract structure graph."
	in
	run_rflood update_value push gA flood_values
;;


class cwA ctx w get_Sols = object(self) inherit graph

	val mutable new_objs = []
	val mutable trivial_nsols = NodeSet.empty
	val mutable impossible_nobjs = NodeSet.empty
	method get_trivial_nsols () = trivial_nsols
	method get_leafs () = NodeSet.union trivial_nsols impossible_nobjs

	val mutable conts_flood = Hashtbl.create 50

	method conts = min_conts
	
	method commit () =
		self#debug ();
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
			dbg ("minCONT("^string_of_obj (a,i,j)^")="
					^a^"_"^string_of_iset ais^ " ("^string_of_ctx ctx^")");
			ISet.iter make_cont ais
		in
		let my_objs = new_objs
		in
		new_objs <- [];
		List.iter register_cont my_objs;
		if new_objs <> [] then self#commit ()

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

	method init_proc (a,i) =
		if not (self#has_proc (a,i)) then (
			let np = NodeProc (a,i)
			in
			let objs = ISet.fold (fun j objs -> (a,j,i)::objs) (ctx_get a ctx) []
			in
			List.iter (fun obj ->
				let nobj = NodeObj obj
				in
				self#add_child nobj np;
				self#init_obj obj nobj) objs
		)
end;;


class cwB ctx w get_Sols = object(self) inherit (cwA ctx w get_Sols)
	method conts = max_conts
end;;

class cwB_generator ctx w get_Sols = object(self)

	val mutable has_next = false
	method has_next = has_next

	method init =
		(* TODO *)
		()

	method next =
		(* TODO *)
		()
end;;

