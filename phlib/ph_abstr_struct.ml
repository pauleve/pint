
open Debug;;

open Ph_types;;

type node =
	  NodeObj of objective
	| NodeProc of process
	| NodeSol of (objective * PSet.t)

module NodeOrd = struct type t = node let compare = compare end
module NodeSet = Set.Make (NodeOrd)
module NodeMap = Map.Make (NodeOrd)

class graph = 
object(self)
	val mutable edges = NodeMap.empty
	val mutable rev_edges = NodeMap.empty

	method add_child n1 n2 =
		let n1c = try NodeMap.find n1 edges
				with Not_found -> []
		and n2p = try NodeMap.find n2 rev_edges
				with Not_found -> []
		in
		(edges <- NodeMap.add n1 (n2::n1c) edges;
		rev_edges <- NodeMap.add n2 (n1::n2p) rev_edges)

	method private _flood
		: 'a. bool -> (NodeMap.key -> 'a) 
			-> (NodeMap.key -> 'a -> NodeMap.key -> 'a -> 'a * bool)
			-> NodeSet.t -> 'a NodeMap.t
		= fun desc init push ns ->
		let edges = if desc then edges else rev_edges
		in
		let rec flood chgs values = 
			let forward n (chgs, values) = 
				(* forward the new value v of n to its childs *)
				let v = NodeMap.find n values
				in
				let forward_to (chgs, values) n' =
					let n'v, isnew = try NodeMap.find n' values, false
						with Not_found -> (init n', true)
					in
					let n'v, changed = push n' n'v n v
					in
					let values = NodeMap.add n' n'v values
					and chgs = if changed || isnew then
							NodeSet.add n' chgs else chgs
					in
					chgs, values
				in
				let childs = try NodeMap.find n edges
						with Not_found -> []
				in
				List.fold_left forward_to (chgs, values) childs
			in
			let chgs, values = NodeSet.fold forward chgs (NodeSet.empty, values)
			in
			if NodeSet.is_empty chgs then values else flood chgs values

		and setup n values =
			let v = init n
			in
			NodeMap.add n v values

		in
		let values = NodeSet.fold setup ns NodeMap.empty
		in
		flood ns values
	
	method flood 
		: 'a. (NodeMap.key -> 'a) 
			-> (NodeMap.key -> 'a -> NodeMap.key -> 'a -> 'a * bool)
			-> NodeSet.t -> 'a NodeMap.t
		= self#_flood true
	method rflood 
		: 'a. (NodeMap.key -> 'a) 
			-> (NodeMap.key -> 'a -> NodeMap.key -> 'a -> 'a * bool)
			-> NodeSet.t -> 'a NodeMap.t
		= self#_flood false

end;;

let parents_sorts (gaS : #graph) objs =
	let init = function
		  NodeSol ((a,_,_),_) -> SSet.singleton a
		| _ -> SSet.empty
	and push n v n' v' =
		let v' = SSet.union v v'
		in
		v', v <> v'
	in
	let fold_obj obj ns = NodeSet.add (NodeObj obj) ns
	in
	let ns = ObjSet.fold fold_obj objs NodeSet.empty
	in
	let values = gaS#flood init push ns
	in
	let dbg_val n v = match n with
		  NodeObj obj -> dbg ("parentsSorts("^string_of_obj obj^")="
		  		^ SSet.fold (fun a buf -> buf^a^" ") v "")
		| _ -> ()
	in
	NodeMap.iter dbg_val values
;;

let min_cont (gaS : #graph) objs =
	let init = function
		  NodeProc (a,i) -> SMap.add a (PSet.singleton (a,i), NodeMap.empty) SMap.empty
		| _ -> SMap.empty
	and push n v n' v' = if SMap.is_empty v' then (v, false) else
		match n, n' with
		  NodeSol _, NodeProc _ ->
		  	(* union between childs *)
			let merge a (ps,_) my_v =
				let ps' = try fst (SMap.find a my_v) with Not_found -> PSet.empty
				in
				SMap.add a (PSet.union ps ps', NodeMap.empty) my_v
			in
			let my_v = SMap.fold merge v' v
			in
			my_v, my_v <> v

		| NodeObj _, NodeSol _ | NodeProc _, NodeObj _ ->
			(* intersection between childs *)
			let ignored = match n with NodeProc (b,_) -> b | _ -> ""
			in
			let merge a (ps,_) (my_v, changed) =
				if a = ignored then (my_v, changed) else (
				let my_ps, cache = try SMap.find a my_v
					with Not_found -> PSet.empty, NodeMap.empty
				in
				let cache = NodeMap.add n' ps cache
				in
				let build_ps _ ps = function
					  None -> Some ps
					| Some my_ps' -> Some (PSet.inter my_ps' ps)
				in
				let my_ps' = match NodeMap.fold build_ps cache None with
					  None -> PSet.empty
					| Some x -> x
				in
				let my_v = SMap.add a (my_ps',cache) my_v
				and changed = my_ps <> my_ps'
				in
				my_v, changed )
			in
			SMap.fold merge v' (v,false)

		| NodeObj _, NodeObj _ -> v, false (* ignore Cont rels *)
		| _ -> failwith "wrong abstract structure graph."
	in
	let fold_obj obj ns = NodeSet.add (NodeObj obj) ns
	in
	let ns = ObjSet.fold fold_obj objs NodeSet.empty
	in
	let values = gaS#rflood init push ns
	in
	(*
	let string_of_map v =
		let folder a (ps,_) buf =
			buf^"{"^a^" : "^string_of_procs ps^"} "
		in
		SMap.fold folder v ""
	in
	let dbg_val n v = match n with
		  NodeObj obj -> dbg ("minCONT^Obj("^string_of_obj obj^")="^string_of_map v)
		| NodeProc p -> dbg ("minCONT^Proc("^string_of_proc p^")="^string_of_map v)
		| _ -> ()
	in
	NodeMap.iter dbg_val values;*)
	let dbg_val n v = match n with
		  NodeObj (a,i,j) -> 
		  	let ps = try fst (SMap.find a v) with Not_found -> PSet.empty
			in
			dbg ("minCONT("^string_of_obj (a,i,j)^")="^string_of_procs ps)
		| _ -> ()
	in
	if !dodebug then NodeMap.iter dbg_val values
;;


