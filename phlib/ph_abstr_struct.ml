
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
	val edges = Hashtbl.create 50
	val rev_edges = Hashtbl.create 50

	method add_child n1 n2 =
		Hashtbl.add edges n1 n2;
		Hashtbl.add rev_edges n2 n1

	method private _flood
		: 'a. bool -> (node -> 'a) 
			-> (node -> 'a -> node -> 'a -> 'a * bool)
			-> NodeSet.t -> (node, 'a) Hashtbl.t
		= fun desc init push ns ->
		let edges = if desc then edges else rev_edges
		and values = Hashtbl.create 50
		in
		let rec flood chgs = 
			let forward n chgs = 
				(* forward the new value v of n to its childs *)
				let v = Hashtbl.find values n
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
				let childs = Hashtbl.find_all edges n
				in
				List.fold_left forward_to chgs childs
			in
			let chgs = NodeSet.fold forward chgs NodeSet.empty
			in
			if not (NodeSet.is_empty chgs) then flood chgs

		and setup n =
			Hashtbl.add values n (init n)
		in
		NodeSet.iter setup ns;
		flood ns;
		values
	
	method flood 
		: 'a. (node -> 'a) 
			-> (node -> 'a -> node -> 'a -> 'a * bool)
			-> NodeSet.t -> (node, 'a) Hashtbl.t
		= self#_flood true
	method rflood 
		: 'a. (node -> 'a) 
			-> (node -> 'a -> node -> 'a -> 'a * bool)
			-> NodeSet.t -> (node, 'a) Hashtbl.t
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
	Hashtbl.iter dbg_val values
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
	if !dodebug then Hashtbl.iter dbg_val values
;;


