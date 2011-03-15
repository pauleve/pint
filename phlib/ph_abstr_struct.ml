(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2010)

loic.pauleve@irccyn.ec-nantes.fr
morgan.magnin@irccyn.ec-nantes.fr
olivier.roux@irccyn.ec-nantes.fr

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

(* example of flood usage: 
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
*)

