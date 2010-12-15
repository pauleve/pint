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

