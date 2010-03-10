
open Ph_types;;

type sortgraph_edge_t = E of (process * process * hit_t)
                      | L of (process * hit_t);;

let sortgraph smap a =
	try SMap.find a smap with Not_found -> []
;;

let sortgraphs_of_actions =
	let folder smap action = match action with
		Hit ((a,i),(b,j),j') ->
			let smap = if a = b then smap else
				SMap.add a (L ((a,i),action)::sortgraph smap a) smap
			in
			SMap.add b (E ((b,j),(b,j'),action)::sortgraph smap b) smap
	in
	List.fold_left folder SMap.empty
;;

let sortgraph_degres =
	let degres pmap ai = try PMap.find ai pmap with Not_found -> (0,0)
	in
	let count_degres pmap = function L _ -> pmap |
		E (ai,aj,_) ->
			let (ai_din, ai_dout) = degres pmap ai
			and (aj_din, aj_dout) = degres pmap aj
			in
			let pmap = PMap.add ai (ai_din,ai_dout+1) pmap
			in
			PMap.add aj (aj_din+1,aj_dout) pmap
	in
	List.fold_left count_degres PMap.empty
;;

exception Non_eulerian;;

(*
	Returns the (optional) first and last nodes of the Eulerian paths
	of the Sort Graph.
	Raises Non_eulerian if the graph is not Eulerian.
*)
let sortgraph_eulerian_extrem sortgraph =
	let eulerian_extrem ai (ai_din,ai_dout) (first,last) =
		match ai_din - ai_dout with
		  0 -> (first,last)
		| -1 -> if first = None then (Some ai,last) else raise Non_eulerian
		|  1 -> if last = None then (first,Some ai) else raise Non_eulerian
		| _ -> raise Non_eulerian
	in
	let degres = sortgraph_degres sortgraph
	in
	PMap.fold eulerian_extrem degres (None, None)
;;

(*
	Check if by duplicating edges, the sortgraph can become or stay Eulerian.
	Returns a ternary (True/False/Inconc).

	Algo:
		returns False if there exists a node with a null in (resp. out) degre
			and out (resp. in) degre greater than 1.
		returns False if there exist more than one node with a null in (resp. out) degre
			and out (resp. in) degre equal to 1.
		returns Inconc otherwise.
*)
let sortgraph_reeulerisable sortgraph =
	let test ai ai_d (first,last) =
		match ai_d with
		  (0,0) -> (first,last)
		| (0,1) -> if first = None then (Some ai,last) else raise Not_found
		| (1,0) -> if last = None then (first,Some ai) else raise Not_found
		| (0,_) | (_,0) -> raise Not_found
		| _ -> (first,last)
	in
	let degres = sortgraph_degres sortgraph
	in
	try
		ignore(PMap.fold test degres (None,None));
		Inconc
	with Not_found -> False
;;



