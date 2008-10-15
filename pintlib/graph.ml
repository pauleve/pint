
type ('a, 'b) t = ('a, ('b * 'a)) Hashtbl.t

let create = Hashtbl.create;;
let add (graph:('a,'b)t) = Hashtbl.add graph;;
let get (graph:('a,'b)t) = Hashtbl.find_all graph;;
let iter f (graph:('a,'b)t) = Hashtbl.iter f graph;;
let fold f (graph:('a,'b)t) = Hashtbl.fold f graph;;

let vertices (graph:('a,'b)t) =
	let register_vertex vertex1 (label, vertex2) acc =
		Util.list_prepend_if_new vertex2 (Util.list_prepend_if_new vertex1 acc)
	in
	fold register_vertex graph []
;;
let edges graph =
	let register_edge a (e, b) acc = Util.list_prepend_if_new e acc
	in
	fold register_edge graph []
;;

let to_dot (graph:('a,'b)t) string_of_vertex string_of_label =
	let dot_of_vertex vertex = 
		let id = string_of_vertex vertex in "\""^id^ "\"[label=\""^id^"\"]"
	and dot_of_edge ((a,b),labels) =
		let aid = string_of_vertex a
		and bid = string_of_vertex b
		and label = String.concat "," (List.map string_of_label labels)
		in
		"\""^aid^"\"->\""^bid^"\"[label=\""^label^"\"]"
	and register_edge a (e, b) acc =
		((a,b), Util.list_prepend_if_new e (try List.assoc (a,b) acc with Not_found -> []))
		::List.remove_assoc (a,b) acc
	in
	let edges = fold register_edge graph []
	in
	"digraph G { node[fontsize=20] edge[fontsize=15,fontname=times]\n" ^
	(String.concat "\n" (List.map dot_of_vertex (vertices graph)))^"\n" ^
	(String.concat "\n" (List.map dot_of_edge edges))^"\n"^
	"}\n"
;;


let remove_labeled graph labels =
	let graph2 = create 0
	in
	let register_non_matching a (label, b) =
		if not (List.mem label labels) then add graph2 a (label, b)
	in
	iter register_non_matching graph;
	graph2
;;

let next_vertices graph start =
	Util.list_uniq (List.map snd (get graph start))
;;

let next_colored_vertices graph colored start =
	List.filter (fun v -> List.mem v colored) (next_vertices graph start)
;;

let next_uncolored_vertices graph colored start =
	List.filter (fun v -> not(List.mem v colored)) (next_vertices graph start)
;;

let reachability graph dest start =
	let rec _reachability graph colored dest = 
		let start = List.hd colored
		in
		if dest <> start then
			let folder (found, colored) v =
				match found with 
					  true -> (found,colored)
					| false -> _reachability graph (v::colored) dest
			and following = next_uncolored_vertices graph colored start
			in
			List.fold_left folder (false, colored) following
		else (true, colored)
	in
	fst (_reachability graph [start] dest)
;;

let color_reachability graph vertices start =
	let rec _reachability graph colored vertices = 
		let start = List.hd colored
		in
		if not (List.mem start vertices) then
			let folder colored v = _reachability graph (v::colored) vertices
			and following = next_uncolored_vertices graph colored start
			in
			List.fold_left folder colored following
		else colored
	in
	_reachability graph [start] vertices
;;


