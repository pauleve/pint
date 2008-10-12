
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
		let svertex = string_of_vertex vertex in
		"\"" ^ svertex ^ "\"[label = \"" ^ svertex ^ "\"]\n"
	in
	let source = "digraph G { node[fontsize=20] edge[fontsize=20,fontname=times]\n" ^
		(String.concat "" (List.map dot_of_vertex (vertices graph)))
	and write_edge vertex1 (label, vertex2) source =
		source ^ "\"" ^ (string_of_vertex vertex1) ^ "\" -> \"" 
				^ (string_of_vertex vertex2) ^ "\"[label=\" " ^ (string_of_label label) ^" \"]\n"
	in
	(Hashtbl.fold write_edge graph source) ^ "}\n"
;;

