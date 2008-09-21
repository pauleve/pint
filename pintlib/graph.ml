
type ('a, 'b) t = ('a, ('b * 'a)) Hashtbl.t

let create = Hashtbl.create;;
let add (graph:('a,'b)t) = Hashtbl.add graph;;
let get (graph:('a,'b)t) = Hashtbl.find_all graph;;

let to_dot (graph:('a,'b)t) string_of_vertex string_of_label =
	let insert_if_new li el = if List.mem el li then li else el::li
	in
	let register_vertex vertex1 (label, vertex2) acc =
		insert_if_new (insert_if_new acc vertex1) vertex2
	in
	let vertices = Hashtbl.fold register_vertex graph []
	and dot_of_vertex vertex = 
		let svertex = string_of_vertex vertex in
		"\"" ^ svertex ^ "\"[label = \"" ^ svertex ^ "\"]\n"
	in
	let source = "digraph G { node[fillcolor = yellow, fontsize = 20] edge[fontsize = 20,fontname=times]\n" ^
		(String.concat "" (List.map dot_of_vertex vertices))
	and write_edge vertex1 (label, vertex2) source =
		source ^ "\"" ^ (string_of_vertex vertex1) ^ "\" -> \"" 
				^ (string_of_vertex vertex2) ^ "\"[label=\" " ^ (string_of_label label) ^" \"]\n"
	in
	(Hashtbl.fold write_edge graph source) ^ "}"
;;

