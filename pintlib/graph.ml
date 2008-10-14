
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

