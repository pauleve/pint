
let prepend s = List.map (fun e -> s::e);;

let rec cross l = function [] -> [] | h::q -> (prepend h l) @ (cross l q);;

let cross_list (x:'a list list) : 'a list list = List.fold_left cross [[]] x;;

let list_union a b = a @ (List.filter (fun x -> not (List.mem x a)) b);;

let list_prepend_if_new a l = if List.mem a l then l else a::l;;

let list_uniq (l:'a list) =
	let rec _list_uniq u = function [] -> u
		| h::q -> if List.mem h u then _list_uniq u q 
				else _list_uniq (h::u) q
	in
	_list_uniq [] l
;;
let list_uniq2 l =
	list_uniq (List.map (List.sort Pervasives.compare) l)

let rec list_intersection a = function [] -> []
	| h::q -> (if List.mem h a then [h] else []) @ list_intersection a q
;;

let list_remove v = List.filter (fun x -> x <> v)
;;

let rec list_replace v v' = function [] -> []
	| h::q -> (if h = v then v' else h)::list_replace v v' q
;;

let rec list_sub a = function [] -> a | v::b -> list_sub (list_remove v a) b
;;

let list_separate pred =
	let folder (sideA, sideB) item =
		if pred item then item::sideA, sideB
		else sideA, item::sideB
	in
	List.fold_left folder ([], [])
;;

let rec subset l = function [] -> true
	| h::q -> (List.mem h l) && subset l q
;;

let rec rrange a b = if a <= b then b::rrange a (b-1) else [];;
let rec range a b = if a <= b then a::range (a+1) b else [];;

let dump_to_file filename content = 
	let fd = open_out filename in
	output_string fd content;
	close_out fd
;;

let rec count_elements = function [] -> []
	| h::q ->
		let hs, q = list_separate (fun h' -> h = h') q
		in
		(h, 1+List.length hs)::count_elements q
;;

