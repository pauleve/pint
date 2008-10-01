
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

let rec list_intersection a = function [] -> []
	| h::q -> (if List.mem h a then [h] else []) @ list_intersection a q
;;

let list_remove v = List.filter (fun x -> x <> v)

let dump_to_file filename content = 
	let fd = open_out filename in
	output_string fd content;
	close_out fd
;;

