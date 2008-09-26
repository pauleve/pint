
let prepend s = List.map (fun e -> s::e);;

let rec cross l = function [] -> [] | h::q -> (prepend h l) @ (cross l q);;

let cross_list (x:'a list list) : 'a list list = List.fold_left cross [[]] x;;

