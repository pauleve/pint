
module IKSets = KSets.Make (struct type t = int let compare = compare 
							let max_h = 4 let string_of_elt = string_of_int end);;
let n = IKSets.singleton and u = IKSets.union and p = IKSets.product
in

let a' = p (n 1) (p (n 2) (u (n 3) (n 4)))
in

let a = p (n 1) (u (n 4) (p (n 3) (n 5)))
in

let a = u a a'
in

(*let a = IKSets.rm_sursets (p (n 1) (n 4)) a in*)
let a = IKSets.simplify a in



List.iter (fun elt -> 
	let str = String.concat ", " (List.map string_of_int elt)
	in
	print_endline ("{ "^str^" }")) (IKSets.elements a);

let fd = open_out "gen/test.dot"
in
output_string fd (IKSets.to_dot a);
close_out fd


