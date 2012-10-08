
module ISSet = SSet.Make (struct type t = int let compare = compare 
							let max_h = 4 let string_of_elt = string_of_int end);;
let n = ISSet.singleton and u = ISSet.union and p = ISSet.product
in

let a' = p (n 1) (p (n 2) (u (n 3) (n 4)))
in

let a = p (n 1) (u (n 4) (p (n 3) (n 5)))
in

let a = u a a'
in



let a = ISSet.cleanup a


in
let fd = open_out "gen/test.dot"
in
output_string fd (ISSet.to_dot a);
close_out fd


