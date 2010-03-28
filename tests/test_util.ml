
let selectors = [[0];[1;2];[3;4]]
and handler ilist =
	print_endline (String.concat ";" (List.map string_of_int ilist))
and merger () () = ()
and stopper () = false
in
Util.cross_forward (handler,merger,stopper) selectors
;;

print_endline "---";

let selectors = [[0];[1;2];[3;4]]
and handler ilist = 
	print_endline (String.concat ";" (List.map string_of_int ilist));
	List.mem 2 ilist
and merger r l = r || l
and stopper v = v
in
Util.cross_forward (handler,merger,stopper) selectors


