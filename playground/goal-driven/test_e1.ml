
open PintTypes;;
open Ph_types;;
open Ph_reach;;

let test_worth env ai =
	let env = {env with ctx = 
		Ph_cooperativity.coherent_ctx !Ph_instance.cooperativities env.ctx}
	in
	print_endline ("is it worth to reach "^string_of_proc ai^"? ");
	print_endline (string_of_bool (Ph_thomas.is_worthy_process env ai))

in
let model = "e1.ph"
and goal = "c", 1
in
let ph, ctx = Ph_util.parse (open_in model)
in
let env = init_env ph ctx [goal]
in

test_worth env ("d", 1);
test_worth env ("a", 1);
