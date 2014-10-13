
open PintTypes;;
open Ph_types;;
open Ph_reach;;

let test_worth ph ctx goal procs =
	let ctx = Ph_cooperativity.coherent_ctx !Ph_instance.cooperativities ctx
	in
	let env = init_env ph ctx [goal]
	in
	let glc = worth_glc env
	in
	let test_worth_proc ai =
		print_endline ("is it worth to reach "^string_of_proc ai^"? ");
		print_endline (string_of_bool (is_process_worth glc ai))
	in
	List.iter test_worth_proc procs

in
let model = "e1.ph"
and goal = "c", 1
in
let ph, ctx = Ph_util.parse (open_in model)
in
test_worth ph ctx goal ["d",1;"a",1;"b",1];
