
open LocalCausalityGraph
open AutomataNetwork

let usage_msg = "pint-sg"


let do_count = ref false

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--count-reachable", Arg.Set do_count, "\tCount reachable states.");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = if args <> [] then abort ()

let an, ctx = An_cli.process_input ()


let count () =
	let state = Ph_types.state_of_ctx ctx
	in
	let nb_states, _ = An_stategraph.reachable_states an state
	in
	print_endline (Big_int.string_of_big_int nb_states ^ " reachable states")

let _ = if !do_count then count ()

