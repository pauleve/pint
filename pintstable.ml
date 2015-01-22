
open PintTypes
open AutomataNetwork

let usage_msg = "pint-stable [opts]"

let do_fixpoints = ref false

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--fixpoints", Arg.Set do_fixpoints,
			"\tCompute all the fixed points.");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = (if args <> [] then abort ())

let an, ctx = An_cli.process_input ()

let fixpoints () =
	let fps = An_fixpoint.fixpoints an
	and string_of_lsset = string_of_set (string_of_localstate an) LSSet.elements
	in
	print_endline ("# "^string_of_int (List.length fps)^ " fixed points");
	List.iter print_endline (List.map string_of_lsset fps)

let _ = if !do_fixpoints then fixpoints ()

