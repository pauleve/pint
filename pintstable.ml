
open PintTypes
open AutomataNetwork

let usage_msg = "pint-stable [--fixpoints] [opts]"

let do_fixpoints = ref false
let restrict = ref []

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--fixpoints", Arg.Set do_fixpoints,
			"\tCompute all the fixed points.");
		("--restrict", An_cli.arg_set_sls_list restrict,
			"\tRestrict the results with the given local states");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = (if args <> [] || not !do_fixpoints then abort ())

let an, ctx = An_cli.process_input ()
let restrict = resolve_siglocalstates an !restrict

let fixpoints () =
	let fps = An_fixpoint.fixpoints ~restrict an
	and string_of_lsset = string_of_set (string_of_localstate an) LSSet.elements
	in
	print_endline ("# "^string_of_int (List.length fps)^ " fixed points");
	List.iter print_endline (List.map string_of_lsset fps)

let _ = if !do_fixpoints then fixpoints ()

