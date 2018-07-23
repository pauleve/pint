
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
let restrict = List.map (resolve_sig_ls an) !restrict

let fixpoints () =
	if !An_cli.opt_json_stdout then
        let fps = An_fixpoint.fixpoints ~restrict an
        in
		print_endline (json_of_list (json_of_state an) fps)
	else
        let n = ref 0
        in
        let fixpoint state =
            incr n;
            print_endline (string_of_state an state)
        in
        let _ = An_fixpoint.fixpoints_jit fixpoint restrict an
        in
		print_endline ("# "^string_of_int !n^ " fixed points")

let _ = if !do_fixpoints then fixpoints ()

