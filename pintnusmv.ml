
open Debug

open PintTypes
open AutomataNetwork
open An_export

let usage_msg = "pint-nusmv [opts] <local state> -- [NuSMV opts]"
	^" # symbolic model-checking with NuSMV [http://nusmv.fbk.eu]"

let opt_witness = ref false
and opt_counterexample = ref false
and opt_extra = ref []
and opt_ctx_universal = ref false

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
	("--witness", Arg.Set opt_witness,
		"\tEnable witness computation");
	("--counterexample", Arg.Set opt_counterexample,
		"\tEnable counterexample computation");
	("--existential-ctx", Arg.Clear opt_ctx_universal,
		"Make context existential (default)");
	("--universal-ctx", Arg.Set opt_ctx_universal,
		"Make context universal instead of existential");
	("--", Arg.Rest (fun arg -> opt_extra := !opt_extra @ [arg]),
		"Extra options for NuSMV");
]

let args, abort = An_cli.parse cmdopts usage_msg

let an, ctx = An_cli.process_input ()

let goal = match args with
	  [str_ls] -> An_cli.parse_local_state an str_ls
	| _ -> abort ()

let map = Hashtbl.create 50

let data = nusmv_of_an ~map:(Some map) !opt_ctx_universal an ctx

let make_smv data =
	let filename, outp = Filename.open_temp_file "pint" ".smv"
	in
	output_string outp data;
	close_out outp;
	(if not !Debug.dodebug then
		at_exit (fun () -> Unix.unlink filename));
	filename

let do_ctl () =
	let g,t = Hashtbl.find map goal
	in
	let ctl = "EF ("^g^"="^t^")\n"
	in
	let data = data ^ "\nSPEC\n"
		^if !opt_witness then ("!"^ctl) else ctl
	in
	let smv = make_smv data
	in
	let cmdline = "NuSMV"
		^(if !opt_witness || !opt_counterexample then "" else " -dcx ")
		^" "^smv
		^" "^String.concat " " !opt_extra
	in
	(if !opt_witness then
		prerr_endline "WARNING: witness requires the negation of the specification");
	prerr_endline ("# "^cmdline);
	ignore(Unix.system cmdline)

let _ = do_ctl ()

