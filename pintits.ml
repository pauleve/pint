
open PintTypes
open AutomataNetwork
open An_export

let usage_msg = "pint-its [opts] <sub-state> -- [its opts] # symbolic model-checking with ITS tools [http://ddd.lip6.fr/itstools.php]"

let tools = ["reach";"ctl"]
and opt_tool = ref "reach"
and opt_witness = ref false
and opt_extra = ref []
and opt_verbose = ref false
and opt_iscutset = ref ""

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
	("--tool", Arg.Symbol (tools, (fun t -> opt_tool := t)),
		"\tITS tool (default: reach)");
	("--witness", Arg.Set opt_witness,
		"\tEnable witness computation (used by: reach)");
	("--verbose", Arg.Set opt_verbose,
		"\tDrop --quiet option");
	("--is-cutset", Arg.Set_string opt_iscutset,
		"<local state list>\tCheck if given local state set is a cut set for the reachability property (only with --tool ctl)");
	("--", Arg.Rest (fun arg -> opt_extra := !opt_extra @ [arg]),
		"Extra options for the ITS tool");
]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = if !opt_tool <> "ctl" && !opt_iscutset <> "" then abort ()

let an, ctx = An_cli.process_input ()

let goal = match args with
	  [str_s] -> An_cli.parse_local_state_list an str_s
	| _ -> abort ()

let map = Hashtbl.create 50

let data = romeo_of_an ~map:(Some map) an ctx

let itsfile, itsfile_out = Filename.open_temp_file "pint" ".xml"
let _ = output_string itsfile_out data;
		close_out itsfile_out

let place_of_ls ls =
	let (label, idx) = Hashtbl.find map ls
	in
	"P_"^(string_of_int idx)^label

let its_state substate =
	String.concat " && " (List.map (fun ai -> place_of_ls ai^"=1") substate)

let do_reach () =
	let cmdline = "its-reach -i "^itsfile^" -t ROMEO"
		^" -reachable \""^(its_state goal)^"\""
		^(if !opt_verbose then "" else " --quiet")
		^(if !opt_witness then "" else " --nowitness")
		^" "^String.concat " " !opt_extra
	in
	prerr_endline ("# "^cmdline);
	ignore(Unix.system cmdline)

let do_ctl () =
	let itsctl, itsctl_out = Filename.open_temp_file "pint" ".ctl"
	in
	let ctl =
		if !opt_iscutset = "" then
			("EF ("^its_state goal^")")
		else
			let sls = An_cli.parse_local_state_list an !opt_iscutset
			in
			let sls = List.map (fun ai -> "!"^its_state [ai]) sls
			in
			("!E(("^(String.concat " && " sls)^") U "^its_state goal^")")
	in
	output_string itsctl_out (ctl^";\n");
	close_out itsctl_out;
	let cmdline = "its-ctl -i "^itsfile^" -t ROMEO -ctl "^itsctl
		^(if !opt_verbose then "" else " --quiet")
		^" "^String.concat " " !opt_extra
	in
	prerr_endline ("# "^cmdline);
	ignore(Unix.system cmdline);
	Unix.unlink itsctl

let _ = at_exit (fun () -> Unix.unlink itsfile)

let _ = if !opt_tool = "reach" then do_reach ()
		else if !opt_tool = "ctl" then do_ctl ()

