
open PintTypes
open AutomataNetwork
open An_export

let usage_msg = "pint-its [opts] <sub-state> -- [its opts] # symbolic model-checking with ITS tools [http://ddd.lip6.fr/itstools.php]"

let tools = ["reach";"ctl";"count"]
and opt_tool = ref "reach"
and opt_witness = ref false
and opt_extra = ref []
and opt_verbose = ref false
and opt_iscutset = ref ""
and opt_bifurcations = ref false

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
	("--tool", Arg.Symbol (tools, (fun t -> opt_tool := t)),
		"\tITS tool (default: reach)");
	("--witness", Arg.Set opt_witness,
		"\tEnable witness computation (used by: reach)");
	("--verbose", Arg.Set opt_verbose,
		"\tDrop --quiet option");
	("--bifurcations", Arg.Set opt_bifurcations,
		"\tVerify bifurcation specification for each local transition (tool=ctl)");
	("--is-cutset", Arg.Set_string opt_iscutset,
		"<local state list>\tCheck if given local state set is a cut set for the reachability property (only with --tool ctl)");
	("--", Arg.Rest (fun arg -> opt_extra := !opt_extra @ [arg]),
		"Extra options for the ITS tool");
]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = if !opt_tool <> "ctl" && (!opt_iscutset <> "" || !opt_bifurcations) then abort ()

let an, ctx = An_cli.process_input ()

let (an,ctx), goal, _ =
    if !opt_tool = "count" then
        (an,ctx), (0, 0), []
    else
        An_cli.prepare_goal (an,ctx) args

let map = Hashtbl.create 50

let data = romeo_of_an ~map:(Some map) an ctx

let itsfile, itsfile_out = Filename.open_temp_file "pint" ".xml"
let _ = output_string itsfile_out data;
		close_out itsfile_out

let place_of_ls ls =
	let (label, idx) = Hashtbl.find map (resolve_ls an ls)
	in
	"P_"^(string_of_int idx)^label

let its_state substate =
	String.concat " && " (List.map (fun ai -> place_of_ls ai^"=1") substate)


let do_count () =
	let cmdline = "its-reach -i "^itsfile^" -t ROMEO"
		^" --fixpass 0 --quiet"
		^" "^String.concat " " !opt_extra
	in
    let regex_result =
        Str.regexp "^ Total reachable state count : \\([0-9]+\\)$"
    in
    let rec parse_its_output cin =
        let line = input_line cin
        in
        if Str.string_match regex_result line 0 then
            Str.matched_group 1 line
        else
            parse_its_output cin
    in
    if !An_cli.opt_json_stdout then
        let cin = Unix.open_process_in cmdline
        in
        let result = parse_its_output cin
        in
        print_endline result;
        ignore(Unix.close_process_in cin)
    else
	(prerr_endline ("# "^cmdline);
	ignore(Unix.system cmdline))


let regex_reach_prop_true =
    Str.regexp "^Reachability property \\(.*\\) is true\\."
let regex_reach_prop_false =
    Str.regexp "^No reachable states exhibit your property : \\(.*\\)"

let rec parse_its_reach cin =
    let line = input_line cin
    in
    if Str.string_match regex_reach_prop_true line 0 then
        True
    else if Str.string_match regex_reach_prop_false line 0 then
        False
    else
        parse_its_reach cin

let do_reach () =
    let its_property = its_state [goal]
    in
	let cmdline = "its-reach -i "^itsfile^" -t ROMEO"
		^" -reachable \""^its_property^"\""
		^(if !opt_verbose then "" else " --quiet")
		^(if !opt_witness then "" else " --nowitness")
		^" "^String.concat " " !opt_extra
	in
    if !An_cli.opt_json_stdout then
        let cin = Unix.open_process_in cmdline
        in
        let result = parse_its_reach cin
        in
		print_endline (json_of_ternary result);
        ignore(Unix.close_process_in cin)
    else
	(prerr_endline ("# "^cmdline);
	ignore(Unix.system cmdline))

let do_ctl () =
	let itsctl, itsctl_out = Filename.open_temp_file "pint" ".ctl"
	in
	let ctls =
        if !opt_bifurcations then
			let ctl_of_tr (id,tr) =
				let preconds = IMap.bindings tr.pre
				and postconds = IMap.bindings tr.dest
				in
                "EF ("^its_state preconds^" && (EF ("^its_state [goal]^")) "^
                    "&& EX ("^its_state postconds^" && !EF ("^its_state [goal]^")))\n"
			in
            let trs = Hashtbl.fold (fun i tr trs -> (i,tr)::trs) an.trs []
			in
			List.map ctl_of_tr (List.sort compare trs)
        else if !opt_iscutset = "" then
            ["EF ("^its_state [goal]^")"]
		else
			let sls = An_cli.parse_local_state_list an !opt_iscutset
			in
			let sls = List.map (fun ai -> "!"^its_state [ai]) sls
			in
            ["!E(("^(String.concat " && " sls)^") U "^its_state [goal]^")"]
	in
    List.iter (fun ctl -> output_string itsctl_out (ctl^";\n")) ctls;
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
		else if !opt_tool = "count" then do_count ()

