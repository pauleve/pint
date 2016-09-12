
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
and opt_iscutset = ref ""
and opt_bifurcations = ref false

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
	("--witness", Arg.Set opt_witness,
		"\tEnable witness computation");
	("--counterexample", Arg.Set opt_counterexample,
		"\tEnable counterexample computation");
	("--existential-ctx", Arg.Clear opt_ctx_universal,
		"Make context existential (default)");
	("--universal-ctx", Arg.Set opt_ctx_universal,
		"Make context universal instead of existential");
	("--is-cutset", Arg.Set_string opt_iscutset,
		"<local state list>\tCheck if given local state set is a cut set for the reachability property");
	("--bifurcations", Arg.Set opt_bifurcations,
		"\tAdd bifurcation specification for each local transition");
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

let smv_ls ai =
	let a,i = Hashtbl.find map ai
	in
	a^"="^i

let do_ctl () =
	let ctl =
		if !opt_bifurcations then
			let smv_goal = smv_ls goal
			in
			let ctl_of_tr ((a,i,j),conds) =
				let preconds = (a,i)::SMap.bindings conds
				and postconds = [a,j]
				in
				let preconds = String.concat " & " (List.map smv_ls preconds)
				and postconds = String.concat " & " (List.map smv_ls postconds)
				in
				"CTLSPEC EF ("^preconds^" & (EF ("^smv_goal^")) "^
					"& EX ("^postconds^" & !EF ("^smv_goal^")));"
			in
			let trs = TRSet.elements (an_sorted_transitions an)
			in
			let ctls = List.map ctl_of_tr trs
			in
			String.concat "\n" ctls
		else
			let ctl =
			if !opt_iscutset = "" then
				("EF ("^smv_ls goal^")")
			else
				let sls = An_cli.parse_local_state_list an !opt_iscutset
				in
				let sls = List.map (fun ai -> "!("^smv_ls ai^")") sls
				in
				("!E [("^(String.concat " & " sls)^") U ("^smv_ls goal^")]")
			in
			"CTLSPEC "^if !opt_witness then ("!("^ctl^")") else ctl
	in
	let data = data^"\n"^ctl^"\n"
	in
	let smv = make_smv (data^"\n")
	in
	let cmdline = "NuSMV"
		^(if !opt_witness || !opt_counterexample then "" else " -dcx ")
		^" "^String.concat " " !opt_extra
		^" "^smv
	in
	(if !opt_witness then
		prerr_endline "WARNING: witness requires the negation of the specification");
	prerr_endline ("# "^cmdline);
	ignore(Unix.system cmdline)

let _ = do_ctl ()

