
open Debug
open PintTypes
open AutomataNetwork
open An_export

let usage_msg = "pint-mole [opts] <sub-state> -- [mole opts]"
	^" # unfolding with mole [http://www.lsv.ens-cachan.fr/~schwoon/tools/mole]"

let opt_extra = ref []
let opt_mci = ref ""

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
	("--mci", Arg.Set_string opt_mci,
		"\tSave resulting .mci file to given path");
	("--", Arg.Rest (fun arg -> opt_extra := !opt_extra @ [arg]),
		"\tExtra options for mole");
]

let args, abort = An_cli.parse cmdopts usage_msg

let an, ctx = An_cli.process_input ()

let goal = match args with
	  [str_s] -> An_cli.parse_local_state_list an str_s
	| _ -> abort ()

let conds = List.fold_left (fun conds (a,i) -> SMap.add a i conds) SMap.empty goal

let opts = {
	contextual_ptnet = false;
}
let data = pep_of_an opts ~goal:(Some (conds, "GOAL")) an ctx

let netfile, netfile_out = Filename.open_temp_file "pint" ".ll"

let _ = at_exit (fun () -> Unix.unlink netfile)

let mcifile = if !opt_mci <> "" then !opt_mci else
	let f = Filename.temp_file "pint" ".mci"
	in
	(at_exit (fun () -> Unix.unlink f); f)

let _ = output_string netfile_out data;
		close_out netfile_out

let _ =
	let cmdline = "mole -T GOAL"
		^" "^String.concat " " !opt_extra
		^" "^netfile
		^" -m "^mcifile
	in
	dbg ~level:1 ("# "^cmdline);
    let result = match Unix.system cmdline with
	  Unix.WEXITED 2 -> True
	| Unix.WEXITED 0 -> False
	| _ -> failwith "Error executing mole"
    in
    if !An_cli.opt_json_stdout then
        print_endline (json_of_ternary result)
    else
        print_endline (string_of_ternary result)

