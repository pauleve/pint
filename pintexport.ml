
open PintTypes;;
open AutomataNetwork;;
open An_export;;

let languages = ["dump";"pep";"ph";"prism"]
and opt_language = ref "dump"
and opt_output = ref ""
and opt_ptnet_context = ref false
and opt_goal = ref ""
and opt_partial = ref ""
and opt_mapfile = ref ""
in
let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-o", Arg.Set_string opt_output, "<filename>\tOutput filename");
		("--contextual-ptnet", Arg.Set opt_ptnet_context, 
									"\tContextual petri net");
		("--mapfile", Arg.Set_string opt_mapfile, 
									"\tOutput mapping of identifiers (for 'pep')");
		("--reduce-for-goal", Arg.Set_string opt_goal, 
			"\"a\"=i\tBefore exportation, reduce the model to include only transitions that may "
			^ "be involved in the reachability of the given local state");
		("--partial", Arg.Set_string opt_partial,
			"a,b,..\tConsider only the sub-network composed of a, b, ..");
	]
and usage_msg = "pint-export"
in
let args, abort = An_cli.parse cmdopts usage_msg
in
if args <> [] || !opt_language = "" then (abort ());
if !opt_mapfile <> "" && !opt_language <> "pep" then 
	(prerr_endline "Option --mapfile only supported with -l pep"; abort ());
let opts = {
	contextual_ptnet = !opt_ptnet_context;
}
in
let languages = [
	("dump", dump_of_an);
	("pep", pep_of_an opts ~mapfile:!opt_mapfile);
	("ph", ph_of_an);
	("prism", prism_of_an);
]
in
let translator = List.assoc !opt_language languages
in

let an, ctx = An_cli.process_input ()
in

let an, ctx = if !opt_partial = "" then an, ctx else
	let spec = "{"^(!opt_partial)^"}"
	in
	let aset = An_input.parse_string An_parser.automata_set spec
	in
	partial an aset, SMap.filter (fun a _ -> SSet.mem a aset) ctx
in

let an =
	if !opt_goal <> "" then
		let goal = [An_cli.parse_local_state an !opt_goal]
		in
		let env = An_reach.init_env an ctx goal
		in
		An_reach.reduced_an env
	else an
in

let data = translator an ctx
in
let channel_out = if !opt_output = "" then stdout else open_out !opt_output
in
output_string channel_out data;
close_out channel_out

