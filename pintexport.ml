
open An_export;;

let languages = ["dump";"pep";"prism"]
and opt_language = ref "dump"
and opt_output = ref ""
and opt_ptnet_context = ref false
and opt_goal = ref ""
in
let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-o", Arg.Set_string opt_output, "<filename>\tOutput filename");
		("--contextual-ptnet", Arg.Set opt_ptnet_context, 
									"\tContextual petri net");
		("--reduce-for-goal", Arg.Set_string opt_goal, 
			"\"a\"=i\tReduce the model to include only transitions that may "
			^ "be involved in the reachability of the given local state");
	]
and usage_msg = "pint-export"
in
let args, abort = An_cli.parse cmdopts usage_msg
in
(if args <> [] || !opt_language = "" then abort ());
let opts = {
	contextual_ptnet = !opt_ptnet_context;
}
in
let languages = [
	("dump", dump_of_an);
	("pep", pep_of_an opts);
	("prism", prism_of_an);
]
in
let translator = List.assoc !opt_language languages
in

let an, ctx = An_cli.process_input ()
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

