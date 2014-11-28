
open An_export;;

let languages = ["pep";"prism"]
and opt_language = ref ""

and opt_output = ref ""

and opt_ptnet_context = ref false

in
let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-o", Arg.Set_string opt_output, "<filename>\tOutput filename");
		("--contextual-ptnet", Arg.Set opt_ptnet_context, 
									"\tContextual petri net");
	]
and usage_msg = "pint-export"
in
let args, abort = An_cli.parse cmdopts usage_msg
in
(if args <> [] then abort ());
let opts = {
	contextual_ptnet = !opt_ptnet_context;
}
in
let languages = [
	("pep", pep_of_an opts);
	("prism", prism_of_an);
]
in
let translator = List.assoc !opt_language languages
in

let an, ctx = An_cli.process_input ()
in
let data = translator an ctx
in
let channel_out = if !opt_output = "" then stdout else open_out !opt_output
in
output_string channel_out data;
close_out channel_out

