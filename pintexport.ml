
open An_export;;

let languages = ["pep";"prism"]
and opt_language = ref ""

and opt_output = ref ""

and opt_ptnet_context = ref false

in
let cmdopts = Ui.common_cmdopts @ Ui.input_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-o", Arg.Set_string opt_output, "<filename>\tOutput filename");
		("--contextual-ptnet", Arg.Set opt_ptnet_context, 
									"\tContextual petri net");
	]
and usage_msg = "pint-export"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;
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

let an, ctx = An_input.parse !Ui.opt_channel_in
in
let ctx = Ph_types.ctx_override ctx !Ph_useropts.initial_procs
in
let data = translator an ctx
in
let channel_out = if !opt_output = "" then stdout else open_out !opt_output
in
output_string channel_out data;
close_out channel_out

