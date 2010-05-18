(***
	Process Hitting Compiler
***)

open Ph_translator;;

let languages = ["dump"; "spim"; "prism"; "prism_mdp"; "romeo"; "tina"; "biocham"];;

let opt_language = ref "dump"
and opt_input = ref ""
and opt_output = ref ""
in
let cmdopts = Ui.common_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)), "\tOutput language");
		("-i", Arg.Set_string opt_input, "\tInput filename");
		("-o", Arg.Set_string opt_output, "\tOutput filename");
	]
and usage_msg = "phc"
in
let anon_fun _ = (Arg.usage cmdopts usage_msg; raise Exit)
in
Arg.parse cmdopts anon_fun usage_msg;
let opts = {
	alpha = 0.05;
	round_fi = Param.round_fi_ex
}
in
let languages = [
	("dump", dump_of_ph);
	("spim", spim_of_ph);
	("prism", prism_of_ph);
	("prism_mdp", prism_mdp_of_ph);
	("romeo", romeo_of_ph opts);
	("tina", tina_of_ph);
	("biocham", biocham_of_ph);
]
in
let translator = List.assoc !opt_language languages
in

let channel_in = if !opt_input = "" then stdin else open_in !opt_input
in
let ph, init_state = Ph_util.parse channel_in
in
close_in channel_in;
let data = translator ph init_state
in
let channel_out = if !opt_output = "" then stdout else open_out !opt_output
in
output_string channel_out data;
close_out channel_out

