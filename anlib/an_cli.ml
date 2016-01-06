
open Ph_types
open AutomataNetwork

let parse_local_state an data =
	let (a,sig_i) = An_input.parse_string An_parser.local_state data
	in
	(a, get_automaton_state_id an a sig_i)

let parse_sls_list = An_input.parse_string An_parser.local_state_list

let parse_local_state_list an data =
	let sls = parse_sls_list data in
	List.map (fun (a,sig_i) -> a, get_automaton_state_id an a sig_i) sls

let arg_string_set f ref =
	Arg.String (fun s -> ref := f s)

let arg_set_sls_list =
	arg_string_set parse_sls_list

let common_cmdopts = [
	("--no-debug", Arg.Clear Debug.dodebug, "Disable debugging");
	("--debug", Arg.Set Debug.dodebug, "Enable debugging");
	("--debug-level", Arg.Set_int Debug.debuglevel, "Maximum debug level");
	("--version", Arg.Unit (fun () ->
			print_endline ("Pint version "^Distenv.version);
			ignore(exit 0)), "Print Pint version and quit");
	]


(**
	Input options
 **)
let opt_channel_in = ref stdin
let opt_filename_in = ref "<stdin>"

let setup_opt_channel_in filename =
	opt_filename_in := filename;
	opt_channel_in := open_in filename

let opt_override_ctx = ref []

let input_cmdopts = [
	("-i", Arg.String setup_opt_channel_in, "<model.an>\tInput filename");
	("--initial-state", arg_set_sls_list opt_override_ctx,
		"<local state list>\tInitial state");
	("--initial-context", arg_set_sls_list opt_override_ctx,
		"<local state list>\tInitial context (equivalent to --initial-state)");
	]

(**
	Main usage
 **)


let parse cmdopts usage_msg = 
	let opt_anonargs = ref ([]:string list)
	in
	let anon_fun arg = opt_anonargs := !opt_anonargs @ [arg]
	in
	Arg.parse cmdopts anon_fun usage_msg;
	!opt_anonargs, (fun () -> Arg.usage cmdopts usage_msg; raise Exit)

let process_input () =
	let an, ctx = An_input.parse !opt_channel_in
	in
	let user_ctx = ctx_of_siglocalstates an !opt_override_ctx
	in
	let ctx = ctx_override_by_ctx ctx user_ctx
	in
	an, ctx

let prepare_goal (an, ctx) args abort =
	let sig_goal = List.map parse_sls_list args
	in
	match sig_goal with [] -> abort ()
	| [[(a,sig_i)]] -> (an, ctx), (a, get_automaton_state_id an a sig_i)
	| _ -> An_reach.inject_goal_automaton (an, ctx) sig_goal

