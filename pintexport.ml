
open PintTypes
open AutomataNetwork
open An_export
open Ph_types

let make_partial spec (an, ctx) =
	let spec = "{"^spec^"}"
	in
	let aset = An_input.parse_string An_parser.automata_set spec
	in
	SSet.iter (fun a -> if not(Hashtbl.mem an.automata a) then
					failwith ("Unknown automaton '"^a^"'")) aset;
	let an = partial an aset
	and ctx = SMap.filter (fun a _ -> SSet.mem a aset) ctx
	in
	an, ctx

let make_reduce_for_goal goal (an, ctx) =
	let goal = [An_cli.parse_local_state an goal]
	in
	let env = An_reach.init_env an ctx goal
	in
	An_reach.reduced_an env, ctx

let make_squeeze (an, ctx) =
	squeeze an ctx

let make_simplify (an, ctx) =
	simplify an, ctx

let make_disable dctx (an, ctx) =
	let dctx = ctx_of_siglocalstates an dctx
	in
	let rctx = ctx_diff (full_ctx an) dctx
	in
	restrict an rctx, ctx

let languages = ["dump";"nusmv";"pep";"ph";"prism";"romeo"]
and opt_language = ref "dump"
and opt_output = ref ""
and opt_ptnet_context = ref false
and opt_mapfile = ref ""
and opt_transforms = Queue.create ()
and opt_ctx_universal = ref false

let push_transform func =
	Queue.push func opt_transforms

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("-l", Arg.Symbol (languages, (fun l -> opt_language := l)),
			"\tOutput language (default: dump)");
		("-o", Arg.Set_string opt_output, "<filename>\tOutput filename");
		("--contextual-ptnet", Arg.Set opt_ptnet_context,
			"\tContextual petri net (used by: pep)");
		("--mapfile", Arg.Set_string opt_mapfile,
			"\tOutput mapping of identifiers (used by: pep, romeo)");
		("--partial", Arg.String
			(fun spec -> push_transform (make_partial spec)),
			"a,b,..\tConsider only the sub-network composed of a, b, ..");
		("--reduce-for-goal", Arg.String
			(fun goal -> push_transform (make_reduce_for_goal goal)),
			"\"a\"=i\tRemove transitions that never occur in minimal traces for reaching the given local state");
		("--simplify", Arg.Unit (fun () -> push_transform make_simplify),
			"\tTry to simplify transition conditions of the automata network");
		("--squeeze", Arg.Unit (fun () -> push_transform make_squeeze),
			"\tRemove unused automata and local states");
		("--disable", Arg.String
			(fun ctx -> push_transform (make_disable (An_cli.parse_sls_list ctx))),
			"<local state list>\tDisable mentionned local states");
		("--existential-ctx", Arg.Clear opt_ctx_universal,
			"Make context existential (default, used by: nusmv)");
		("--universal-ctx", Arg.Set opt_ctx_universal,
			"Make context universal instead of existential (used by: nusmv)");
	]
and usage_msg = "pint-export"

let args, abort = An_cli.parse cmdopts usage_msg

let _ = if args <> [] || !opt_language = "" then (abort ())

let opts = {
	contextual_ptnet = !opt_ptnet_context;
}
let languages = [
	("dump", dump_of_an);
	("pep", pep_of_an opts ~mapfile:!opt_mapfile);
	("nusmv", nusmv_of_an ~map:None !opt_ctx_universal);
	("ph", ph_of_an);
	("prism", prism_of_an);
	("romeo", romeo_of_an ~map:None ~mapfile:!opt_mapfile);
]
let translator = List.assoc !opt_language languages

let an, ctx = An_cli.process_input ()

let an, ctx = Queue.fold (fun (an, ctx) func -> func (an, ctx)) (an, ctx) opt_transforms

let data = translator an ctx

let channel_out = if !opt_output = "" then stdout else open_out !opt_output

let _ =
	output_string channel_out data;
	close_out channel_out

