
open PintTypes
open AutomataNetwork
open An_export
open An_transformers

let make_partial spec (an, ctx) =
	let aset = An_input.parse_string An_parser.automata_set spec
	in
    let aset = SSet.fold (fun a -> ISet.add (resolve_sig_a an a)) aset ISet.empty
    in
	let an = partial an aset
	and ctx = IMap.filter (fun a _ -> ISet.mem a aset) ctx
	in
	an, ctx

let make_goal spec anctx =
    let anctx, _, _ = An_cli.prepare_goal anctx [spec]
    in
    anctx

let opt_reduction_skip_oa = ref false
let make_reduce_for_goal spec (an, ctx) =
    let (an, ctx), (g,top), extra_a = An_cli.prepare_goal (an, ctx) [spec]
	in
	let env = An_reach.init_env an ctx [g,top]
	in
	let an = An_reach.reduced_an ~skip_oa:!opt_reduction_skip_oa env
    in
    let ctx = List.fold_left (fun ctx a ->
        remove_sink_automaton an a;
        IMap.remove a ctx) ctx extra_a
    in
    an, ctx

let opt_squeeze_preserve = ref SSet.empty
let make_squeeze (an, ctx) =
	squeeze ~preserve:(resolve_sig_a_set an !opt_squeeze_preserve) an ctx

let make_simplify (an, ctx) =
	simplify an, ctx

let make_disable dctx (an, ctx) =
	let dctx = ctx_of_siglocalstates an dctx
	in
    let state_match state =
        IMap.exists (fun a is -> match IMap.find_opt a state with
                  Some i -> ISet.mem i is
                | None -> false) dctx
    in
    let filter _ tr =
        not (state_match tr.pre)
    in
    an_with_filtered_transitions an filter, ctx

let make_lock lctx (an, ctx) =
    let lctx = ctx_of_siglocalstates an lctx
    in
    let filter _ tr =
        not (IMap.exists (fun a _ -> IMap.mem a tr.orig) lctx)
    in
    let ctx = ctx_override_by_ctx ctx lctx
    in
    an_with_filtered_transitions an filter, ctx

let languages = ["dump";"nusmv";"pep";"ph";"prism";"romeo";"nbjson";"asp"]
and opt_language = ref "dump"
and opt_output = ref ""
and opt_ptnet_context = ref false
and opt_mapfile = ref ""
and opt_transforms = Queue.create ()
and opt_ctx_universal = ref false
and opt_output_transitions = ref true

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
        ("--skip-transitions", Arg.Clear opt_output_transitions,
            "\tDo not output transitions (used by: nbjson)");
		("--partial", Arg.String
			(fun spec -> push_transform (make_partial spec)),
			"a,b,..\tConsider only the sub-network composed of a, b, ..");
        ("--inject-goal", Arg.String
            (fun goal -> push_transform (make_goal goal)),
            "<goal>\tInject the goal automaton - mainly for debugging purpose");
		("--reduce-for-goal", Arg.String
			(fun goal -> push_transform (make_reduce_for_goal goal)),
			"<goal>\tRemove transitions that never occur in minimal traces for reaching the given local state");
		("--test-partial-reduction", Arg.Set opt_reduction_skip_oa,
			"(for benchmarks only) do not use the over-approximation of the goal-oriented reduction");
		("--simplify", Arg.Unit (fun () -> push_transform make_simplify),
			"\tTry to simplify transition conditions of the automata network");
		("--squeeze", Arg.Unit (fun () -> push_transform make_squeeze),
			"\tRemove unused automata and local states");
        ("--squeeze-preserve", Arg.String (fun a ->
            opt_squeeze_preserve := SSet.add a !opt_squeeze_preserve),
            "\tDo not squeeze given automaton");
		("--disable", Arg.String
			(fun ctx -> push_transform (make_disable (An_cli.parse_sls_list ctx))),
			"<local state list>\tDisable mentionned local states");
		("--lock", Arg.String
			(fun ctx -> push_transform (make_lock (An_cli.parse_sls_list ctx))),
			"<local state list>\tLock to local states");
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
    ("asp", asp_of_an);
	("dump", dump_of_an);
	("pep", pep_of_an opts ~mapfile:!opt_mapfile);
	("nusmv", nusmv_of_an ~map:None !opt_ctx_universal);
	("prism", prism_of_an);
	("romeo", romeo_of_an ~map:None ~mapfile:!opt_mapfile);
	("nbjson", nbjson_of_an ~output_transitions:!opt_output_transitions);
]
let translator = List.assoc !opt_language languages

let an, ctx = An_cli.process_input ()

let an, ctx = Queue.fold (fun (an, ctx) func -> func (an, ctx)) (an, ctx) opt_transforms

let data = translator an ctx

let channel_out = if !opt_output = "" then stdout else open_out !opt_output

let _ =
	output_string channel_out data;
	close_out channel_out

