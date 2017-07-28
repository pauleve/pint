
open PintTypes

open LocalCausalityGraph
open AutomataNetwork

open An_reach

let usage_msg = "pint-lcg [opts] [<sub-state1> ...] # compute Local Causality Graph for goal"

let lcg_types = ["verbose";"trimmed";"saturated";"worth";"full"]
let lcg_type = ref "verbose"
let opt_output = ref "-"

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("-o", Arg.Set_string opt_output, "<lcg.dot>\tOutput file");
		("-t", Arg.Symbol (lcg_types, (fun x -> lcg_type := x)),
					"\tWhich Local Causality Graph (default: "^(!lcg_type)^")");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let an, ctx = An_cli.process_input ()

let (an, ctx), goal, _ =
    if !lcg_type = "full" then
        (an,ctx), (0,0), []
    else
        An_cli.prepare_goal (an, ctx) args

let env = init_env an ctx [goal]

let sols = An_localpaths.abstract_local_paths env.ac env.an

let verbose_lcg () =
    build_oa_lcg env.an env.ctx env.goal sols

let trimmed_lcg () =
	let lcg = verbose_lcg ()
	in
	let lcg = bot_trimmed_lcg env sols lcg
	in
	top_trimmed_lcg env lcg;
	lcg

let worth_lcg () =
	gored_lcg env

let saturated_lcg () =
    assert_async_an env.an; (* TODO *)
	let uoa, (oa_lcg, valid) = unordered_oa env sols
	in
    let overlay = Hashtbl.create (NodeSet.cardinal valid)
    in
    restrict_sols overlay oa_lcg valid;
    let sols = An_localpaths.restricted_abstract_local_paths env.ac env.an overlay
	in
	let lcg = new lcg default_lcg_setup an ctx env.goal sols
	in
	lcg#set_auto_conts false;
	lcg#build;
	lcg#saturate_ctx;
	lcg


let lcg_factories = [
	("verbose", verbose_lcg);
	("trimmed", trimmed_lcg);
	("worth", worth_lcg);
	("saturated", saturated_lcg);
	("full", (fun () -> full_lcg env.ac env.an));
]

let lcg = (List.assoc !lcg_type lcg_factories) ()

let data = lcg#to_dot

let _ =
	let channel_out = if !opt_output = "-" then stdout else open_out !opt_output
	in
	output_string channel_out data;
	close_out channel_out

