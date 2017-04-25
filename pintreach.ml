
open PintTypes
open Ph_types
open LocalCausalityGraph
open AutomataNetwork
open An_cli
open An_reach

let usage_msg = "pint-reach [opts] <sub-state1> ... # Static analysis for reachability"

and opt_cutsets_n = ref 0
and opt_cutsets_noinit = ref false
and opt_req_automata = ref SSet.empty
and opt_req_universal = ref false
and opt_legacy = ref false
and opt_bifurcations = ref false
and bifurcations_mode_choices = ["ua";"mole+ua"]
and opt_bifurcations_mode = ref "ua"
and opt_oneshot_mutations_cut = ref 0
and opt_ignore_automata = ref SSet.empty

let parse_automata_set =
	An_input.parse_string An_parser.automata_set

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--legacy", Arg.Set opt_legacy,
			"\tUse legacy under-approximation implementation (no clingo required).");
		("--bifurcations", Arg.Set opt_bifurcations,
			"\tIdentify bifurcation transitions for goal reachability (under-approximation)");
		("--bifurcations-method", Arg.Symbol (bifurcations_mode_choices,
									(fun x -> opt_bifurcations_mode := x)),
			"\tMode for computing bifurcations");
		("--cutsets", Arg.Set_int opt_cutsets_n,
			"n\tCompute cutsets up to given maximum cardinality");
		("--no-init-cutsets", Arg.Set opt_cutsets_noinit,
			"\tIgnore cut sets containing initial local states");
		("--requirements", arg_string_set parse_automata_set opt_req_automata,
			"<a,b,..>\tList requirements in term of given automata");
		("--requirements-universal", Arg.Set opt_req_universal,
			"\tCompute requirements for all initial state");
        ("--oneshot-mutations-for-cut", Arg.Set_int opt_oneshot_mutations_cut,
            "\tCompute mutations for making goal impossible");
        ("--ignore-automata",
            arg_string_set parse_automata_set opt_ignore_automata,
            "a,b,..\tIgnore given automata for cutsets or mutations");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let an, ctx = An_cli.process_input ()

let (an, ctx), goal = An_cli.prepare_goal (an, ctx) args

let do_cutsets = !opt_cutsets_n > 0

let do_requirements = SSet.cardinal !opt_req_automata > 0

let do_bifurcations = !opt_bifurcations

let do_reach = not (do_cutsets || do_requirements || do_bifurcations
    || !opt_oneshot_mutations_cut > 0)


(** verify opt_req_automata *)
let ukn = SSet.filter (fun a -> not (Hashtbl.mem an.automata a)) !opt_req_automata

let _ = if not (SSet.is_empty ukn) then
	failwith ("Invalid --requirements argument: unknown automata "^ string_of_sset ukn)

let env = init_env an ctx [goal]

let static_reach () =
	let result =
		if !opt_legacy then
			legacy_local_reachability env
		else
			local_reachability env
	in
	if !An_cli.opt_json_stdout then
		print_endline (json_of_ternary result)
	else
	print_endline (string_of_ternary result)

let bifurcations () =
    let btrs = Queue.create ()
    in
	let bifurcations =
		match !opt_bifurcations_mode with
		  "ua" ->
			if not !An_cli.opt_json_stdout then
			prerr_endline ("# mode: under-approximation");
			An_bifurcations.ua_bifurcations_ua
		| "mole+ua" ->
			if not !An_cli.opt_json_stdout then
			prerr_endline ("# mode: under-approximation enhanced by mole");
			An_bifurcations.ua_bifurcations_mole
		| _ -> failwith "Invalid mode for bifurcations."
	in
	let handle_solution aij cond =
		if !An_cli.opt_json_stdout then
            Queue.push (json_of_transition aij cond) btrs
		else
		print_endline (string_of_transition an aij cond)
	in
	let n = bifurcations handle_solution (an,ctx) goal
	in
	if !An_cli.opt_json_stdout then
        let fold (first,b) json_btr = false,
            if first then json_btr
            else (b^",\n"^json_btr)
        in
        let data = snd (Queue.fold fold (true,"") btrs)
        in
        print_endline ("["^data^"]")
    else
	prerr_endline (string_of_int n^" bifurcations transitions have been identified.")

let json_of_lss cs =
	json_of_ctx (ctx_of_lslist cs)

let string_of_ai = string_of_localstate an

let output_lss_list ?(ls_sep=",") lss_list =
	let print_ps ps =
		let s = String.concat ls_sep (List.map string_of_ai ps)
		in
		print_endline s
	in
    let lss_list = List.map (List.sort compare) lss_list
    in
    let lss_list = List.sort (fun a b ->
            let c = compare (List.length a) (List.length b)
            in
            if c <> 0 then c else compare a b) lss_list
    in
    if !An_cli.opt_json_stdout then
        print_endline (json_of_list json_of_lss lss_list)
    else (
        List.iter print_ps lss_list)

let cutsets n =
	let gA = lcg_for_cutsets env
	in
    let exclude_localstate (a,_) =
        SSet.mem a !opt_ignore_automata
    in
	let exclude_localstate =
		if !opt_cutsets_noinit then
			fun ai -> exclude_localstate ai || ctx_has_proc ai ctx
		else exclude_localstate
	in
    let (d_nkp, index_proc) = cutsets gA n exclude_localstate gA#leafs
	in
	let resolve_ps =
		List.map (Hashtbl.find index_proc)
	in
	let handle_proc ai =
		try
			let pss = fst (Hashtbl.find d_nkp (NodeProc ai))
			in
			let n = PSSet.cardinal pss
			in
			let elts = PSSet.elements pss
			in
			let elts = List.map resolve_ps elts
			in
            (if not (!An_cli.opt_json_stdout) then
				prerr_endline ("# "^string_of_int n^" cut set(s) for "
                       ^string_of_ai ai^":"));
            output_lss_list elts
		with Not_found ->
			if !An_cli.opt_json_stdout then
				print_endline (json_of_list id [])
			else
			print_endline (string_of_node (NodeProc ai)^" is not reachable.")
	in
	List.iter handle_proc env.goal

let requirements automata universal =
	let gA = lcg_for_requirements env
	in
    let reqs, index_proc = requirements gA automata gA#leafs universal
	in
	let resolve_ps = List.map (Hashtbl.find index_proc)
	in
	(* TODO: output in the same format as Causalex
	Hashtbl.iter (fun n v ->
		let pss = fst v
		in
		let elts = PSSet.elements pss
		in
		let elts = List.map resolve_ps elts
		in
		let elts = List.map (List.sort compare) elts
		in
		let elts = List.sort (fun a b ->
				let c = compare (List.length a) (List.length b)
				in
				if c <> 0 then c else compare a b) elts
		in
		let sv = String.concat " OR " (List.map (fun ps ->
				String.concat ";" (List.map string_of_ai ps)) elts)
		in
		prerr_endline ("["^string_of_node n^"] "^sv)) reqs;
	*)


	let handle_proc ai =
		try
			let pss = fst (Hashtbl.find reqs (NodeProc ai))
			in
			let n = PSSet.cardinal pss
			in
			let elts = PSSet.elements pss
			in
			let elts = List.map resolve_ps elts
			in
            (if not (!An_cli.opt_json_stdout) then
                prerr_endline ("# "^string_of_int n^" different requirements for "
                    ^string_of_ai ai^":"));
            output_lss_list ~ls_sep:" and " elts
		with Not_found ->
			if !An_cli.opt_json_stdout then
				print_endline (json_of_list id [])
			else
			print_endline (string_of_node (NodeProc ai)^" is not reachable.")
	in
	List.iter handle_proc env.goal

let _ =
    opt_ignore_automata := SSet.add "_pint_goal" !opt_ignore_automata;
	(if do_bifurcations then bifurcations ());
	(if do_cutsets then cutsets !opt_cutsets_n);
	(if do_requirements then requirements !opt_req_automata !opt_req_universal);
    (if !opt_oneshot_mutations_cut > 0 then
        let sols = An_reprogramming.ua_oneshot_mutations_for_cut
                        ~ignore:!opt_ignore_automata
                        (an,ctx) goal !opt_oneshot_mutations_cut
        in
        output_lss_list sols
    );
	(if do_reach then static_reach ())

