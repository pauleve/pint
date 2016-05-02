
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

let parse_automata_set =
	An_input.parse_string An_parser.automata_set

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--legacy", Arg.Set opt_legacy,
			"\tUse legacy under-approximation implementation (no clingo required).");
		("--cutsets", Arg.Set_int opt_cutsets_n,
			"n\tCompute cutsets up to given maximum cardinality");
		("--no-init-cutsets", Arg.Set opt_cutsets_noinit,
			"\tIgnore cut sets containing initial local states");
		("--requirements", arg_string_set parse_automata_set opt_req_automata,
			"<a,b,..>\tList requirements in term of given automata");
		("--requirements-universal", Arg.Set opt_req_universal,
			"\tCompute requirements for all initial state");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let an, ctx = An_cli.process_input ()

let (an, ctx), goal = An_cli.prepare_goal (an, ctx) args abort

let do_cutsets = !opt_cutsets_n > 0

let do_requirements = SSet.cardinal !opt_req_automata > 0

let do_reach = not (do_cutsets || do_requirements)


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
	print_endline (string_of_ternary result)

let cutsets n =
	let gA = lcg_for_cutsets env
	in
	prerr_endline ("#nodes = "^string_of_int gA#count_nodes);
	prerr_endline ("#procs = "^string_of_int gA#count_procs);
	prerr_endline ("#objs = "^string_of_int gA#count_objs);

	let exclude_localstate =
		if !opt_cutsets_noinit then
			fun ai -> ctx_has_proc ai ctx
		else fun _ -> false
	in

    let (d_nkp, index_proc) = cutsets gA n exclude_localstate gA#leafs
	in
	let resolve_ps =
		List.map (Hashtbl.find index_proc)
	in
	let string_of_ai = string_of_localstate an
	in
	let print_ps ps =
		let s = String.concat "," (List.map string_of_ai ps)
		in
		print_endline s
	in
	let handle_proc ai =
		try
			let pss = fst (Hashtbl.find d_nkp (NodeProc ai))
			in
			let n = PSSet.cardinal pss
			in
			prerr_endline ("# "^string_of_int n^" cut set(s) for "^string_of_ai ai^":");
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
			List.iter print_ps elts

		with Not_found ->
			print_endline (string_of_node (NodeProc ai)^" is not reachable.");
	in
	List.iter handle_proc env.goal

let requirements automata universal =
	let gA = lcg_for_requirements env
	in
	prerr_endline ("#nodes = "^string_of_int gA#count_nodes);
	prerr_endline ("#procs = "^string_of_int gA#count_procs);
	prerr_endline ("#objs = "^string_of_int gA#count_objs);

	let cout = open_out "/tmp/req-lcg.dot"
	in
	output_string cout (gA#to_dot);
	close_out cout;

    let reqs, index_proc = requirements gA automata gA#leafs universal
	in
	let resolve_ps = List.map (Hashtbl.find index_proc)
	in
	let string_of_ai = string_of_localstate an
	in
	let print_ps ps =
		let s = String.concat " and " (List.map string_of_ai ps)
		in
		print_endline s
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
			prerr_endline ("# "^string_of_int n^" different requirements for "^string_of_ai ai^":");
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
			List.iter print_ps elts

		with Not_found ->
			print_endline (string_of_node (NodeProc ai)^" is not reachable.");
	in
	List.iter handle_proc env.goal

let _ =
	(if do_cutsets then cutsets !opt_cutsets_n);
	(if do_requirements then requirements !opt_req_automata !opt_req_universal);
	(if do_reach then static_reach ())

