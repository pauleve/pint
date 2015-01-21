
open PintTypes
open LocalCausalityGraph
open AutomataNetwork

let usage_msg = "pint-reach [opts] <local state> # Static analysis for reachability"

and opt_cutsets_n = ref 0

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--cutsets", Arg.Set_int opt_cutsets_n,
			"n\tCompute cutsets up to given maximum cardinality");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let an, ctx = An_cli.process_input ()

let goal = match args with
	  [str_ls] -> [An_cli.parse_local_state an str_ls]
	| _ -> abort ()

let do_cutsets = !opt_cutsets_n > 0

let do_reach = not do_cutsets

let env = An_reach.init_env an ctx goal

let static_reach () =
	let result = An_reach.local_reachability env
	in
	print_endline (string_of_ternary result)

let cutsets n =
	let gA = An_reach.lcg_for_cutsets env
	in
	prerr_endline ("#nodes = "^string_of_int gA#count_nodes);
	prerr_endline ("#procs = "^string_of_int gA#count_procs);
	prerr_endline ("#objs = "^string_of_int gA#count_objs);

	let exclude_localstate _ = false
	in

    let (d_nkp, index_proc) = cutsets gA n exclude_localstate gA#leafs
	in
	let resolve_ps =
		List.map (Hashtbl.find index_proc)
	in
	let string_of_ai = string_of_localstate an
	in
	let print_ps ps =
		let s = String.concat ";" (List.map string_of_ai ps)
		in
		print_endline s
	in
	let handle_proc ai =
		try
			let pss = fst (Hashtbl.find d_nkp (NodeProc ai))
			in
			let n = PSSet.cardinal pss
			in
			prerr_endline ("# "^string_of_int n^" key local state(s) for "^string_of_ai ai^":");
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
	List.iter handle_proc env.An_reach.goal

let _ =
	(if do_cutsets then cutsets !opt_cutsets_n);
	(if do_reach then static_reach ())

