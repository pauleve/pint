
open Big_int_Z

open PintTypes

open LocalCausalityGraph
open AutomataNetwork


let usage_msg = "pint-sg - State graph analyser"


let do_count = ref false
let do_attractors = ref false
let do_description = ref false
let do_states = ref false
let do_sg = ref ""

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--count-reachable", Arg.Set do_count, "\tCount reachable states.");
		("--state-graph", Arg.Set_string do_sg, "graph.dot\tCompute reachable state graph (DOT format).");
		("--reachable-states", Arg.Set do_states, "\tCompute reachable states.");
		("--reachable-attractors", Arg.Set do_attractors, "\tList reachable attractors.");
		("--description", Arg.Set do_description,
			"\tShow basic counts.");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = if args <> [] then abort ()

let an, ctx = An_cli.process_input ()

let count () =
	let state = state_of_ctx ctx
	in
	let nb_states, _, _ = An_stategraph.reachable_states an state
	in
	if !An_cli.opt_json_stdout then
		print_endline (string_of_big_int nb_states)
	else
		print_endline (string_of_big_int nb_states ^ " reachable states.")

let reachable_states () =
	let state = state_of_ctx ctx
	in
    let _, states, state_of_sid = An_stategraph.reachable_states an state
    in
    if !An_cli.opt_json_stdout then (
        let first = ref true
        in
        let output_state sid _ =
            (if !first then first := false else print_string ", ");
            print_endline (json_of_state an (state_of_sid sid))
        in
        print_string "[ ";
        An_stategraph.BigHashtbl.iter output_state states;
        print_endline "]";
    ) else
        let output_state sid _ =
            print_endline (string_of_state an (state_of_sid sid))
        in
        An_stategraph.BigHashtbl.iter output_state states

let stategraph output =
	let state = state_of_ctx ctx
	in
	let sg, sid0, state_of_sid = An_stategraph.reachable_stategraph an state
	in
	let cout = open_out output
	in
	let node_label s =
		String.concat "," (List.map
			(string_of_ls ~protect:false an) (IMap.bindings s))
	in
	let output_tr sid nexts =
		let sfrom = string_of_big_int sid
		in
		output_string cout (sfrom
			^" [label=\"" ^node_label (state_of_sid sid)^"\""
			^(if eq_big_int sid sid0 then ",style=filled" else "")
			^"];\n");
		List.iter (fun sid' ->
			output_string cout (sfrom^" -> "
				^string_of_big_int sid'^";\n")) nexts
	in
	output_string cout "digraph SG {\n";
	An_stategraph.BigHashtbl.iter output_tr sg;
	output_string cout "}";
	close_out cout

let attractors () =
	let state = state_of_ctx ctx
	in
	let bsccs, state_of_sid = An_stategraph.attractors an state
	in
	if !An_cli.opt_json_stdout then (
		let json_of_attractor (sid, size) =
			let desc =
				("type", json_of_str (if size = 1 then "fixpoint" else "cyclic"))
				::("size", json_of_int size)
				::("sample", json_of_state an (state_of_sid sid))
				::[]
			in
			json_of_bindings (fun k v -> json_of_str k, v) desc
		in
		print_endline (json_of_list json_of_attractor bsccs)
	) else (
	print_endline (string_of_int (List.length bsccs)^" reachable attractors.");
	List.iter (fun (sid, size) ->
		if size = 1 then
			print_endline ("- fixed point:\n\t"^string_of_state an (state_of_sid sid))
		else
			print_endline ("- cycle between "
				^ string_of_int (size) ^ " states, including:\n\t"
				^ string_of_state an (state_of_sid sid))) bsccs
	)

let description () =
	let nb_automata = count_automata an
	and nb_ls = Hashtbl.fold (fun _ -> (+)) an.ls 0
	and nb_tr = count_transitions an
	and nb_states = Hashtbl.fold (fun _ -> mult_int_big_int) an.ls unit_big_int
	and largest = Hashtbl.fold (fun _ -> max) an.ls 0
	in
	if !An_cli.opt_json_stdout then (
		let desc =
			("nb_automata", json_of_int nb_automata)
			::("nb_local_states", json_of_int nb_ls)
			::("max_local_states", json_of_int largest)
			::("nb_transitions", json_of_int nb_tr)
			::("nb_states", string_of_big_int nb_states)
			::[]
		in
		print_endline (json_of_bindings (fun k v -> json_of_str k,v) desc)
	) else (
	print_endline (string_of_int nb_automata^" automata");
	print_endline (string_of_int nb_ls^" local states");
	print_endline (string_of_int largest^" max local states per automaton");
	print_endline (string_of_int nb_tr^" transitions");
	print_endline (string_of_big_int nb_states^" states")
	)

let _ =
	if !do_count then count ();
	if !do_states then reachable_states ();
	if !do_sg <> "" then stategraph !do_sg;
	if !do_attractors then attractors ();
	if !do_description then description ()

