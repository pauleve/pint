
open Big_int

open PintTypes

open Ph_types

open LocalCausalityGraph
open AutomataNetwork


let usage_msg = "pint-sg - State graph analyser"


let do_count = ref false
let do_attractors = ref false
let do_description = ref false

let cmdopts = An_cli.common_cmdopts @ An_cli.input_cmdopts @ [
		("--count-reachable", Arg.Set do_count, "\tCount reachable states.");
		("--reachable-attractors", Arg.Set do_attractors, "\tList reachable attractors.");
		("--description", Arg.Set do_description,
			"\tShow basic counts.");
	]

let args, abort = An_cli.parse cmdopts usage_msg

let _ = if args <> [] then abort ()

let an, ctx = An_cli.process_input ()

let count () =
	let state = Ph_types.state_of_ctx ctx
	in
	let nb_states, _ = An_stategraph.reachable_states an state
	in
	print_endline (Big_int.string_of_big_int nb_states ^ " reachable states.")

let attractors () =
	let state = Ph_types.state_of_ctx ctx
	in
	let bsccs, state_of_sid = An_stategraph.attractors an state
	in
	print_endline (string_of_int (List.length bsccs)^" reachable attractors.");
	List.iter (fun (sid, size) ->
		if size = 1 then
			print_endline ("- fixed point:\n\t"^string_of_state an (state_of_sid sid))
		else
			print_endline ("- cycle between "
				^ string_of_int (size) ^ " states, including:\n\t"
				^ string_of_state an (state_of_sid sid))) bsccs

let description () =
	let nb_automata = count_automata an
	and nb_ls = count_local_states an
	and nb_tr = count_transitions an
	and nb_states = count_states an
	and largest = Hashtbl.fold (fun _ def n ->
		max n (List.length def)) an.automata 0
	in
	print_endline (string_of_int nb_automata^" automata");
	print_endline (string_of_int nb_ls^" local states");
	print_endline (string_of_int largest^" max local states per automaton");
	print_endline (string_of_int nb_tr^" transitions");
	print_endline (string_of_big_int nb_states^" states")

let _ =
	if !do_count then count ();
	if !do_attractors then attractors ();
	if !do_description then description ()

