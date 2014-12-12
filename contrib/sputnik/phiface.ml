
open PintTypes
open Ph_types

let test () =
	print_endline ("Pint - Sputnik");
	true

let dump_ph outf ph ctx =
	let data = Ph_translator.dump_of_ph ph ctx
	in
	let cout = open_out outf
	in
	output_string cout data;
	close_out cout

let init_stack =
	Stack.create

let push_ph stack ph glc =
	let state = Ph_util.ph_copy ph, glc, Ph_instance.copy ()
	in
	Stack.push state stack

let pop_ph stack =
	let ph, glc, instance = Stack.pop stack
	in
	Ph_instance.restore instance;
	ph, glc

let worth_glc ph ctx goal =
	let ctx = Ph_cooperativity.coherent_ctx !Ph_instance.cooperativities ctx
	in
	let env = Ph_reach.init_env ph ctx [goal]
	in
	Ph_reach.worth_glc env

let thomas_constrain ph ctx a bound min_bound =
	let ig = !Ph_instance.interaction_graph
	and ps = fst ph
	in
	let actx = InteractionGraph.IG.extended_local_ctx ig ps a ctx
	and is = if min_bound then Util.range bound (List.assoc a ps)
				else Util.range 0 bound
	in
	Ph_thomas.constrained_ph ph ig a actx is

