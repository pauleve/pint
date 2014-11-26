
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

let push_ph stack ph =
	let state = ph, Ph_instance.copy ()
	in
	Stack.push state stack

let top_ph stack =
	let ph, instance = Stack.top stack
	in
	Ph_instance.restore instance;
	ph

let pop_ph stack =
	ignore(Stack.pop stack)

let init_env ph ctx goal =
	Ph_reach.init_env ph ctx [goal]

let worth_glc env ctx =
	let ctx = Ph_cooperativity.coherent_ctx !Ph_instance.cooperativities ctx
	in
	let env = {env with Ph_reach.ctx = ctx}
	in
	Ph_reach.worth_glc env

