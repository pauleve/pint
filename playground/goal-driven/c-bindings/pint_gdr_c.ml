
(* pintc -output-obj -o pint_gdr_caml.o pint_gdr_c.ml *)

open Ph_types

let ph_parse filename =
	Ph_util.parse (open_in filename)

let init_env ph a i =
	Ph_reach.init_env ph ctx_empty [(a,i)]

let worth_glc env ctx =
	let ctx = Ph_cooperativity.coherent_ctx !Ph_instance.cooperativities ctx
	in
	let env = {env with Ph_reach.ctx = ctx}
	in
	Ph_reach.worth_glc env

let _ = Callback.register "ph_parse" ph_parse
let _ = Callback.register "ph_init_env" init_env
let _ = Callback.register "ph_worth_glc" worth_glc

