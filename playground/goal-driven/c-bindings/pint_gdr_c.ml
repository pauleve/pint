
(* pintc -output-obj -o pint_gdr_caml.o pint_gdr_c.ml *)

open PintTypes
open Ph_types

let pint_ctx_set ctx a i = 
	SMap.add a (ISet.singleton i) ctx
let _ = Callback.register "pint_ctx_set" pint_ctx_set

let ph_parse filename =
	Ph_util.parse (open_in filename)
let _ = Callback.register "ph_parse" ph_parse

let ph_init_env ph a i =
	Ph_reach.init_env ph ctx_empty [(a,i)]
let _ = Callback.register "ph_init_env" ph_init_env

let ph_worth_glc env ctx =
	let ctx = Ph_cooperativity.coherent_ctx !Ph_instance.cooperativities ctx
	in
	let env = {env with Ph_reach.ctx = ctx}
	in
	Ph_reach.worth_glc env
let _ = Callback.register "ph_worth_glc" ph_worth_glc

let ph_is_worth glc a i =
	Ph_reach.is_process_worth glc (a,i)

let _ = Callback.register "ph_is_worth" ph_is_worth

