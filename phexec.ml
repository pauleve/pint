
open Ui;;

Random.self_init ();;
R.set_seed (Random.bits ()) (Random.bits ());;

let opt_args = ref []
in
let cmdopts = Ui.common_cmdopts
and usage_msg = "ph-exec [opts] <model.ph> <duration>"
and anon_fun arg = opt_args := !opt_args@[arg]
in
Arg.parse cmdopts anon_fun usage_msg;
let phname, duration = match !opt_args with
	  [phname; duration] -> phname, float_of_string duration
	| _ -> (Arg.usage cmdopts usage_msg; raise Exit)
in

let ph, state = Ui.ph_load2 phname
in
Ph_machine.execute (Ph_machine.create_env ph) state duration

