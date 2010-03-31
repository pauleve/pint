
open Debug;;
open Ph_types;;

let opt_method = ref "complete"
and opt_args = ref []
in
let cmdopts = Ui.common_cmdopts @ [
		("--method", Arg.Set_string opt_method, "Method of analysis (complete, execute)");
	]
and usage_msg = "ph-reach [opts] <model.ph> <z> <l>"
and anon_fun arg =
	opt_args := !opt_args@[arg]
in
Arg.parse cmdopts anon_fun usage_msg;
let phname, zl = match !opt_args with
	   [phname;z;l] -> phname, (z, int_of_string l)
	 | _ -> (Arg.usage cmdopts usage_msg; raise Exit)
in
let ph, state = Ui.ph_load2 phname
in
let nb_actions = Ph_op.ph_count_actions ph
in
dbg ("# "^phname^": "^(string_of_int nb_actions)^" actions");
dbg ("# testing reachability of "^string_of_process zl^" from state "^string_of_state state);

(*
let hdepend, _ = Ph_verif.process_reachability_prepare ph zl state
in
Util.dump_to_file (phname^"-hdepend.dot") (Ph_verif.dot_from_hdepend hdepend)
*)

let env = Ph_reach.create_env ph
in
let bpzl = Ph_reach.reach_bounce_path state zl
in
let decision = 
match !opt_method with
	  "complete" -> Ph_reach.process_reachability env zl state
	| "execute" -> Ph_reach.process_reachability_using_execute env bpzl state
	| "test" -> Ph_reach.test env bpzl state
	| _ -> failwith "Unknown method."
in
match decision with
true -> (dbg "# SUCCESS"; exit 0)
| false -> (dbg "# FAILURE"; exit 1)

