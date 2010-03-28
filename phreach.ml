
open Debug;;

open Ph_types;;

let phname = Sys.argv.(1)
let zl = Sys.argv.(2), int_of_string (Sys.argv.(3));;

let ph, state = Ui.ph_load2 phname;;
let nb_actions = Ph_op.ph_count_actions ph;;

dbg (phname^": "^(string_of_int nb_actions)^" actions");;
dbg ("# testing reachability of "^string_of_process zl^" from state "^string_of_state state);;

(*
let hdepend, _ = Ph_verif.process_reachability_prepare ph zl state
in
Util.dump_to_file (phname^"-hdepend.dot") (Ph_verif.dot_from_hdepend hdepend)
*)

let env = Ph_reach.create_env ph
in
let decision = Ph_reach.process_reachability env zl state
in
match decision with
  true -> (dbg "# SUCCESS"; exit 0)
| false -> (dbg "# FAILURE"; exit 1)

