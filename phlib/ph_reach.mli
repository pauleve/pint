(**
Process reachability by static analysis.
*)

type objective = Ph_types.sort * Ph_types.sortidx * Ph_types.sortidx

(** [process_reachability ph p s]
returns the semi-decision (ternary) of the reachability of process [p] from state [s]
in the process hitting [ph].
*)
val process_reachability : Ph_types.ph -> Ph_types.process -> Ph_types.state -> Ph_types.ternary

