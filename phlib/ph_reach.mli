(**
Process reachability by static analysis.
*)

type objective_seq = Ph_types.objective list

(** returns a string representation of the given objective sequence. *)
val string_of_objseq : objective_seq -> string

(** returns the objective sequences corresponding to the successive rechability of given processes in the given state. *)
val objseq_from_procseq : Ph_types.state -> Ph_types.process list -> objective_seq

(** [process_reachability ph s w]
returns the semi-decision (ternary) of the concretizability of objective sequence [w] in the state [s] in the process hitting [ph].
*)
val process_reachability : Ph_types.ph -> Ph_types.state -> objective_seq -> Ph_types.ternary

val test : Ph_types.ph -> Ph_types.state -> objective_seq -> Ph_types.ternary

