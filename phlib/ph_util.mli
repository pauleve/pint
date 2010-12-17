(** Process Hitting helpers. *)

(** Reference to the process list that should be present in the initial state. *)
val opt_initial_procs : Ph_types.process list ref

(** Parse a Process Hitting and its initial state from given input channel.
	Overwrite the initial state with the processes referenced by [opt_initial_procs]. *)
val parse : in_channel -> Ph_types.ph * Ph_types.state

(** Returns the total number of states of the given Process Hitting. *)
val count_states : Ph_types.ph -> Big_int.big_int

(** Returns the  number of sorts in the given Process Hitting. *)
val count_sorts : Ph_types.ph -> int

(** Returns the  number of processes in the given Process Hitting. *)
val count_processes : Ph_types.ph -> int

(** Returns the  number of actions in the given Process Hitting. *)
val count_actions : Ph_types.ph -> int

