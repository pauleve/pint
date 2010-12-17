val fill_state : Ph_types.state -> Ph_types.process list -> Ph_types.state
val opt_initial_procs : Ph_types.process list ref
val parse : in_channel -> Ph_types.ph * Ph_types.state
val matching : Ph_types.ph -> (Ph_types.action -> bool) -> Ph_types.action list

val count_states : Ph_types.ph -> Big_int.big_int
val count_sorts : Ph_types.ph -> int
val count_processes : Ph_types.ph -> int
val count_actions : Ph_types.ph -> int

