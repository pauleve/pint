val fill_state : Ph_types.state -> Ph_types.process list -> Ph_types.state
val opt_initial_procs : Ph_types.process list ref
val parse : in_channel -> Ph_types.ph * Ph_types.state
val matching : Ph_types.ph -> (Ph_types.action -> bool) -> Ph_types.action list
