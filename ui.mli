val ph_load : string -> Ph_types.ph
val ph_load2 : string -> Ph_types.ph * Ph_types.sortidx Ph_types.SMap.t
val ph_stable_states :
  (Ph_types.sort * int) list *
  (Ph_types.process, (Ph_types.process * 'a) * 'b) Hashtbl.t ->
  Ph_types.process list list
val ph_count_states : ('a * int) list * 'b -> Big_int.big_int
val ph_count_sorts : 'a list * 'b -> int
val ph_count_processes : ('a * int) list * 'b -> int
val ph_count_actions : 'a * ('b, 'c) Hashtbl.t -> int
val setup_opt_initial_procs : string -> unit
val common_cmdopts : (string * Arg.spec * string) list
val opt_channel_in : in_channel ref
val opt_filename_in : string ref
val setup_opt_channel_in : string -> unit
val input_cmdopts : (string * Arg.spec * string) list
