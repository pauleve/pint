(** User Interface helpers. *)

val common_cmdopts : (string * Arg.spec * string) list

val setup_opt_initial_procs : string -> unit

val input_cmdopts : (string * Arg.spec * string) list

val opt_channel_in : in_channel ref
val opt_filename_in : string ref
val setup_opt_channel_in : string -> unit

