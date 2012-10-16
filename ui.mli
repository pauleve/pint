(** User Interface helpers. *)

(** Reference to the input channel. *)
val opt_channel_in : in_channel ref

(** Reference to the input filename. *)
val opt_filename_in : string ref

(** path to Pint directory *)
val pint_path : string

(** Sets [Ph_util.opt_initial_procs] to the process list represented by the given string. *)
val setup_opt_initial_procs : string -> unit

(** Sets [opt_channel_in] and [opt_filename_in] according to given input filename. *)
val setup_opt_channel_in : string -> unit

(** List of command line options (for use with [Arg] module) common to the majority of tools. *)
val common_cmdopts : (Arg.key * Arg.spec * Arg.doc) list

(** List of command line options (for use with [Arg] module) for tools taking a model as input. *)
val input_cmdopts : (Arg.key * Arg.spec * Arg.doc) list

(** Use of [input_cmdopts] to parse command line. Returns the input process hitting with context and
additionnal command line arguments as string list *)
val simple_input : unit -> ((Ph_types.ph * Ph_types.ctx) * string list)

(** Make a list of processes from a string list alterning sort and process index. *)
val proclist_from_stringlist : string list -> Ph_types.process list

