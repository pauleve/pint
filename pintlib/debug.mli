(** Debugging control. *)

(** Set to [true] if debug messages should be displayed. *)
val dodebug : bool ref

(** [dbg msg] prints [msg^"\n"] to standard error channel. *)
val dbg : string -> unit

(** [dbg_noendl] prints [msg] to standard error channel. *)
val dbg_noendl : string -> unit

