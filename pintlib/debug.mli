(** Debugging control. *)

(** Set to [true] if debug messages should be displayed. *)
val dodebug : bool ref

(** Maximum debug level to display *)
val debuglevel : int ref

(** [dbg msg] prints [msg^"\n"] to standard error channel. *)
val dbg : ?level:int -> string -> unit

(** [dbg_noendl] prints [msg] to standard error channel. *)
val dbg_noendl : ?level:int -> string -> unit

