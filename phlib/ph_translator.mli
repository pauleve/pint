(** Translation of Process Hitting into different formats. *)

type opts = { alpha : float; round_fi : float * float -> int * int; }
val string_of_float0 : float -> string
type piproc_arg_action = ArgReset | ArgUpdate of (string * string) list
val spim_of_ph :
  (Ph_types.SMap.key * int) list *
  (Ph_types.SMap.key * int,
   ((Ph_types.SMap.key * int) * (float * int) option) * int)
  Hashtbl.t -> int Ph_types.SMap.t -> string
val prism_mdp_of_ph :
  (Ph_types.SMap.key * int) list *
  (Ph_types.SMap.key * int, ((Ph_types.SMap.key * int) * 'a) * int) Hashtbl.t ->
  int Ph_types.SMap.t -> string
val prism_of_ph :
  (Ph_types.SMap.key * int) list *
  (Ph_types.SMap.key * int,
   ((Ph_types.SMap.key * int) * (float * int) option) * int)
  Hashtbl.t -> int Ph_types.SMap.t -> string

(** Return a Process Hitting source as a flat action list from the given Process Hitting. *)
val dump_of_ph : Ph_types.ph -> Ph_types.state -> string

val romeo_pid : ('a * int) list * 'b -> 'a * int -> string
val romeo_of_ph :
  opts ->
  (Ph_types.SMap.key * int) list *
  (Ph_types.SMap.key * int,
   ((Ph_types.SMap.key * int) * (float * int) option) * int)
  Hashtbl.t -> int Ph_types.SMap.t -> string
val tina_of_ph :
  'a * (string * int, ((string * int) * 'b) * int) Hashtbl.t ->
  int Ph_types.SMap.t -> string
val biocham_of_ph :
  'a * (string * int, ((string * int) * 'b) * int) Hashtbl.t ->
  int Ph_types.SMap.t -> string
val kappa_of_ph :
  (string * int) list * (string * int, ((string * int) * 'a) * int) Hashtbl.t ->
  int Ph_types.SMap.t -> string
