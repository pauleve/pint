(** Process Hitting related types. *)

module SSet : Set.S with type elt = string
module SMap : Map.S with type key  = string
module ISet : Set.S with type elt = int
module IMap : Map.S with type key = int

val string_of_set : ('a -> string) -> ('b -> 'a list) -> 'b -> string

(** String representation of an int Set *)
val string_of_iset : ISet.t -> string

type ternary = True | False | Inconc

(** String representation of ternary *)
val string_of_ternary : ternary -> string

type sort = string
type sortidx = int
type process = sort * sortidx
module PSet : Set.S with type elt = process
module PMap : Map.S with type key = process

(** Retruns string representation of a process *)
val string_of_proc : process -> string

(** Returns string representation of a process Set *)
val string_of_procs : PSet.t -> string

type rate = (float * int) option
type hits = (process, (process * rate) * int) Hashtbl.t
type ph = process list * hits

type action = Hit of (process * process * int)

(** String representation of an action *)
val string_of_action : action -> string

val hitter : action -> process
val target : action -> process
val bounce : action -> int
val bounce2 : action -> sort * int

type state = sortidx SMap.t

(** String representation of a state *)
val string_of_state : int SMap.t -> string

val state0 : (SMap.key * 'a) list * 'b -> int SMap.t
val merge_state : 'a SMap.t -> (SMap.key * 'a) list -> 'a SMap.t

(** [state_value s a] returns the process index of sort a present in s *)
val state_value : state -> sort -> sortidx

(** Converts a set into a list of processes *)
val list_of_state : state -> process list

type regulation_sign = Positive | Negative
type regulation_t = Regulation of (string * int * regulation_sign * string)
type match_p =
    Any
  | ProcessLevel of process
  | Process of string
  | Matching of (process -> bool)

module PCSet : Set.S with type elt = process * process
module ActionSet : Set.S with type elt = action

val uniqise_actions : ActionSet.elt list -> ActionSet.elt list
val string_of_actions : action list -> string
val string_of_actionset : ActionSet.t -> string
val ph_sigma : ('a * 'b) list * 'c -> 'a list
type t_directive = {
  mutable default_rate : float option;
  mutable default_sa : int;
  mutable sample : float;
}
val directive : t_directive

type objective = sort * sortidx * sortidx
module ObjSet : Set.S with type elt = objective
module ObjMap : Map.S with type key = objective

(** String representation of the given objective *)
val string_of_obj : objective -> string

