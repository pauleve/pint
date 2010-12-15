(** Process Hitting related types. *)

module SSet : Set.S with type elt = string
module SMap : Map.S with type key  = string
module ISet : Set.S with type elt = int
module IMap : Map.S with type key = int

val string_of_set : ('a -> string) -> ('b -> 'a list) -> 'b -> string
val string_of_iset : ISet.t -> string
type ternary = True | False | Inconc
val string_of_ternary : ternary -> string
type sort = string
type sortidx = int
type process = sort * sortidx
val string_of_process : string * int -> string
val string_of_process' : string * int -> string
val string_of_proc : string * int -> string
type rate = (float * int) option
type hits = (process, (process * rate) * int) Hashtbl.t
type ph = process list * hits
type regulation_sign = Positive | Negative
type regulation_t = Regulation of (string * int * regulation_sign * string)
type action = Hit of (process * process * int)
val hitter : action -> process
val target : action -> process
val bounce : action -> int
val bounce2 : action -> sort * int
val string_of_action : action -> string
type state = sortidx SMap.t
val string_of_state : int SMap.t -> string
val state0 : (SMap.key * 'a) list * 'b -> int SMap.t
val merge_state : 'a SMap.t -> (SMap.key * 'a) list -> 'a SMap.t
val state_value : 'a SMap.t -> SMap.key -> 'a
val list_of_state : 'a SMap.t -> (SMap.key * 'a) list
type match_p =
    Any
  | ProcessLevel of process
  | Process of string
  | Matching of (process -> bool)

module PCSet : Set.S with type elt = process * process
module PMap : Map.S with type key = process
module PSet : Set.S with type elt = process
module ActionSet : Set.S with type elt = action

val string_of_processes : PSet.t -> string
val string_of_procs' : PSet.t -> string
val string_of_procs : PSet.t -> string
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

