(** Process Hitting related types and associated operations. *)

type sort = string
type sortidx = int
type process = sort * sortidx
module PSet : Set.S with type elt = process
module PMap : Map.S with type key = process
module PCSet : Set.S with type elt = process * process

(** Converts a list of processus to a set of processes. *)
val procs_of_ps : process list -> PSet.t

(** String representation of a process. *)
val string_of_proc : process -> string

(** PINT string representation of a process. *)
val pintstring_of_proc : process -> string

(** String representation of a process Set. *)
val string_of_procs : PSet.t -> string


(**
	RateSA (rate, stochasticity_absorption) or
	FiringInterval (d1, d2, confidence_coefficient)
*)
type rate = (float * int) option

(** Returns the rate and stochasticity absorption factor for the given parameter or None if the rate is infinite. *)
val rsa_of_stochatime : PintTypes.stochatime -> rate

type hits = (process, (process * PintTypes.stochatime) * sortidx) Hashtbl.t
type ph = process list * hits

type action = Hit of (process * process * int)
module ASet : Set.S with type elt = action

(** String representation of an action. *)
val string_of_action : action -> string

(** PINT string representation of an action. *)
val pintstring_of_action : action -> string

(** String representation of a list of actions. *)
val string_of_actions : action list -> string

(** Returns the hitter process involved in the given action. *)
val hitter : action -> process

(** Returns the target process involved in the given action. *)
val target : action -> process

(** Returns the bounce process index involved in the given action. *)
val bounce : action -> int

(** Returns the bounce process involved in the given action. *)
val bounce2 : action -> process


type state = sortidx PintTypes.SMap.t

(** String representation of a state. By default, state *)
val string_of_state : ?show_zero:bool -> state -> string

(** Returns the state where all sorts have the process of index 0 present. *)
val state0 : process list -> state

(** [merge_states dest orig] sets the processes in the state [orig] in the state [dest]. *)
val merge_states : state -> state -> state

(** Sets the processes in the given process list present in the given state. *)
val merge_state_with_ps : state -> process list -> state

(** [state_value s a] returns the process index of sort a present in s. *)
val state_value : state -> sort -> sortidx

(** Converts a set into a list of processes. *)
val list_of_state : state -> process list

(** Converts a state to a set of processes. *)
val procs_of_state : state -> PSet.t

(** Empty state *)
val state_empty : state


(** Context type *)
type ctx = PintTypes.ISet.t PintTypes.SMap.t

(** Check ctx equality *)
val ctx_equal : ctx -> ctx -> bool

(** Empty context *)
val ctx_empty : ctx

(** Returns the set of sorts in the context *)
val ctx_sorts : ctx -> PintTypes.SSet.t

(** Get indexes of the given sort in the given context. *)
val ctx_get : sort -> ctx -> PintTypes.ISet.t

(** Same as previous but return empty set if sort is not present in the context keys. *)
val ctx_safe_get : sort -> ctx -> PintTypes.ISet.t

(** [ctx_has_proc p ctx] retruns true iff [p] is present in [ctx]. *)
val ctx_has_proc : process -> ctx -> bool

(** [ctx_add_proc p ctx] returns the context where [p] has been added. *)
val ctx_add_proc : process -> ctx -> ctx

(** [ctx_rm_proc p ctx] returns the context where [p] has been removed. *)
val ctx_rm_proc : process -> ctx -> ctx

(** String representation of a context. *)
val string_of_ctx : ctx -> string

(** Converts a context to a set of processes. *)
val procs_of_ctx : ctx -> PSet.t

(** Converts a set of processes to a context. *)
val procs_to_ctx : PSet.t -> ctx

(** Converts a context to a state. Raise Invalid_arg if there exists more than one process per sort.
*)
val state_of_ctx : ctx -> state

(** Context overriding. *)
val ctx_override_by_ctx : ctx -> ctx -> ctx

(** Context overriding. *)
val ctx_override : ctx -> PSet.t -> ctx

(** Union between contexts. *)
val ctx_union : ctx -> ctx -> ctx

(** Intersection between contexts. *)
val ctx_inter : ctx -> ctx -> ctx

(** State to context. *)
val ctx_of_state : state -> ctx


(** Returns the list of sorts defined in the given Process Hitting.*)
val ph_sigma : ph -> sort list

(** Directives of a Process Hitting model *)
type t_directive = {
  mutable default_rate : float option;
  mutable default_sa : int;
  mutable sample : float;
}
(** Default directives *)
val directive : t_directive

type objective = sort * sortidx * sortidx
module ObjSet : Set.S with type elt = objective
module ObjMap : Map.S with type key = objective

(** String representation of the given objective. *)
val string_of_obj : objective -> string

(** Sort of the objective. *)
val obj_sort : objective -> sort

(** Process index of the objective target. *)
val obj_target : objective -> sortidx

(** Process index of the objective bounce. *)
val obj_bounce : objective -> sortidx

(** Bounce process of the objective. *)
val obj_bounce_proc : objective -> process

(** [obj_reach s p] returns the objective of the reachability of process [p] from the state [s]. *)
val obj_reach : state -> process -> objective


type anostate = int list

type state_matching_t =
	  SM of (string list * anostate list)
	| SM_Not of state_matching_t
	| SM_And of (state_matching_t * state_matching_t)
	| SM_Or of (state_matching_t * state_matching_t)
;;

