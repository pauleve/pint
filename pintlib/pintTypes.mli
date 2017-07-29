
(** Identity *)
val id : 'a -> 'a

module SSet : Set.S with type elt = string
module SMap : Map.S with type key  = string
module ISet : Set.S with type elt = int
module IMap : Map.S with type key = int

val set_of_list : 'a -> ('b -> 'a -> 'a) -> 'b list -> 'a

(** ISet from int list. *)
val iset_of_list : int list -> ISet.t

val string_of_list :
	?lbracket:string -> ?rbracket:string -> ?delim:string ->
		('a -> string) -> 'a list -> string

val string_of_set :
	?lbracket:string -> ?rbracket:string -> ?delim:string ->
		('a -> string) -> ('b -> 'a list) -> 'b -> string

val string_of_map :
	?lbracket:string -> ?rbracket:string -> ?delim:string ->
		('a * 'b -> string) -> (('a -> 'b -> string -> string) -> 't -> string
		-> string) -> 't -> string

(** String representation of an int Set. *)
val string_of_iset : ISet.t -> string

(** String representation of a string Set. *)
val string_of_sset : SSet.t -> string

val map_of_bindings : ('a -> 'b -> 'c -> 'c) -> 'c -> ('a * 'b) list -> 'c

type id = int

(** Returns a unique identifier (not thread-safe) *)
val new_id : unit -> int

type 'a reg_t = {
    elt2id: ('a, id) Hashtbl.t;
    id2elt: (id, 'a) Hashtbl.t;
}

val new_reg : int -> 'a reg_t

val register_elt : 'a reg_t -> 'a -> id

type ternary = True | False | Inconc

(** String representation of ternary. *)
val string_of_ternary : ternary -> string

(** Json representation of ternary *)
val json_of_ternary : ternary -> string

(** Json representation of string *)
val json_of_str : string -> string

(** Json representation of int *)
val json_of_int : int -> string

(** Json represenetation of list *)
val json_of_list : ('a -> string) -> 'a list -> string

(** Json representation of bindings *)
val json_of_bindings : ('a -> 'b -> string * string) ->
		('a*'b) list -> string

type stochatime = 
	  Instantaneous
	| RateSA of (float * int)
	| FiringInterval of (float*float*float) 

