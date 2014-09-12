
module SSet : Set.S with type elt = string
module SMap : Map.S with type key  = string
module ISet : Set.S with type elt = int
module IMap : Map.S with type key = int

(** ISet from int list. *)
val iset_of_list : int list -> ISet.t

val string_of_set : 
	?lbracket:string -> ?rbracket:string -> ?delim:string ->
		('a -> string) -> ('b -> 'a list) -> 'b -> string

(** String representation of an int Set. *)
val string_of_iset : ISet.t -> string

type ternary = True | False | Inconc

(** String representation of ternary. *)
val string_of_ternary : ternary -> string


type stochatime = 
	  Instantaneous
	| RateSA of (float * int)
	| FiringInterval of (float*float*float) 
