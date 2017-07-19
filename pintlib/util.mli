(** OCaml helpers. *)

(** String representation of the given float, ensuring there is always a number after the dot (e..g, 1. is represented as "1.0"). *)
val string_of_float0 : float -> string

(** [string_of_list str_of_el l] returns string representation of the given list [l] using the element to string function [str_of_el]. *)
val string_of_list : ('a -> string) -> 'a list -> string

(** [opt_default default opt] returns [x] if opt is [Some x], otherwise returns [default]. *)
val opt_default : 'a -> 'a option -> 'a

(** [dump_to_file filename str] dumps [str] to [filename]. *)
val dump_to_file : string -> string -> unit

(** [list_remove a l] removes all [a] occurences in [l]. *)
val list_remove : 'a -> 'a list -> 'a list

(** [list_lassoc b l] acts like List.assoc with flipped key/value. *)
val list_lassoc : 'a -> ('b * 'a) list -> 'b

(** [index_of a l] returns the first index of [a] occurrence in [l].
	Raises [Not_found] if [a] is not present in [l]. *)
val index_of : 'a -> 'a list -> int

(** [range a b] returns the list of successive elements from a to b [[a;a+1;..;b]]. *)
val range : int -> int -> int list

(** Same as [range] but in the reverse order. *)
val rrange : int -> int -> int list

val srange : int -> int -> PintTypes.ISet.t

val hashtbl_filter_bindings : ('a, 'b) Hashtbl.t -> 'a -> ('b -> bool) -> unit

(** [string_apply match data values] replaces successively the n-th match of [match] in data with the n-th elements of [values]. *)
val string_apply : string -> string -> string list -> string

(** [cross [l1;..;ln] [e1;..;ek]] returns [e1::l1;..;ek::l1;..;e1::ln;..;ek::ln]. *)
val cross : 'a list list -> 'a list -> 'a list list

(** [cross [l1;..;ln]] returns the cartesian product [l1 x .. x ln]. *)
val cross_list : 'a list list -> 'a list list

exception Empty
(** [cross_forward (handler, merger, stopper) [l1;..;ln]]
	applies on-the-fly [handler] to each list in the cartesian product [l1 x .. x ln].
	All [handler] results are aggregated using [merger].
	Computation stops as soon as [stopper] evaluates the result to [False].
	Raises [Empty] if the cartesian product gives no element. *)
val cross_forward : ('a list -> 'b) * ('b -> 'b -> 'b) * ('b -> bool) -> 'a list list -> 'b


val stream_permutations : 'a list -> ('a list) Stream.t

(** [smap_remove_keys smap keys] returns smap without the bindings from keys. *)
val smap_remove_keys : 'a PintTypes.SMap.t -> PintTypes.SSet.t -> 'a PintTypes.SMap.t

(** [smap_subset equal m1 m2] returns true if and only if all bindings in [m1] are
in [m2]. *)
val imap_subset : ('a -> 'b -> bool) -> 'a PintTypes.IMap.t -> 'b PintTypes.IMap.t -> bool

