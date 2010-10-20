val prepend : 'a -> 'a list list -> 'a list list
val cross : 'a list list -> 'a list -> 'a list list
val cross_list : 'a list list -> 'a list list
val list_union : 'a list -> 'a list -> 'a list
val list_prepend_if_new : 'a -> 'a list -> 'a list
val list_uniq : 'a list -> 'a list
val list_uniq2 : 'a list list -> 'a list list
val list_intersection : 'a list -> 'a list -> 'a list
val list_remove : 'a -> 'a list -> 'a list
val list_replace : 'a -> 'a -> 'a list -> 'a list
val list_sub : 'a list -> 'a list -> 'a list
val index_of : 'a -> 'a list -> int
val subset : 'a list -> 'a list -> bool
val rrange : int -> int -> int list
val range : int -> int -> int list
val dump_to_file : string -> string -> unit
val count_elements : 'a list -> ('a * int) list
val string_apply : string -> string -> string list -> string
val string_of_float0 : float -> string
exception No_choice
val cross_forward :
  ('a list -> 'b) * ('b -> 'b -> 'b) * ('b -> bool) -> 'a list list -> 'b
