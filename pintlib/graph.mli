type ('a, 'b) t = ('a, 'b * 'a) Hashtbl.t
val create : int -> ('a, 'b) t
val add : ('a, 'b) t -> 'a -> 'b * 'a -> unit
val get : ('a, 'b) t -> 'a -> ('b * 'a) list
val to_dot : ('a, 'b) t -> ('a -> string) -> ('b -> string) -> string
