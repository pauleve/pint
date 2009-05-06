(**
 Generic types
 TODO: move to commonlib
**)

module SSet = Set.Make (struct type t = string let compare = compare end);;

(**
 Process Hitting types
**)

type metaprocess = string * int (* name * level max *)
type process = string * int (* name * level *)
type sa = int option
type rate = float

type hits = (process, ((process * (rate*sa)) * int)) Hashtbl.t

type ph = metaprocess list * hits

type regulation_sign = Positive | Negative
(* gene_a * threshold * sign * gene_b *)
type regulation_t = Regulation of (string * int * regulation_sign * string)

(* (a,i) * (b,j) * j' *)
type hit_t = Hit of (process * process * int)

