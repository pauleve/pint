(** Process Hiting **)

type metaprocess = string * int (* name * level max *)
type process = string * int (* name * level *)
type rate = RateInf | Rate of float

type hits = (process, ((process * rate) * int)) Hashtbl.t

type ph = metaprocess list * hits

