(**
 Generic types
 TODO: move to commonlib
**)

module SSet = Set.Make (struct type t = string let compare = compare end);;
module SMap = Map.Make (struct type t = string let compare = compare end);;

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

(* Proc level couple set *)
module PCSet = Set.Make (struct type t = (process * process) 
		let compare (ai,bj) (ai',bj') = 
			let order_couple (x,y) =
				if compare x y <= 0 then (x,y) else (y,x)
			in
			let ai,bj = order_couple (ai,bj)
			and ai',bj' = order_couple (ai',bj')
			in
			let c = compare ai ai'
			in
			if c = 0 then compare bj bj' else c
		end)
;;

module PMap = Map.Make (struct type t = process let compare = compare end);;


