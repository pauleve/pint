
type 'a vertex = Value of float | Var of 'a

type edge = Equal | FactorEqual of float

type 'a t = ('a vertex, edge) Graph.t
;;

let create = Graph.create;;
let add (cs:t) = Graph.add cs;;
let get (cs:t) = Graph.get cs;;


