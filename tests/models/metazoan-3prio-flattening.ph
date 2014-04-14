
process a 1
process c 1
process f 1

(* actions on c *)
COOPERATIVITY([f;a] -> c 0 1, [[1;1]])
a 0 -> c 1 0

(* actions on a *)
COOPERATIVITY([f;c] -> a 0 1, [[1;0]])
c 1 -> a 1 0

(*
(* actions on f *)
f 1 -> f 1 0
f 0 -> c 1 0
*)

initial_state f 1, c 0, a 0

