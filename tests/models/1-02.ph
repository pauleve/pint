(*
[1] Transformation
  vers 
sur-approximation avec séquestration
*)



process a 1
process b 1
process bp 1

process z 1



COOPERATIVITY([a;b] -> bp 0 1, [[1;1]])

COOPERATIVITY([b;bp] -> z 0 1, [[1;0]])



initial_state a 1, b 1, bp 0, z 0
