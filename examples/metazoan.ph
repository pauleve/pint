directive sample 40.

process a 1 process c 1 process f 1
process fc 3 (* cooperative sort {f,c} *)
c 1 -> fc 0 1 @5.
c 1 -> fc 2 3 @5.
c 0 -> fc 1 0 @10.
c 0 -> fc 3 2 @5.
f 1 -> fc 0 2 @10.
f 1 -> fc 1 3 @10.
f 0 -> fc 2 0 @0.1
f 0 -> fc 3 1 @0.1

(* actions on c *)
fc 2 -> c 0 1 @0.5~50 (* only if (f1,c0) *)
c 1 -> c 1 0 @0.5~50
(* actions on a *)
fc 2 -> a 0 1 @1.~50 (* only if (f1,c0) *)
c 1 -> a 1 0 @1.~50
(* actions on f *)
f 1 -> f 1 0 @0.034~100 (* auto-off *)
f 0 -> c 1 0 @0.1

initial_state f 1, c 0, a 0
