(**

Metazoan segmentation.

Bibliography:
* Francois, P.; Hakim, V. & Siggia, E. D. Deriving structure from evolution: metazoan segmentation.
  Mol Syst Biol, EMBO and Nature Publishing Group, 2007, 3.
* Loïc Paulevé, Morgan Magnin, and Olivier Roux. Refining dynamics of gene regulatory networks in a
  stochastic π-calculus framework. In Transactions on Computational Systems Biology XIII, volume 6575
  of Lecture Notes in Comp Sci, pages 171-191. Springer, 2011
  http://dx.doi.org/10.1007/978-3-642-19748-2_8

**)



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
