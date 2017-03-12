% objective per initial state
oa_lcg(G,ls(A,I),obj(A,J,I)) :- oa_lcg(G,_,ls(A,I)), oa_init(G,A,J).

% necessary condition for reachability
:- oa_valid(G,bottom).
oa_valid(G,X) :- oa_lcg(G,X,top).
oa_valid(G,ls(A,I)) :- oa_lcg(G,ls(A,I),X),oa_valid(G,X).
oa_valid(G,obj(A,I,J)) :- not lock(A,I),oa_lcg(G,obj(A,I,J),X),oa_valid(G,X),I!=J.
% trivial objectives
oa_valid(G,obj(A,I,I)) :- oa_lcg(G,obj(A,I,I),X),oa_valid(G,X).

% instantiate reach LCG
oa_lcg(reach,goal,ls(A,I)) :- goal(A,I).
1 { oa_init(reach,A,J) : ls(A,J) } 1 :- oa_lcg(reach, _, ls(A, _)).

% goal is not valid in LCG
:- oa_valid(reach,ls(G,I)),goal(G,I).

oa_init(reach,A,I) :- lock(A,I).
oa_init(reach,A,I) :- init(s0,A,I),not lock(A,_).

ignore(A) :- goal(A,_).

% lock local states
{ lock(A,I) : ls(A,I),not ignore(A) } maxcard.
% at most one lock per automaton
:- lock(A,I),lock(A,J),I!=J.

#show lock/2.

