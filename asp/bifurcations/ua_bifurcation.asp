% objective per initial state
oa_lcg(G,ls(A,I),obj(A,J,I)) :- oa_lcg(G,_,ls(A,I)), oa_init(G,A,J).

% necessary condition for reachability
:- oa_valid(G,bottom).
oa_valid(G,X) :- oa_lcg(G,X,top).
oa_valid(G,ls(A,I)) :- oa_lcg(G,ls(A,I),X),oa_valid(G,X).
oa_valid(G,obj(A,I,J)) :- oa_lcg(G,obj(A,I,J),X),oa_valid(G,X).
%oa_valid(G,goal) :- oa_valid(G,X), oa_lcg(G,goal,X).

%
% Ucont
%
oa_lcg(ucont,goal,ls(A,I)) :- goal(A,I).
1 { oa_init(ucont,A,J) : ls(A,J) } 1 :- oa_lcg(ucont, _, ls(A, _)).
%oa_lcg(ucont,ls(A,I),obj(A,J,I)) :- oa_lcg(ucont,_,ls(A,I)),oa_init(ucont,A,J),tr(_,A,_,J), not init(s0,A,J).
%oa_lcg(ucont,ls(A,I),obj(A,J,I)) :- oa_lcg(ucont,_,ls(A,I)),oa_init(ucont,A,J),init(s0,A,J).

% ucont should not reach goal
:- oa_valid(ucont,ls(G,I)),goal(G,I).


% select one bifurcation transition
1 { btr(T) : tr(T) } 1.
#show btr/1.
oa_init(ucont,A,J) :- btr(T), post(T,A,J).
:- btr(T), post(T,B,K), oa_init(ucont,B,L), L != K.

%
% Check that the state before btr can reach the input goal
%
1 { init(sic,A,J) : ls(A,J) } :- ua_lcg(sic, _, ls(A, _)).
init(sic,A,I) :- btr(T), pre(T,A,I).
ba(A) :- btr(T), pre(T,A,_).
init(sic,A,I) :- oa_init(ucont,A,I), not ba(A).
ua_lcg(sic,goal,ls(A,I)) :- goal(A,I).
:- goal(A,I),init(sic,A,I). % optional, but makes it faster

% choose a single initial state
1 {sb(A,I) : init(sic,A,I)} 1 :- init(sic,A,_).
sb(A,I) :- oa_init(ucont,A,I), not ba(A).
sb(A,I) :- btr(T), pre(T,A,I).

