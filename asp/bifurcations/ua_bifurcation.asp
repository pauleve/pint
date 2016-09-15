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
1 { btr(ID,A,I,J) : tr(ID,A,I,J) } 1.
btrcond(ID,B,K) :- btr(ID,A,I,J),trcond(ID,B,K).
#show btr/4.
#show btrcond/3.
oa_init(ucont,A,J) :- btr(ID,A,I,J).
K=L :- btr(ID,_,_,_),trcond(ID,B,K),oa_init(ucont,B,L).

%
% Check that the state before btr can reach the input goal
%
1 { init(sic,A,J) : ls(A,J) } :- ua_lcg(sic, _, ls(A, _)).
init(sic,A,I) :- oa_init(ucont,A,I),btr(_,B,_,_),B!=A.
init(sic,A,I) :- btr(ID,_,_,_),trcond(ID,A,I).
init(sic,A,I) :- btr(ID,A,I,_).
ua_lcg(sic,goal,ls(A,I)) :- goal(A,I).
:- goal(A,I),init(sic,A,I). % optional, but makes it faster

% choose a single initial state
1 {sb(A,I) : init(sic,A,I)} 1 :- init(sic,A,_).
sb(A,I) :- oa_init(ucont,A,I),btr(_,B,_,_),B!=A.
sb(A,I) :- btr(ID,_,_,_),trcond(ID,A,I).
sb(A,I) :- btr(ID,A,I,_).

