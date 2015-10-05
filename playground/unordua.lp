
conn(X,Y) :- edge(X,Y).
conn(X,Y) :- edge(X,Z), conn(Z,Y).

% initial objective
edge(ls(A,I),obj(A,J,I)) :- edge(_,ls(A,I)), init(A,J).

% no cycle
:- conn(X,X).

% context saturation
init(A,I) :- edge(n, ls(A,I)), n != root.

% sufficient continuity
edge(obj(A,I,J),obj(A,K,J)) :- not boolean(A), conn(obj(A,I,J),ls(A,K)), J != K.

% synchronisation independence
:- indep(A,I,ls(A,J)), I != J.
indep(A,I,N) :- indep(A,I,ls(B,J)), B != A, edge(ls(B,J),N).
indep(A,I,N) :- indep(A,I,obj(B,J,K)), edge(obj(B,J,K),N).
indep(A,I,N) :- indep(A,I,sol(obj(B,J,K),L)), edge(sol(obj(B,J,K),L),N).

#show edge/2.

