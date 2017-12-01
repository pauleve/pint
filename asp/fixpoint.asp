
% select one state for each automaton
1 {fp(A, I) : ls(A,I)} 1  :- automaton(A).

% none precondition applies
:- tr(T); fp(A,I) : pre(T,A,I).

#show fp/2.

