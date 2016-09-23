
% associate local state with condition on the cut
1 { cut(C) : h(C,P) } 1 :- reach(A,I),name(P,A,I).

% select parent event
e(E) :- cut(C),edge(E,C).
% select parent events
e(F) :- edge(F,C),edge(C,E),e(E).

% cut constraint
:- cut(C),edge(C,E),e(E).
%cut(C) :- e(E),edge(E,C),edge(C,F),not e(F).

% conflicts (optional?)
:- edge(C,E),edge(C,F),e(E),e(F),E != F.


