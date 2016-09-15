%
% reachability under-approximation for init(sic)
%

% build ua-LCG from s0 to sb
ua_lcg(s0,goal,ls(A,I)) :- sb(A,I).
indep(s0,root,A,I,ls(B,J)) :- sb(A,I),sb(B,J),B != A.

