%%% Vérification de la propriété 4 : bien-formé pour l'inférence des paramètres

ext_cooperation(A,AI,CS,CSJ) :- cooperation(CS,A,AI,CSJ).
ext_cooperation(A,AI,CSB,CSBJ) :- ext_cooperation(A,AI,CSA,CSAJ),
  ext_cooperation(CSA,CSAJ,CSB,CSBJ).
differs_ext_cooperation(A,CS,CSI,CSJ) :- ext_cooperation(A,AI,CS,CSI),
  ext_cooperation(A,AJ,CS,CSJ), AI!=AJ.
not_same_ext_cooperation(B,CS,CSI,CSJ) :- ext_cooperation(B,BI,CS,CSI),
  ext_cooperation(B,BJ,CS,CSJ), differs_ext_cooperation(A,CS,CSI,CSJ),
  A!=B.
same_ext_cooperation(B,CS,CSI,CSJ) :- ext_cooperation(B,BI,CS,CSI),
  ext_cooperation(B,BJ,CS,CSJ), not not_same_ext_cooperation(B,CS,CSI,CSJ).

%new_not_valid(B,C,A,BI,BJ,CI,CJ,AK,AL)
:- is_ig_predec(B,A),
  is_ph_predec(C,A), is_ph_pred(B,C), is_cs(C),
  levels(B,A,BI), levels(B,A,BJ),
  ext_cooperation(B,BI,C,CI), ext_cooperation(B,BJ,C,CJ),
  same_ext_cooperation(B,C,CI,CJ),
  action(C,CI,A,AK,AL), not action(C,CJ,A,AK,AL).

%new_not_valid(B,A,BI,BJ,AK,AL)
:- is_ig_predec(B,A),
  levels(B,A,BI), levels(B,A,BJ), B!=A,
  action(B,BI,A,AK,AL), not action(B,BJ,A,AK,AL).
%new_not_valid(B,A,BI,BJ,AK,AL)
:- is_ig_predec(B,A),
  ulevels(B,A,BI), ulevels(B,A,BJ), B!=A,
  action(B,BI,A,AK,AL), not action(B,BJ,A,AK,AL).

