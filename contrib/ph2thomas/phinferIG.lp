%%% Inférence du Graphe des Interactions depuis un Process Hitting

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Inférence adaptée de : Adrien Richard, Negative circuits and %%%
%%%   sustained oscillations in asynchronous automata networks %%%%%
%%% Avec prise en compte des groupes de prédécesseurs %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% Format des données d'entrée

% Process Hitting (général)
%sort(nom, plafond).                      % Sorte
%cooperative_sort(nom).                   % Sorte coopérative
%cooperation(nom_ab, nom_a, a_i, ab_j).   % Coopération : ab_j représente a_i
%action(a,i,b,j,k).                       % Action a_i -> b_j /> b_k



%%% Fonctions standard

%% Fonctions pour Process Hitting
gene(A,P) :- sort(A,P), not cooperative_sort(A).
is_gene(A) :- gene(A,_).
is_process(A,AI) :- gene(A,AP), AI=0..AP.
is_bounce(A,AI,AJ) :- is_process(A,AI), is_process(A,AJ), AI!=AJ.

cooperative_sort(CS,CSP) :- cooperative_sort(CS), sort(CS,CSP).
is_cs(CS) :- cooperative_sort(CS,_).  % cs = cooperative sort
is_gene_or_cs(A) :- is_gene(A).
is_gene_or_cs(CS) :- is_cs(CS).
cs_of(A,CS) :- cooperation(CS,A,_,_).
is_process_of_cs(CS,CSI) :- cooperative_sort(CS,CSP), CSI=0..CSP.
is_process_or_process_of_cs(A,AI) :- gene(A,AP), AI=0..AP.
is_process_or_process_of_cs(CS,CSI) :- cooperative_sort(CS,CSP), CSI=0..CSP.
is_bounce_of_cs(CS,CSI,CSJ) :- is_process_of_cs(CS,CSI), is_process_of_cs(CS,CSJ), CSI!=CSJ.
is_bounce_or_bounce_of_cs(A,AJ,AK) :- is_bounce(A,AJ,AK).
is_bounce_or_bounce_of_cs(CS,CSJ,CSK) :- is_bounce_of_cs(CS,CSJ,CSK).
is_ph_predec(A,B) :- action(A,_,B,_,_).



%%% Contraintes d'intégrité

%% Sur le Process Hitting
:- sort(_,P), P<=0.

:- action(A,AI,_,_,_), not is_process(A,AI), not is_process_of_cs(A,AI).
:- action(_,_,B,BJ,BK), not is_bounce(B,BJ,BK), not is_bounce_of_cs(B,BJ,BK).
:- action(_,_,B,BJ,BK), #abs(BJ-BK) != 1, is_gene(B).
:- action(A,AI,A,AJ,_), AI != AJ.

is_cs_debug(CS) :- cooperation(CS,_,_,_).
:- is_cs(CS), not is_cs_debug(CS).
:- is_cs_debug(CS), not is_cs(CS).
:- cooperation(_,A,AI,_), not is_process(A,AI), not is_process_of_cs(A,AI).
at_least_one_cooperation_debug(CS,A,CSJ) :- is_process(A,AI), is_process_of_cs(CS,CSJ),
  cs_of(A,CS), cooperation(CS,A,AI,CSJ).
:- is_process_of_cs(CS,CSJ), is_gene(A), cs_of(A,CS), not at_least_one_cooperation_debug(CS,A,CSJ).



%%% Résolution de l'inférence du Graphe des Interactions

%% Prédécesseurs dans le PH : influencent de façon directe ou à travers une/des SC
is_ph_pred(A,B) :- is_ph_predec(A,B).
is_ph_pred(A,B) :- is_ph_pred(A,CS), is_ph_pred(CS,B), is_cs(CS).
is_ph_pred_cs(CS,B) :- is_ph_pred(CS,B), is_cs(CS).
%% Régulateurs : prédécesseurs gènes (non SC)
is_ph_reg(A,B) :- is_ph_pred(A,B), is_gene(A).
is_ph_reg_or_same(A,B) :- is_ph_reg(A,B).
is_ph_reg_or_same(A,A) :- is_gene(A), no_reg(A).
temp_not_no_reg(A) :- is_ph_reg(_,A).
no_reg(A) :- is_gene(A), not temp_not_no_reg(A).

%% Recherche de tous les arcs potentiels (A -> A compris)
1 {eb(B) : is_ph_reg_or_same(B,A)} 1 :- ea(A).
1 {ea(A) : is_gene(A)} 1.
b_process(BI) :- eb(B), is_process(B,BI).
a_process(AI) :- ea(A), is_process(A,AI).
auto_action :- ea(A), eb(A).
couple(BI,AI) :- a_process(AI), b_process(BI), not auto_action.
couple(AI,AI) :- a_process(AI), auto_action.

%% gamma : prédécesseurs C infuençant A conjointement avec B et autres que B
%%          (B non inclus et A non inclus sauf si auto_action)
%%          (C ≠ B et A non inclus si A ≠ B)
gamma(C,B,A) :- is_gene(C), is_gene(B), is_gene(A), is_ph_reg(B,A), is_ph_pred_cs(CS,A),
  is_ph_pred(B,CS), is_ph_pred(C,CS), C!=B.
gamma(C,B,A) :- gamma(D,B,A), is_gene(C), is_ph_pred_cs(CS,A),
  is_ph_pred(C,CS), is_ph_pred(D,CS), C!=B, C!=D.
gamma_cs(CS,B,A) :- is_cs(CS), is_gene(B), is_gene(A), is_ph_reg(B,A), is_ph_pred_cs(CS,A),
  gamma(C,B,A), is_ph_pred(C,CS).

%% Sélection d'une configuration des prédécesseurs
1 {sigma_select(C,CI) : is_process(C,CI)} 1 :- gamma(C,B,A), eb(B), ea(A), C!=B, C!=A.
sigma(BI,AI,C,CI) :- couple(BI,AI), sigma_select(C,CI).
sigma(BI,AI,B,BI) :- eb(B), couple(BI,AI).
sigma(BI,AI,A,AI) :- ea(A), couple(BI,AI).
%% Héritage aux SC
temp_not_sigma(BI,AI,CS,CSI) :- ea(A), is_ph_pred_cs(CS,A), is_process_of_cs(CS,CSI),
  is_ph_predec(C,CS), sigma(BI,AI,C,CJ), not cooperation(CS,C,CJ,CSI).
sigma(BI,AI,CS,CSI) :- ea(A), eb(B), couple(BI,AI), is_process_of_cs(CS,CSI), is_ph_pred(CS,A),
  not temp_not_sigma(BI,AI,CS,CSI).

%% Recherche des bonds possibles autour d'un processus
bounce_to(BI,AI,AJ) :- ea(A), a_process(AI), a_process(AJ), b_process(BI), sigma(BI,AI,C,CI),
  action(C,CI,A,AI,AJ).
exists_bounce(BI,AI) :- bounce_to(BI,AI,AJ), AI!=AJ.
bounce_to(BI,AI,AI) :- ea(A), a_process(AI), b_process(BI), sigma(BI,AI,_,_),
  not exists_bounce(BI,AI).



%%% Conclusion

%% Conclusion du cas B -> A
gi_edge(B,"+",BI+1,A) :- ea(A), eb(B), b_process(BI), b_process(BI+1), bounce_to(BI,AI,AJ),
  bounce_to(BI+1,AI,AK), AJ<AK, not auto_action.
gi_edge(B,"-",BI+1,A) :- ea(A), eb(B), b_process(BI), b_process(BI+1), bounce_to(BI,AI,AJ),
  bounce_to(BI+1,AI,AK), AJ>AK, not auto_action.
%% Conclusion du cas A -> A
gi_edge(A,"+",AI+1,A) :- ea(A), a_process(AI), a_process(AI+1), bounce_to(AI,AI,AJ),
  bounce_to(AI+1,AI+1,AK), AK>=AI+1, AJ<=AI, auto_action, no_reg(A).
gi_edge(A,"-",AI+1,A) :- ea(A), a_process(AI), a_process(AI+1), bounce_to(AI,AI,AJ),
  bounce_to(AI+1,AI+1,AK), AK<AI+1, AJ>AI, auto_action, no_reg(A).
%% Conclusion du cas A -> A (B ≠ A mais observation des groupes de régulateurs)
gi_edge(A,"+",AI+1,A) :- ea(A), a_process(AI), a_process(AI+1), b_process(BI),
  bounce_to(BI,AI,AJ), bounce_to(BI,AI+1,AK), AK>=AI+1, AJ<=AI, not auto_action.
gi_edge(A,"-",AI+1,A) :- ea(A), a_process(AI), a_process(AI+1), b_process(BI),
  bounce_to(BI,AI,AJ), bounce_to(BI,AI+1,AK), AK<AI+1, AJ>AI, not auto_action.



%%% Affichage des solutions
#hide.

#show ea/1.
#show eb/1.
#show gi_edge/4.
%#show gi_edge_debug/6.

