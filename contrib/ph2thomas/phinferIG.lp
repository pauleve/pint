%%% Inférence des paramètres de Thomas depuis un Process Hitting
%%% Inférence du Graphe des Interactions depuis un Process Hitting

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abandon des projections %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Considération uniquement des confugurations des prédécesseurs %%%
%%% Nouvelle version selon la formalisation du papier %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% Format des données d'entrée (Automatiquement mis en forme par phinferK.ml)

% Process Hitting (général)
%sort(nom, plafond).                      % Sorte
%cooperative_sort(nom).                   % Sorte coopérative
%cooperation(nom_ab, nom_a, a_i, ab_j).   % Coopération : ab_j représente a_i
%action(a,i,b,j,k).                       % Action a_i -> b_j /> b_k



%%% Fonctions standard

% Fonctions pour Process Hitting
gene(A,P) :- sort(A,P), not cooperative_sort(A).
is_gene(A) :- gene(A,_).
is_process(A,I) :- gene(A,P), I = 0..P.
is_bounce(A,I,J) :- is_process(A,I), is_process(A,J), I != J.

cooperative_sort(CS,P) :- cooperative_sort(CS), sort(CS,P).
is_cs(CS) :- cooperative_sort(CS,_).  % cs = cooperative sort
is_gene_or_cs(A) :- is_gene(A).
is_gene_or_cs(CS) :- is_cs(CS).
cs_of(A,CS) :- cooperation(CS,A,_,_).
is_process_of_cs(CS,I) :- cooperative_sort(CS,P), I = 0..P.
is_process_or_process_of_cs(A,I) :- gene(A,P), I = 0..P.
is_process_or_process_of_cs(CS,I) :- cooperative_sort(CS,P), I = 0..P.
is_bounce_of_cs(CS,I,J) :- is_process_of_cs(CS,I), is_process_of_cs(CS,J), I != J.
is_bounce_or_bounce_of_cs(A,J,K) :- is_bounce(A,J,K).
is_bounce_or_bounce_of_cs(CS,J,K) :- is_bounce_of_cs(CS,J,K).
is_ph_predec(A,B) :- action(A,_,B,_,_).

is_direction(-1..1).



%%% Contraintes d'intégrité
% Sur le Process Hitting
:- sort(_,P), P <= 0.

:- action(A,I,_,_,_), not is_process(A,I), not is_process_of_cs(A,I).
:- action(_,_,B,J,K), not is_bounce(B,J,K), not is_bounce_of_cs(B,J,K).
:- action(_,_,B,J,K), #abs(J-K) != 1, is_gene(B).
:- action(A,I,A,J,K), I != J.

is_cs_debug(CS) :- cooperation(CS,_,_,_).
:- is_cs(CS), not is_cs_debug(CS).
:- is_cs_debug(CS), not is_cs(CS).
:- cooperation(_,A,I,_), not is_process(A,I), not is_process_of_cs(A,I).
%:- cooperation(CS,A,I1,J), cooperation(CS,A,I2,J), I1 != I2. % Non -> cas de représentations de plusieurs processus
at_least_one_cooperation_debug(CS,A,J) :- is_process(A,I), is_process_of_cs(CS,J), cs_of(A,CS), cooperation(CS,A,I,J).
:- is_process_of_cs(CS,J), is_gene(A), cs_of(A,CS), not at_least_one_cooperation_debug(CS,A,J).



%%% Résolution de l'inférence du Graphe des Interactions

% Prédécesseurs dans le PH : directs ou à travers une/des SC
is_ph_pred(A,B) :- is_ph_predec(A,B).
is_ph_pred(A,B) :- is_ph_pred(A,CS), is_ph_pred(CS,B), is_cs(CS).
is_ph_reg(A,B) :- is_ph_pred(A,B), is_gene(A).
is_ph_pred_cs(CS,B) :- is_ph_pred(CS,B), is_cs(CS).

%gamma(A,B,A) :- is_gene(B), is_gene(A), is_ph_reg(B,A).
%gamma(B,B,A) :- is_gene(B), is_gene(A), is_ph_reg(B,A).
gamma(C,B,A) :- is_gene(C), is_gene(B), is_gene(A), is_ph_reg(B,A), is_ph_pred_cs(CS,A), is_ph_pred(B,CS), is_ph_pred(C,CS), C != B.
gamma_cs(CS,B,A) :- is_cs(CS), is_gene(B), is_gene(A), is_ph_reg(B,A), is_ph_pred_cs(CS,A), gamma(C,B,A), is_ph_pred(C,CS).

% Pour le cas où seul a régule a
not_forever_alone(A) :- is_gene(A), is_ph_reg(B,A), B != A.
% Cycle dans les bounces
bounce_predec(A,X,Y) :- action(A,_,A,X,Y).
bounce_predec(A,X,Z) :- bounce_predec(A,X,Y), bounce_predec(A,Y,Z).
bounce_cycle(A) :- bounce_predec(A,X,X).
% focals
exists_self_hit(A,J) :- action(A,_,A,J,_).
self_focals(A,I) :- is_process(A,I), not exists_self_hit(A,I), not bounce_cycle(A).
self_focals_exists_upper(A,I) :- self_focals(A,I), self_focals(A,J), I < J.
self_focals_max(A,I) :- self_focals(A,I), not self_focals_exists_upper(A,I).
self_focals_exists_lower(A,I) :- self_focals(A,I), self_focals(A,J), J < I.
self_focals_min(A,I) :- self_focals(A,I), not self_focals_exists_lower(A,I).
focals_is_not_interval(A) :- is_process(A,I), self_focals_min(A,Min), self_focals_max(A,Max), Min < I, I < Max, not self_focals(A,I).
exists_self_focals(A) :- self_focals(A,_).
focals_is_not_interval(A) :- is_gene(A), not exists_self_focals(A).

% Si non précisé, recherche de tous les arcs
1 {eb(B) : is_ph_reg(B,A)} 1 :- ea(A).
1 {ea(A) : is_gene(A)} 1.
b_process(BI) :- eb(B), is_process(B,BI).
a_process(AI) :- ea(A), is_process(A,AI).
% Capture du cas auto_influence
auto_influence :- gamma(A,B,A), ea(A), eb(B).
auto_influence :- ea(A), eb(A).
auto_action :- ea(A), eb(A).

% Sélection d'une configuration de tous les autres prédécesseurs
%seac(C) :- is_gene(C), eb(B), is_ph_predec_proj(C,B), not ea(C). %%% gamma
1 {sigma_select(C,I) : is_process(C,I)} 1 :- gamma(C,B,A), eb(B), ea(A).%, not eb(C).
sigma(BI,C,I) :- sigma_select(C,I), b_process(BI).
sigma(BI,B,BI) :- eb(B), b_process(BI).
% Héritage aux sortes coopératives
temp_not_sigma(BI,CS,I) :- ea(A), is_ph_pred_cs(CS,A), is_process_of_cs(CS,I), is_ph_predec(C,CS), sigma(BI,C,J), not cooperation(CS,C,J,I).
sigma(BI,CS,I) :- eb(B), ea(A), b_process(BI), is_process_of_cs(CS,I), gamma_cs(CS,B,A), not temp_not_sigma(BI,CS,I).
% Debug
%:- sigma(BI,CS,I), sigma(BI,CS,J), I != J.
% Bounds depuis le contexte sélectionné (sigma)
%% Cas sans auto-influence
temp_exists_hit(BI,J) :- ea(A), action(C,I,A,J,_), sigma(BI,C,I), not auto_influence.
bounces(BI,AK,AK) :- b_process(BI), a_process(AK), not temp_exists_hit(BI,AK), not auto_influence.
bounces(BI,AK,AL) :- b_process(BI), ea(A), action(C,I,A,AK,AL), sigma(BI,C,I), not auto_influence.
%% Cas avec auto_influence (a régule a avec b)
temp_exists_hit(BI,J) :- ea(A), action(C,I,A,J,_), sigma(BI,C,I), sigma(BI,A,J), auto_influence.
bounces(BI,AK,AK) :- b_process(BI), ea(A), a_process(AK), not temp_exists_hit(BI,AK), sigma(BI,A,AK), auto_influence.
bounces(BI,AK,AL) :- b_process(BI), ea(A), action(C,I,A,AK,AL), sigma(BI,C,I), sigma(BI,A,AK), auto_influence.
% Conclusion
gi_edge(B,"+",BJ,A) :- eb(B), ea(A), b_process(BI), b_process(BJ), BI < BJ, bounces(BI,AK,AL1), bounces(BJ,AK,AL2), AL1 < AL2, not auto_action.
gi_edge(B,"-",BJ,A) :- eb(B), ea(A), b_process(BI), b_process(BJ), BI < BJ, bounces(BI,AK,AL1), bounces(BJ,AK,AL2), AL1 > AL2, not auto_action.

% Cas auto_action (a -> a)
no_influence :- ea(A), not focals_is_not_interval(A), not not_forever_alone(A), auto_action.
% "0" = ∅, "+" = +, "-" = −, "?" = ±
epsilon(AI,"0") :- bounces(AI,AI,AI).
epsilon(AI,"+") :- bounces(AI,AI,AI + 1), not epsilon(AI,"?").
epsilon(AI,"-") :- bounces(AI,AI,AI - 1), not epsilon(AI,"?").
epsilon(AI,"?") :- bounces(AI,AI,AI + 1), bounces(AI,AI,AI - 1).
%sign("0";"+";"-";"?").
%same_sign("+","+";"?"). same_sign("-","-";"?").
true_sign("+";"-").
bar_sign("+","-"). bar_sign("-","+").
exists_sign(S) :- true_sign(S), epsilon(_,S).
exists_sign("+";"-") :- epsilon(_,"?").
all_sign(S) :- exists_sign(S), bar_sign(S,S_bar), not exists_sign(S_bar).
%exists_below(AK,S1) :- a_process(AK), same_sign(S1,S2), epsilon(AI,S2), AI < AK.
%exists_above(AK,S1) :- a_process(AK), same_sign(S1,S2), epsilon(AJ,S2), AK <= AJ.
%all_above(AK,S1) :- a_process(AK), bar_sign(S1,S2), not exists_above(AK,S2), not exists_above(AK,"?").
%%%%%% Contraindre AK ≠ 0 ?
%all_below(AK,S1) :- a_process(AK), bar_sign(S1,S2), not exists_below(AK,S2), not exists_below(AK,"?").

%gi_edge(A,"?",0,A) :- ea(A), not not_forever_alone(A), not no_influence.
%gi_edge(A,S1,AK,A) :- auto_action, ea(A), all_above(AK,S1), all_below(AK,S2), bar_sign(S1,S2), not_forever_alone(A), not no_influence.

gi_edge(A,"?",0,A) :- auto_action, ea(A), epsilon(_,"?"), not no_influence.
gi_edge(A,S,AJ,A) :- auto_action, ea(A), true_sign(S), bar_sign(S,S_bar), epsilon(AI,S_bar), epsilon(AJ,S), AI < AJ, not no_influence.
gi_edge(A,S,P,A) :- auto_action, ea(A), true_sign(S), bar_sign(S,S_bar), epsilon(0,S_bar), sort(A,P), not no_influence.
gi_edge(A,S,P,A) :- auto_action, ea(A), true_sign(S), epsilon(P,S), sort(A,P), not no_influence.
gi_edge(A,S,P,A) :- auto_action, ea(A), true_sign(S), sort(A,P), epsilon(P,"0"), epsilon(0,"0"), all_sign(S), not no_influence.


%debug_GI_EDGE(S) :- auto_action, ea(A), bar_sign(S,Sbar), epsilon(0,S_bar), not no_influence.
%debug_GI_EDGE(S) :- bar_sign(S,"-").


%all_above(AK,S1) :- a_process(AK), bar_sign(S1,S2), same_sign(S2,S3), not exists_above(AK,S3).
%all_below(AK,S1) :- a_process(AK), bar_sign(S1,S2), same_sign(S2,S3), not exists_below(AK,S3).


%no_gi_edge(A,"+",A) :- ea(A), exists_above(_,"-").





#hide.

%#show is_ph_pred/2.
%#show is_ph_reg/2.
%#show is_ph_pred_cs/2.

%#show bounce_predec/3.
%#show bounce_cycle/1.
%#show not_forever_alone/1.
%#show exists_self_hit/2.
#show self_focals/2.
%#show self_focals_exists_upper/2.
%#show self_focals_max/2.
%#show self_focals_exists_lower/2.
%#show self_focals_min/2.
#show exists_self_focals/1.
#show focals_is_not_interval/1.

%#show gamma/3.
%#show gamma_cs/3.
#show ea/1.
#show eb/1.
%#show sigma_select/2.
#show sigma/3.
#show auto_influence/0.
#show auto_action/0.
%#show temp_exists_hit/2.
#show bounces/3.
#show gi_edge/4.

#show no_influence/0.
#show epsilon/2.
#show exists_sign/1.
#show all_sign/1.
%#show exists_below/2.
%#show exists_above/2.
%#show all_above/2.
%#show all_below/2.
#show gi_edge/3.


% Debug
#show debug_GI_EDGE/1.


%*
% Prédécesseurs dans le PH : directs ou à travers une/des SC
temp_is_ph_predec_proj(A,B) :- is_ph_predec(A,B).
temp_is_ph_predec_proj(A,B) :- temp_is_ph_predec_proj(A,CS), temp_is_ph_predec_proj(CS,B), is_cs(CS).
is_ph_predec_proj(A,B) :- temp_is_ph_predec_proj(A,B), is_gene(A), is_gene(B).
is_ph_predec_proj_cs(CS,B) :- temp_is_ph_predec_proj(CS,B), is_cs(CS), is_gene(B).
%is_ph_predec_proj_or_cs(A,B) :- is_ph_predec_proj(A,B).
%is_ph_predec_proj_or_cs(CS,B) :- is_ph_predec_proj_cs(CS,B).

% Si non précisé, recherche de tous les arcs
1 {ea(A) : is_ph_predec_proj(A,B)} 1 :- eb(B).
p_ea(AI) :- ea(A), is_process(A,AI).
1 {eb(B) : is_gene(B)} 1.

% Sélection d'une configuration de tous les autres prédécesseurs
seac(C) :- is_gene(C), eb(B), is_ph_predec_proj(C,B), not ea(C).
1 {seac_select(C,I) : is_process(C,I)} 1 :- seac(C).
seac_p(AI,C,I) :- seac_select(C,I), p_ea(AI).
seac_p(AI,A,AI) :- ea(A), p_ea(AI).
% Héritage aux sortes coopératives
temp_not_seac_p(AI,CS,I) :- eb(B), is_process_of_cs(CS,I), is_ph_predec_proj_cs(CS,B), is_ph_predec(A,CS), seac_p(AI,A,J), not cooperation(CS,A,J,I).
seac_p(AI,CS,I) :- p_ea(AI), eb(B), is_ph_predec_proj_cs(CS,B), is_process_of_cs(CS,I), not temp_not_seac_p(AI,CS,I).
% Sélection uniquement de ea et de ses successeurs
seac_proj(AI,CS,I) :- seac_p(AI,CS,I), temp_is_ph_predec_proj(A,CS), ea(A), is_cs(CS).
seac_proj(AI,A,I) :- seac_p(AI,A,I), ea(A).
%debug_auto_influence(CS) :- eb(B), seac_proj(_,CS,_), temp_is_ph_predec_proj(B,CS).
auto_influence :- eb(B), seac_proj(_,CS,_), temp_is_ph_predec_proj(B,CS), is_cs(CS).
auto_influence :- ea(B), eb(B).
%auto_influence :- eb(B), is_ph_predec_proj(B,B).

% Actions considérées : celles partant des seac_proj(C,I) (H^gamma)
proj_action(AI,A,I,B,J,K) :- action(A,I,B,J,K), seac_proj(AI,A,I), eb(B).
% L^*_{b,a_i}
gi_exists_hit(AI,J) :- p_ea(AI), eb(B), proj_action(AI,_,_,B,J,_).
% Cas sans auto-influence : J = processus focaux
gi_sL_foc(AI,J) :- eb(B), p_ea(AI), is_process(B,J), not gi_exists_hit(AI,J), not auto_influence.
% Cas avec auto-influence : J = +1/0/-1 (selon sens d'attraction)
gi_sL_foc(AI,0) :- auto_influence, seac_p(AI,B,J), eb(B), not gi_exists_hit(AI,J).
gi_sL_foc(AI,D) :- auto_influence, eb(B), seac_p(AI,B,J), proj_action(AI,A,I,B,J,K), D = K - J.
% Cas où L^*_{b,a_i} est vide
temp_gi_sL_foc(AI) :- gi_sL_foc(AI,J).
empty_gi_sL_foc(AI) :- p_ea(AI), not temp_gi_sL_foc(AI).
% Condition 3.2 (processus focaux attracteurs)
% Cas sans auto-influence
temp_gi_focal_is_attractor(J,K,L) :- eb(B), is_process(B,J), is_bounce(B,K,L), #abs(K-L) == 1, #abs(J-L) < #abs(J-K).
temp_gi_not_attractor(AI) :- eb(B), gi_sL_foc(AI,J), proj_action(AI,_,_,B,K,L), not temp_gi_focal_is_attractor(J,K,L), not auto_influence.
gi_attractor(AI) :- p_ea(AI), not temp_gi_not_attractor(AI), not auto_influence.
% Cas avec auto-influence
temp_gi_not_attractor(AI) :- auto_influence, gi_sL_foc(AI,J), gi_sL_foc(AI,K), is_direction(F), J < F, F < K, not gi_sL_foc(AI,F).
gi_attractor(AI) :- auto_influence, p_ea(AI), not temp_gi_not_attractor(AI).
% Min et max des intervalles
temp_gi_exists_sup(AI,J) :- gi_sL_foc(AI,J), gi_sL_foc(AI,K), J < K.
gi_sL_foc_max(AI,Max) :- gi_sL_foc(AI,Max), gi_attractor(AI), not temp_gi_exists_sup(AI,Max).
temp_gi_exists_inf(AI,J) :- gi_sL_foc(AI,J), gi_sL_foc(AI,K), K < J.
gi_sL_foc_min(AI,Min) :- gi_sL_foc(AI,Min), gi_attractor(AI), not temp_gi_exists_inf(AI,Min).
% Relation <_[] entre les intervalles
gi_inf(AI1,AI2) :- gi_sL_foc_max(AI1,Max1), gi_sL_foc_min(AI1,Min1), gi_sL_foc_max(AI2,Max2), gi_sL_foc_min(AI2,Min2), Min1 < Min2, Max1 <= Max2.
gi_inf(AI1,AI2) :- gi_sL_foc_max(AI1,Max1), gi_sL_foc_min(AI1,Min1), gi_sL_foc_max(AI2,Max2), gi_sL_foc_min(AI2,Min2), Min1 <= Min2, Max1 < Max2.
% Relation =_[] entre les intervalles
gi_eq(AI1,AI2) :- gi_sL_foc_max(AI1,Max), gi_sL_foc_min(AI1,Min), gi_sL_foc_max(AI2,Max), gi_sL_foc_min(AI2,Min).
% Conclusion : signe des arcs
% Cas général : arc non-signé si non-conclusif
gi_edge(A,"?",0,B) :- ea(A), eb(B), empty_gi_sL_foc(_).
gi_edge(A,"?",0,B) :- ea(A), eb(B), temp_gi_not_attractor(_).
% Cas sans auto-influence
gi_edge(A,"+",AI2,B) :- ea(A), eb(B), gi_inf(AI1,AI2), AI1 < AI2, not auto_influence.
gi_edge(A,"-",AI2,B) :- ea(A), eb(B), gi_inf(AI2,AI1), AI1 < AI2, not auto_influence.
% Cas avec auto-influence
auto_influence_exception(0) :- auto_influence, gi_sL_foc(0,0), gi_sL_foc(1,-1).
auto_influence_exception(I) :- auto_influence, eb(B), gene(B,I), gi_sL_foc(I,0), gi_sL_foc(I - 1,1).
gi_edge(A,"+",AI2,B) :- auto_influence, ea(A), eb(B), gi_inf(AI1,AI2), AI1 < AI2, not auto_influence_exception(AI1), not auto_influence_exception(AI2).
gi_edge(A,"-",AI2,B) :- auto_influence, ea(A), eb(B), gi_inf(AI2,AI1), AI1 < AI2, not auto_influence_exception(AI1), not auto_influence_exception(AI2).

% Clauses de débogage, warnings et erreurs
debug("I FOUNDS A gi_edge +", AI1, AI2) :- auto_influence, ea(A), eb(B), gi_inf(AI1,AI2), AI1 < AI2.
debug("I FOUNDS A gi_edge -", AI1, AI2) :- auto_influence, ea(A), eb(B), gi_inf(AI2,AI1), AI1 < AI2.
debug_no_action(A,AI1,AI2,B) :- auto_influence, ea(A), eb(B), p_ea(AI1), p_ea(AI2), AI1 < AI2, auto_influence_exception(AI2).
debug_no_action(A,AI1,AI2,B) :- auto_influence, ea(A), eb(B), p_ea(AI1), p_ea(AI2), AI1 < AI2, auto_influence_exception(AI1).
debug_no_action(A,AI1,AI2,B) :- ea(A), eb(B), gi_eq(AI2,AI1), AI1 < AI2.

warning("NOT AN ATTRACTOR [temp_gi_not_attractor(AI)]",A,AI) :- ea(A), temp_gi_not_attractor(AI).

warning("EMPTY L INTERVAL [empty_gi_sL_foc(AI)]",A,AI) :- ea(A), empty_gi_sL_foc(AI).

answer_ok :- debug_no_action(_,_,_,_).
answer_ok :- gi_edge(_,_,_,_).
warning("NOT AN ACTIVATION NOR INHIBITION [not gi_edge(A,+/-,B)]",A,B) :- ea(A), eb(B), not answer_ok.
*%

%*

%%% Affichage des solutions
#hide.

% Inférence du Graphe des Interactions
#show ea/1.
#show ea_p/2.
#show eb/1.
#show seac/1.
%#show seac_p/3.
#show seac_proj/3.
#show proj_action/6.
#show auto_influence/0.
%#show gi_exists_hit/2.
#show gi_sL_foc/2.
%#show temp_gi_focal_is_attractor/3.
%#show temp_gi_not_attractor/1.
#show gi_attractor/1.
#show auto_influence_exception/1.
#show gi_edge/4.
#show gi_edge/3.

% Debug
#show error/3.
#show warning/3.
#show debug/3.
%#show debug_no_action/4.
#show debug_auto_influence/1.

%#show temp_is_ph_predec_proj/2.
%#show temp_proj_action/5.
%#show gi_exists_hit/2.
%#show temp_gi_focal_is_attractor/3.
%#show temp_gi_not_attractor/1.
% Débogage
%#show gi_sL_foc/2.
%#show gi_attractor/1.
%#show temp_gi_sL_foc/1.
%#show empty_gi_sL_foc/1.
%#show gi_sL_foc_max/2.
%#show gi_sL_foc_min/2.
%#show gi_inf/2.
%#show gi_edge/3.

*%

