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

%is_direction(-1..1).



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
is_ph_reg_or_same(A,B) :- is_ph_reg(A,B).
is_ph_reg_or_same(A,A) :- is_gene(A).
is_ph_pred_cs(CS,B) :- is_ph_pred(CS,B), is_cs(CS).

%gamma(A,B,A) :- is_gene(B), is_gene(A), is_ph_reg(B,A).
%gamma(B,B,A) :- is_gene(B), is_gene(A), is_ph_reg(B,A).
gamma(C,B,A) :- is_gene(C), is_gene(B), is_gene(A), is_ph_reg(B,A), is_ph_pred_cs(CS,A), is_ph_pred(B,CS), is_ph_pred(C,CS), C != B.
gamma_cs(CS,B,A) :- is_cs(CS), is_gene(B), is_gene(A), is_ph_reg(B,A), is_ph_pred_cs(CS,A), gamma(C,B,A), is_ph_pred(C,CS).
temp_x(C,CS,A) :- is_cs(CS), is_gene(C), is_gene(A), is_ph_predec(CS,A), is_ph_pred(C,CS).
temp_x(D,CS,A) :- temp_x(C,CS,A), temp_x(D,CS2,A), temp_x(C,CS2,A).
temp_is_x(C,A) :- temp_x(C,_,A).
x(C,CS,A) :- temp_x(C,CS,A), C != CS.
x(C,C,A) :- is_gene(C), is_gene(A), is_ph_predec(C,A), not temp_is_x(C,A).
x_cs(CS,C,A) :- is_gene(A), x(D,C,A), is_ph_pred(D,CS), is_ph_pred_cs(CS,A).

%*
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
*%

% Si non précisé, recherche de tous les arcs
1 {eb(B) : is_ph_reg_or_same(B,A)} 1 :- ea(A).
1 {ea(A) : is_gene(A)} 1.
b_process(BI) :- eb(B), is_process(B,BI).
a_process(AI) :- ea(A), is_process(A,AI).
% Capture du cas auto_influence
auto_influence :- gamma(A,B,A), ea(A), eb(B).
auto_influence :- ea(A), eb(A).
auto_action :- ea(A), eb(A).

% Sélection d'une configuration de tous les autres prédécesseurs
%seac(C) :- is_gene(C), eb(B), is_ph_predec_proj(C,B), not ea(C). %%% gamma
1 {sigma_select(C,I) : is_process(C,I)} 1 :- gamma(C,B,A), eb(B), ea(A), not auto_action.
1 {predec_select(D) : is_ph_predec(D,A)} 1 :- ea(A), auto_action.
1 {sigma_select(C,I) : is_process(C,I)} 1 :- ea(A), x(C,D,A), predec_select(D), auto_action.
sigma(BI,C,I) :- sigma_select(C,I), b_process(BI), eb(B), C != B.
sigma(BI,B,BI) :- eb(B), b_process(BI).
% Héritage aux sortes coopératives
temp_not_sigma(BI,CS,I) :- ea(A), is_ph_pred_cs(CS,A), is_process_of_cs(CS,I), is_ph_predec(C,CS), sigma(BI,C,J), not cooperation(CS,C,J,I).
sigma(BI,CS,I) :- eb(B), ea(A), b_process(BI), is_process_of_cs(CS,I), gamma_cs(CS,B,A), not temp_not_sigma(BI,CS,I), not auto_action.
sigma(BI,CS,I) :- ea(A), predec_select(C), b_process(BI), is_process_of_cs(CS,I), x_cs(CS,C,A), not temp_not_sigma(BI,CS,I), auto_action.
%sigma(AI,CS,I) :- ea(A), a_process(AI), is_process_of_cs(CS,I), is_ph_pred_cs(CS,A), not temp_not_sigma(AI,CS,I), auto_action.
% Debug
%:- sigma(BI,CS,I), sigma(BI,CS,J), I != J.

sign("+",1..3).
sign("-",(-3)..(-1)).
sign("DEBUG_ERROR_SIGN",(-50)..(-4);4..50).

% Calcul des bounces (focals depuis un seul processus)
%% Cas sans auto-influence
temp_exists_hit(BI,J) :- ea(A), action(C,I,A,J,_), sigma(BI,C,I), not auto_influence.
bounce_focals(BI,AK,AK) :- b_process(BI), a_process(AK), not temp_exists_hit(BI,AK), not auto_influence.
bounce_focals(BI,AI,AK) :- ea(A), b_process(BI), action(C,I,A,AI,AK), sigma(BI,C,I), not auto_influence.
%% Cas avec auto_influence (a régule a avec b)
temp_exists_hit(BI,J) :- ea(A), action(C,I,A,J,_), sigma(BI,C,I), sigma(BI,A,J), auto_influence.
bounce_focals(BI,AK,AK) :- b_process(BI), ea(A), a_process(AK), not temp_exists_hit(BI,AK), sigma(BI,A,AK), auto_influence.
bounce_focals(BI,AK,AL) :- b_process(BI), ea(A), action(C,I,A,AK,AL), sigma(BI,C,I), sigma(BI,A,AK), auto_influence.
% Conclusion du cas b -> a
gi_edge(B,S,BI + 1,A) :- eb(B), ea(A), b_process(BI), b_process(BI + 1), bounce_focals(BI,AI,AJ), bounce_focals(BI + 1,AI,AK), sign(S,AK - AJ),
  not auto_action.
%gi_edge(B,"-",BI + 1,A) :- eb(B), ea(A), b_process(BI), b_process(BI + 1), bounce_focals(BI,AK,AL1), bounce_focals(BI + 1,AK,AL2), AL1 > AL2,
%  not auto_action.

% Cas auto_action (a -> a)
gi_edge(A,S,AI + 1,A) :- ea(A), a_process(AI), a_process(AI + 1), bounce_focals(AI,AI,AJ), bounce_focals(AI + 1,AI + 1,AK),
  AK != AJ + 1, sign(S,AK - AJ), auto_action.



#hide.

%#show gamma/3.
%#show gamma_cs/3.
%#show x/3.
%#show x_cs/3.

#show ea/1.
#show eb/1.
#show auto_influence/0.
#show auto_action/0.
%#show predec_select/1.
%#show sigma_select/2.
%#show temp_not_sigma/3.
#show sigma/3.

#show gi_edge/4.








