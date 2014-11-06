%%% Inférence des paramètres de Thomas depuis un Process Hitting



%%% Format des données d'entrée (Automatiquement mis en forme par phinferK.ml)

% Process Hitting (général)
%sort(nom, plafond).                      % Sorte
%cooperative_sort(nom).                   % Sorte coopérative
%cooperation(nom_ab, nom_a, a_i, ab_j).   % Coopération : ab_j représente a_i
%action(a,i,b,j,k).                       % Action a_i -> b_j /> b_k

% Graphe des Interactions du modèle de Thomas
%edge(source, "+"/"-", seuil, cible).     % Arc dans le graphe des interactions

% Énumération des paramètres possibles
%param_label(gène, étiquette).            % Définition d'un paramètre pour ce gène
%param_label(gène, étiquette, predec).    % Définition d'un activateur pour ce paramètre
%param(gène, étiquette, valeur).          % Paramètre déjà obtenu



%%% Fonctions standard

%% Fonctions pour Process Hitting
gene(A,AP) :- sort(A,AP), not cooperative_sort(A).
is_gene(A) :- gene(A,_).
is_process(A,AI) :- gene(A,AP), AI=0..AP.
is_bounce(A,AI,AJ) :- is_process(A,AI), is_process(A,AJ), AI!=AJ.

cooperative_sort(CS,AP) :- cooperative_sort(CS), sort(CS,AP).
is_cs(CS) :- cooperative_sort(CS,_).  % cs = cooperative sort
is_gene_or_cs(A) :- is_gene(A).
is_gene_or_cs(CS) :- is_cs(CS).
cs_of(A,CS) :- cooperation(CS,A,_,_).
is_process_of_cs(CS,AI) :- cooperative_sort(CS,AP), AI=0..AP.
is_bounce_of_cs(CS,AI,AJ) :- is_process_of_cs(CS,AI),
  is_process_of_cs(CS,AJ), AI!=AJ.

%% Fonctions pour le modèle de Thomas
is_predec(A,B) :- edge(A,_,_,B).
is_predec_pos(A,B) :- edge(A,"+",_,B).
is_predec_neg(A,B) :- edge(A,"-",_,B).
%% Compatibilité
%edge(A,"positive",X,B) :- edge(A,"+",X,B).
%edge(A,"negative",X,B) :- edge(A,"-",X,B).
%edge(A,"unsigned",X,B) :- edge(A,"?",X,B).

%% Fonctions concernant la paramétrisation
gene_levels(A,0..AI) :- gene(A,AI).
temp_less_active(A,PA1,PA2) :- param_label(A,PA1), param_label(A,PA2,B),
  not param_label(A,PA1,B), is_predec_pos(B,A).
temp_less_active(A,PA1,PA2) :- param_label(A,PA1,B), param_label(A,PA2),
  not param_label(A,PA2,B), is_predec_neg(B,A).
less_active(A,PA1,PA2) :- temp_less_active(A,PA1,PA2),
  not temp_less_active(A,PA2,PA1).

temp_exists_more_active(A,PA) :- less_active(A,PA,_).
max_active(A,PA) :- param_label(A,PA), not temp_exists_more_active(A,PA).
temp_exists_less_active(A,PA) :- less_active(A,_,PA).
min_active(A,PA) :- param_label(A,PA), not temp_exists_less_active(A,PA).



%%% Contraintes d'intégrité

%% Sur le Process Hitting
:- sort(_,P), P<=0.

:- action(A,AI,_,_,_), not is_process(A,AI), not is_process_of_cs(A,AI).
:- action(_,_,B,BJ,BK), not is_bounce(B,BJ,BK), not is_bounce_of_cs(B,BJ,BK).
:- action(_,_,B,BJ,BK), #abs(BJ-BK)!=1, is_gene(B).
:- action(A,AI,A,AJ,AK), AI!=AJ.

is_cs_debug(CS) :- cooperation(CS,_,_,_).
:- is_cs(CS), not is_cs_debug(CS).
:- is_cs_debug(CS), not is_cs(CS).
:- cooperation(_,A,AI,_), not is_process(A,AI), not is_process_of_cs(A,AI).
%:- cooperation(CS,A,I1,J), cooperation(CS,A,I2,J), I1 != I2. % Non -> cas de représentations de plusieurs processus
at_least_one_cooperation_debug(CS,A,CSJ) :- is_process(A,AI),
  is_process_of_cs(CS,CSJ), cs_of(A,CS), cooperation(CS,A,AI,CSJ).
:- is_process_of_cs(CS,CSJ), is_gene(A), cs_of(A,CS),
  not at_least_one_cooperation_debug(CS,A,CSJ).

%% Sur le Graphe des Interactions (Thomas)
:- edge(A,_,AI,_), not is_process(A,AI).
:- edge(_,_,_,B), not is_gene(B).
:- edge(_,S,_,_), S!="+", S!="-", S!="?".
:- edge(_,"?",_).

%% Sur les paramètres
:- param_label(B,_), not is_gene(B).
:- param_label(_,P), P <= 0.
:- param_label(B,PB,_), not param_label(B,PB).
:- param_label(_,_,A), not is_gene(A).
:- param_label(B,_,A), not is_predec(A,B).
:- param(A,PA,_), not param_label(A,PA).
:- param(A,_,AI), not gene_levels(A,AI).



%%% Résolution de l'énumération des paramètres possibles

%% Reprendre la valeur des paramètres inférés
infered_param(A,PA) :- param(A,PA,_).
enum_param(A,PA,AV) :- infered_param(A,PA), param(A,PA,AV).
%% Au moins une valeur par paramètre
1 {enum_param(A,PA,AV) : gene_levels(A,AV)} 1 :- is_gene(A), param_label(A,PA),
  not infered_param(A,PA).
%% Pas de trou dans les intervalles
exists_param_more(A,PA,AI) :- enum_param(A,PA,AI), enum_param(A,PA,AJ), AI<AJ.
param_max(A,PA,AI) :- enum_param(A,PA,AI), not exists_param_more(A,PA,AI).
exists_param_less(A,PA,AI) :- enum_param(A,PA,AI), enum_param(A,PA,AJ), AJ<AI.
param_min(A,PA,AI) :- enum_param(A,PA,AI), not exists_param_less(A,PA,AI).
:- param_min(A,PA,AMin), param_max(A,PA,AMax), AI=AMin..AMax,
  not enum_param(A,PA,AI).



%%% Contraintes biologiques
%% Bornes supérieure et inférieure
temp_has_predec(A) :- is_predec(_,A).
temp_min_active_ok(A) :- enum_param(A,PA,0), min_active(A,PA).
temp_max_active_ok(A) :- enum_param(A,PA,AI), gene(A,AI), max_active(A,PA).
:- temp_has_predec(A), not temp_min_active_ok(A).
:- temp_has_predec(A), not temp_max_active_ok(A).
%% Monotonicité [Def 3.5 de Bernot et al.] (Moins activé => inférieur ou égal)
param_inf(A,PA1,PA2) :- param_min(A,PA1,AMin1), param_max(A,PA1,AMax1),
  param_min(A,PA2,AMin2), param_max(A,PA2,AMax2), AMin1<=AMin2, AMax1<=AMax2.
:- less_active(A,PA1,PA2), not param_inf(A,PA1,PA2).
%% Activation [Def 3.4 de Bernot et al.] (Plus activé => au moins un strictement supérieur)
param_str_inf(A,PA1,PA2) :- param_min(A,PA1,AMin1), param_max(A,PA1,AMax1),
  param_min(A,PA2,AMin2), param_max(A,PA2,AMax2), AMin1<AMin2, AMax1<=AMax2.
param_str_inf(A,PA1,PA2) :- param_min(A,PA1,AMin1), param_max(A,PA1,AMax1),
  param_min(A,PA2,AMin2), param_max(A,PA2,AMax2), AMin1<=AMin2, AMax1<AMax2.
temp_activation_ok(A,B) :- is_predec(B,A),
  param_label(A,PA1), param_label(A,PA2), param_label(A,PA2,B),
  not param_label(A,PA1,B), param_str_inf(A,PA1,PA2).
temp_activation_ok(A,B) :- is_predec(B,A),
  param_label(A,PA1), param_label(A,PA2), param_label(A,PA2,B),
  not param_label(A,PA1,B), param_str_inf(A,PA2,PA1).
:- is_gene(A), is_predec(B,A), not temp_activation_ok(A,B).



%%% Respect de la dynamique du PH

%% Contexte : Niveaux_+/−
levels(A,B,AI) :- edge(A,_,AT,B), gene(A,AP), AI=AT..AP.
ulevels(A,B,AI) :- edge(A,_,AT,B), AI=0..AT-1.
%% Contecte : Context^b_{a,A,B}
sContext(A,PA,B,BI) :- levels(B,A,BI), param_label(A,PA,B).
sContext(A,PA,B,BI) :- param_label(A,PA), ulevels(B,A,BI),
  not param_label(A,PA,B).
sContext(A,PA,B,BI) :- gene(B,BL), BI=0..BL, param_label(A,PA),
  not is_predec(B,A).
temp_context_cs(A,PA,CS,B,CSI) :- cooperation(CS,B,BJ,CSI), sContext(A,PA,B,BJ).
temp_context_cs_neg(A,PA,CS,CSI) :- param_label(A,PA), is_process_of_cs(CS,CSI),
  cs_of(B,CS), not temp_context_cs(A,PA,CS,B,CSI).
sContext(A,PA,CS,CSI) :- param_label(A,PA), is_process_of_cs(CS,CSI),
  not temp_context_cs_neg(A,PA,CS,CSI).
%% Actions possibles
exists_action(A,PA,AI,AJ) :- action(C,CK,A,AI,AJ), sContext(A,PA,C,CK).
%% Contraintes
% Propriété 5 (Parameter validity)
not_compatible(A,PA) :- is_bounce(A,AI,AJ), sContext(A,PA,A,AI),
  not enum_param(A,PA,AI), not exists_action(A,PA,AI,AJ), AI<AKMin,
  param_min(A,PA,AKMin), AJ>AI, #abs(AI-AJ)==1.
not_compatible(A,PA) :- is_bounce(A,AI,AJ), sContext(A,PA,A,AI),
  not enum_param(A,PA,AI), not exists_action(A,PA,AI,AJ), AI>AKMin,
  param_min(A,PA,AKMin), AJ<AI, #abs(AI-AJ)==1.

%result_param(A,PA,AI) :- param(A,PA,AI).
result_param(A,PA,AI) :- enum_param(A,PA,AI), not not_compatible(A,PA).
%result_param(A,PA,AI) :- enum_param(A,PA,AI).



%%% Contrainte : paramètres simples
%:- enum_param(A,P,I), enum_param(A,P,J), I != J, not infered_param(A,P).



%%% Affichage des solutions
#hide.

%#show enum_param/3.
#show result_param/3.

