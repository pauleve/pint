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
is_bounce_of_cs(CS,I,J) :- is_process_of_cs(CS,I), is_process_of_cs(CS,J), I != J.

% Fonctions pour le modèle de Thomas
is_predec(A,B) :- edge(A,_,_,B).
% Compatibilité
edge(A,"positive",X,B) :- edge(A,"+",X,B).
edge(A,"negative",X,B) :- edge(A,"-",X,B).

% Fonctions concernant la paramétrisation
gene_levels(A,0..I) :- gene(A,I).

temp_less_active(A,P1,P2) :- param_label(A,P1), param_label(A,P2,B), not param_label(A,P1,B).
less_active(A,P1,P2) :- temp_less_active(A,P1,P2), not temp_less_active(A,P2,P1).

temp_exists_more_active(A,P) :- less_active(A,P,_).
max_active(A,P) :- param_label(A,P), not temp_exists_more_active(A,P).
temp_exists_less_active(A,P) :- less_active(A,_,P).
min_active(A,P) :- param_label(A,P), not temp_exists_less_active(A,P).

%is_param(P) :- param_label(_,P).



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

% Sur le Graphe des Interactions (Thomas)
:- edge(A,_,I,_), not is_process(A,I).
:- edge(_,_,_,B), not is_gene(B).
:- edge(_,S,_,_), S != "positive", S != "negative", S != "+", S != "-".
:- edge(_,"?",_).

% Sur les paramètres
:- param_label(B,_), not is_gene(B).
:- param_label(_,P), P <= 0.
:- param_label(B,P,_), not param_label(B,P).
:- param_label(_,_,A), not is_gene(A).
:- param_label(B,_,A), not is_predec(A,B).
:- param(A,P,_), not param_label(A,P).
:- param(A,_,I), not gene_levels(A,I).



%%% Résolution de l'énumération des paramètres possibles
% Reprendre la valeur des paramètres inférés
infered_param(A,P) :- param(A,P,_).
enum_param(A,P,V) :- infered_param(A,P), param(A,P,V).
% Au moins une valeur par paramètre
1 {enum_param(A,P,V) : gene_levels(A,V)} :- is_gene(A), param_label(A,P), not infered_param(A,P).
%% Restriction aux plages trouvées pour les paramètres déjà inférés
% Non : pas de comportements supplémentaires
%infered_param(A,P) :- param(A,P,_).
%:- enum_param(A,P,V), infered_param(A,P), not param(A,P,V).
% Bornes supérieure et inférieure
temp_has_predec(A) :- is_predec(_,A).
enum_param(A,P,0) :- min_active(A,P), temp_has_predec(A).
enum_param(A,P,I) :- gene(A,I), max_active(A,P), temp_has_predec(A).
% Pas de trou dans les intervalles
exists_param_more(A,P,I) :- enum_param(A,P,I), enum_param(A,P,J), I < J.
param_max(A,P,I) :- enum_param(A,P,I), not exists_param_more(A,P,I).
exists_param_less(A,P,I) :- enum_param(A,P,I), enum_param(A,P,J), J < I.
param_min(A,P,I) :- enum_param(A,P,I), not exists_param_less(A,P,I).
:- param_min(A,P,Min), param_max(A,P,Max), I = Min..Max, not enum_param(A,P,I).
% Ordre entre les paramètres
%% Monotonicité [Def 3.5 de Bernot et al.] (Moins activé => inférieur ou égal)
param_inf(A,P1,P2) :- param_min(A,P1,Min1), param_max(A,P1,Max1), param_min(A,P2,Min2), param_max(A,P2,Max2), Min1 <= Min2, Max1 <= Max2.
:- less_active(A,P1,P2), not param_inf(A,P1,P2).
%% Activation [Def 3.4 de Bernot et al.] (Plus activé => au moins un strictement supérieur)
param_str_inf(A,P1,P2) :- param_min(A,P1,Min1), param_max(A,P1,Max1), param_min(A,P2,Min2), param_max(A,P2,Max2), Min1 < Min2, Max1 <= Max2.
param_str_inf(A,P1,P2) :- param_min(A,P1,Min1), param_max(A,P1,Max1), param_min(A,P2,Min2), param_max(A,P2,Max2), Min1 <= Min2, Max1 < Max2.
temp_activation_ok(A,B) :- is_predec(B,A), param_label(A,P1), param_label(A,P2), param_label(A,P2,B), not param_label(A,P1,B), param_str_inf(A,P1,P2).
:- is_gene(A), is_predec(B,A), not temp_activation_ok(A,B).

% Paramètres simples uniquement
%% À activer si souhaité
%:- enum_param(A,P,I), enum_param(A,P,J), I != J, not infered_param(A,P).

% Respect de la dynamique du PH
%% Contexte
% Niveaux +/−
levels_plus(A,B,I) :- edge(A,"positive",T,B), gene(A,P), I = T..P.
levels_plus(A,B,I) :- edge(A,"negative",T,B), I = 0..T-1.
levels_minus(A,B,I) :- edge(A,"negative",T,B), gene(A,P), I = T..P.
levels_minus(A,B,I) :- edge(A,"positive",T,B), I = 0..T-1.
% Context^b_{a,A,B}   % Contexte
sContext(A,P,B,I) :- levels_plus(B,A,I), param_label(A,P,B).
sContext(A,P,B,I) :- param_label(A,P), levels_minus(B,A,I), not param_label(A,P,B).
sContext(A,P,B,I) :- gene(B,L), I = 0..L, param_label(A,P), not is_predec(B,A).
temp_context_cs(A,P,CS,B,I) :- cooperation(CS,B,J,I), sContext(A,P,B,J).
temp_context_cs_neg(A,P,CS,I) :- param_label(A,P), is_process_of_cs(CS,I), cs_of(B,CS), not temp_context_cs(A,P,CS,B,I).
sContext(A,P,CS,I) :- param_label(A,P), is_process_of_cs(CS,I), not temp_context_cs_neg(A,P,CS,I).
% Actions possibles
exists_action(A,P,I,J) :- action(C,K,A,I,J), sContext(A,P,C,K).
% Contraintes
:- is_bounce(A,I,J), not enum_param(A,P,I), not exists_action(A,P,I,J), I < KMin, param_min(A,P,KMin), J > I, #abs(I - J) == 1.
:- is_bounce(A,I,J), not enum_param(A,P,I), not exists_action(A,P,I,J), I > KMin, param_min(A,P,KMin), J < I, #abs(I - J) == 1.
%debug_NOT_VALID1(A,P,I,J) :- is_bounce(A,I,J), not enum_param(A,P,I), not exists_action(A,P,I,J), I < KMin, param_min(A,P,KMin), J > I, #abs(I - J) == 1.
%debug_NOT_VALID2(A,P,I,J) :- is_bounce(A,I,J), not enum_param(A,P,I), not exists_action(A,P,I,J), I > KMin, param_min(A,P,KMin), J < I, #abs(I - J) == 1.

%%% Autres ?


%%% Affichage des solutions
#hide.

% Inférence des paramètres
%#show gene_levels/2.
%#show temp_less_active/3.
%#show less_active/3.

%#show temp_has_predec/1.
%#show max_active/2.
%#show min_active/2.

%#show param/3.
#show enum_param/3.

%#show param_label/3.
%#show sContext/4.
%#show exists_action/4.

#show result/1.
#show error/1.

%#show debug_NOT_VALID1/4.
%#show debug_NOT_VALID2/4.

