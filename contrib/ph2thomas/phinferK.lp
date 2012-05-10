%%% Inférence des paramètres de Thomas depuis un Process Hitting



%%% Format des données d'entrée (Automatiquement mis en forme par phinferK.ml)

% Process Hitting (général)
%sort(nom, plafond).                      % Sorte
%cooperative_sort(nom).                   % Sorte coopérative
%cooperation(nom_ab, nom_a, a_i, ab_j).   % Coopération : ab_j représente a_i
%action(a,i,b,j,k).                       % Action a_i -> b_j /> b_k

% Graphe des Interactions du modèle de Thomas
%edge(source, "+"/"-", seuil, cible).     % Arc dans le graphe des interactions
% Éventuellement :
%ka(nom).     % Nom du gène correspondant au paramètre (K_{a,A,B})
%kA(nom).     % Ressources positives
%kB(nom).     % Ressources négatives (telles que kA u kB = G^-1(a) et kA n kB = 0).



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

is_ph_predec(A,B) :- action(A,_,B,_,_).
is_ph_pred(A,B) :- is_ph_predec(A,B).
is_ph_pred(A,B) :- is_ph_pred(A,CS), is_ph_pred(CS,B), is_cs(CS).

% Fonctions pour le modèle de Thomas
is_predec(A,B) :- edge(A,_,_,B).
% Compatibilité
edge(A,"positive",X,B) :- edge(A,"+",X,B).
edge(A,"negative",X,B) :- edge(A,"-",X,B).



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

% Propriété 3 : bien-formé pour l'inférence des paramètres
:- is_predec(B,A), levels_plus(B,A,I), levels_plus(B,A,J), is_ph_pred(C,A), is_ph_pred(B,C), action(B,I,C,K,L), not action(B,J,C,K,L).
:- is_predec(B,A), levels_minus(B,A,I), levels_minus(B,A,J), is_ph_pred(C,A), is_ph_pred(B,C), action(B,I,C,K,L), not action(B,J,C,K,L).
:- is_predec(B,A), levels_plus(B,A,I), levels_plus(B,A,J), B != A, action(B,I,A,K,L), not action(B,J,A,K,L).
:- is_predec(B,A), levels_minus(B,A,I), levels_minus(B,A,J), B != A, action(B,I,A,K,L), not action(B,J,A,K,L).
%debug_NOT_WELL_FORMED(B,I,A,K,L) 

% Sur le Graphe des Interactions (Thomas)
:- edge(A,_,I,_), not is_process(A,I).
:- edge(_,_,_,B), not is_gene(B).
:- edge(_,S,_,_), S != "positive", S != "negative", S != "+", S != "-".
:- edge(_,"?",_).

% Sur le paramètre
:- ka(A), not is_gene(A).
:- ka(A), ka(B), A != B.
:- kA(SB), ka(A), not is_predec(SB,A).
:- kB(SB), ka(A), not is_predec(SB,A).
:- kA(SA), kB(SA).
:- ka(A), is_predec(SB,A), not kA(SB), not kB(SB).



%%% Résolution de l'inférence de paramètres

% Si non précisé, recherche de tous les paramètres
1 {ka(X) : is_gene(X)} 1.
{kA(SA) : is_predec(SA, X)} :- ka(X).
{kB(SB) : is_predec(SB, X)} :- is_gene(SB), ka(X), not kA(SB).

% Niveaux_+(a -> b)
% Niveaux_−(a -> b)
levels_plus(A,B,I) :- edge(A,"positive",T,B), gene(A,P), I = T..P.
levels_plus(A,B,I) :- edge(A,"negative",T,B), I = 0..T-1.
levels_minus(A,B,I) :- edge(A,"negative",T,B), gene(A,P), I = T..P.
levels_minus(A,B,I) :- edge(A,"positive",T,B), I = 0..T-1.
% Context^b_{a,A,B}   % Contexte
sContext(B,I) :- ka(A), kA(B), levels_plus(B,A,I).
sContext(B,I) :- ka(A), kB(B), levels_minus(B,A,I).
sContext(B,I) :- gene(B,P), I = 0..P, ka(A), not is_predec(B,A).
temp_context_cs(CS,B,I) :- cooperation(CS,B,J,I), sContext(B,J).
temp_context_cs_neg(CS,I) :- is_process_of_cs(CS,I), cs_of(B,CS), not temp_context_cs(CS,B,I).
sContext(CS,I) :- is_process_of_cs(CS,I), not temp_context_cs_neg(CS,I).
% H_{a,A,B}           % Actions jouables dans le contexte
sH(B,I,A,J,K) :- action(B,I,A,J,K), ka(A), sContext(B,I), sContext(A,J).
sHJ(A,J) :- is_process(B,I), is_bounce(A,J,K), sH(B,I,A,J,K).
sHJ(A,J) :- is_process_of_cs(B,I), is_bounce(A,J,K), sH(B,I,A,J,K).
% L^?_{a,A,B}         % Processus atteignables
sL_att(I) :- ka(A), sContext(A,I).
sL_att(K) :- ka(A), sH(B,I,A,J,K).
% L^*_{a,A,B}         % Processus focaux (atteignables non frappés)
sL_foc(J) :- sL_att(J), ka(A), not sHJ(A,J).

% Les processus focaux sont attracteurs ?
%attractor :- ka(A), sH(B,I,A,J,K), sL_foc(F), #abs(F-K) < #abs(F-J).
%exists_hit_to(A) :- sH(_,_,A,_,_).
%attractor :- ka(A), not exists_hit_to(A).
%%%%%%%%%%
temp_is_attractor(F,K,J) :- ka(A), sH(B,I,A,J,K), sL_foc(F), #abs(F-K) < #abs(F-J).
not_attractor :- ka(A), sH(B,I,A,J,K), sL_foc(F), not temp_is_attractor(F,K,J).
attractor :- not not_attractor.
%%%%%%%%%%


% Réponse finale pour l'inférence des paramètres
exists_sL_foc :- sL_foc(_).
cannot_infer :- not exists_sL_foc.
cannot_infer :- not attractor.
result("no") :- cannot_infer.
result("ok") :- not cannot_infer.

error(A) :- ka(A), not attractor.

%%% Résolution de l'inférence du Graphe des Interactions




%%% Affichage des solutions
#hide.

% Inférence des paramètres
#show ka/1.
#show kA/1.
#show kB/1.
%#show sContext/2.
%#show sH/5.
%#show sL_att/1.
#show sL_foc/1.
%#show sHJ/2.
%#show attractor.
%#show exists_sL_foc.
%#show cannot_infer.

#show result/1.
#show error/1.

