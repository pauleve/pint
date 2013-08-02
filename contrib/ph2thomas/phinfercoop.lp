%%% Inférence des sortes coopératives dans un Process Hitting



%%% Format des données d'entrée (Automatiquement mis en forme par phinferK.ml)

% Process Hitting (général)
%sort(nom, plafond).                      % Sorte
%action(a,i,b,j,k).                       % Action a_i -> b_j /> b_k



%%% Fonctions standard

%% Fonctions pour Process Hitting
is_sort(A) :- sort(A,_).
is_process(A,AI) :- sort(A,AP), AI=0..AP.
is_bounce(A,AI,AJ) :- is_process(A,AI), is_process(A,AJ), AI!=AJ.
is_ph_predec(A,B) :- action(A,_,B,_,_).



%%% Contraintes d'intégrité

%% Sur le Process Hitting
:- sort(_,P), P <= 0.
:- action(A,AI,_,_,_), not is_process(A,AI).
:- action(_,_,B,BJ,BK), not is_bounce(B,BJ,BK).
:- action(A,AI,A,AJ,AK), AI!=AJ.



%%% Séparation composants/sortes coopératives

%% Définition de Gamma (composants)
must_be_cs(A) :- action(_,_,A,AJ,AK), #abs(AJ-AK)!=1.
gamma(A) :- is_sort(A), not must_be_cs(A).

%% Pas de cycles entre sortes coopératives
is_ph_pred(A,B) :- is_ph_predec(A,B), A!=B.
is_ph_pred(A,B) :- is_ph_pred(A,CS), is_ph_pred(CS,B), not gamma(CS).
is_ph_reg(A,B) :- is_ph_pred(A,B), gamma(A).
error("Cooperative sort involved in a cycle",CS) :- must_be_cs(CS), is_ph_pred(CS,CS).



%%% Recherche des coopérations sur les sortes coopératives

%% Sortes possiblement coopératives
1 {ecs(CS) : must_be_cs(CS)} 1.
is_process_of_ecs(CSJ) :- ecs(CS), is_process(CS,CSJ).

%% Contrainte : exactement un unique point fixe par configuration
1 {ep(A,AI) : is_process(A,AI)} 1 :- ecs(CS), is_ph_predec(A,CS).

%% Détection des cycles dans les bounces
bounce_predec(CSX,CSY) :- ecs(CS), ep(A,AI), action(A,AI,CS,CSX,CSY).
bounce_predec(CSX,CSZ) :- ecs(CS), bounce_predec(CSX,CSY), bounce_predec(CSY,CSZ).

%% Processus focaux (focals)
exists_bounce_from(CSJ) :- bounce_predec(CSJ,_).
ecs_p(CSJ) :- is_process_of_ecs(CSJ), not exists_bounce_from(CSJ).
exists_ecs_p :- ecs_p(_).
error("Not a well-formed cooperative sort (config with several focals)",CS) :- ecs(CS),
  ecs_p(CSI), ecs_p(CSJ), CSI!=CSJ.
error("Not a well-formed cooperative sort (config with a cycle, thus no focal)",CS) :- ecs(CS),
  bounce_predec(CSX,CSX).



%%% Construction des coopérations
cooperation(CS,A,AI,CSJ) :- ecs(CS), ep(A,AI), ecs_p(CSJ).



%%% Affichage des solutions
#hide.

%#show sort/2.
%#show is_cs/1.
%#show ep/3.
%#show ecs_p/1.
%#show ecs/1.
%#show cannot_be_cs/1.
%#show cannot_be_cs_final/1.
#show ecs/1.
#show cooperation/4.

#show error/2.

