%%% Inférence des sortes coopératives dans un Process Hitting



%%% Format des données d'entrée (Automatiquement mis en forme par phinferK.ml)

% Process Hitting (général)
%sort(nom, plafond).                      % Sorte
%action(a,i,b,j,k).                       % Action a_i -> b_j /> b_k



%%% Fonctions standard

% Fonctions pour Process Hitting
is_sort(A) :- sort(A,_).
is_process(A,I) :- sort(A,P), I = 0..P.
is_bounce(A,I,J) :- is_process(A,I), is_process(A,J), I != J.
is_ph_predec(A,B) :- action(A,_,B,_,_).



%%% Contraintes d'intégrité

% Sur le Process Hitting
:- sort(_,P), P <= 0.
:- action(A,I,_,_,_), not is_process(A,I).
:- action(_,_,B,J,K), not is_bounce(B,J,K).
:- action(A,I,A,J,K), I != J.



%%% Construction de Gamma et vérification de sa validité

% Définition de Gamma
must_be_cs(A) :- action(_,_,A,J,K), #abs(J-K) != 1.
gamma(A) :- is_sort(A), not must_be_cs(A).

% Contraintes d'intégrité sur la séparation composants/sortes coopératives

%% Les sortes non frappées sont dans gamma
%temp_targeted(A) :- action(_,_,A,_,_).
%error("CS is not the target of an action",CS) :- must_be_cs(CS), not temp_targeted(CS).

% Pas de cycles entre sortes coopératives
is_ph_pred(A,B) :- is_ph_predec(A,B).
is_ph_pred(A,B) :- is_ph_pred(A,CS), is_ph_pred(CS,B), not gamma(CS).
is_ph_reg(A,B) :- is_ph_pred(A,B), gamma(A).
error("Cooperative sort involved in a cycle",CS) :- must_be_cs(CS), is_ph_pred(CS,CS).



% Sortes possiblement coopératives
1 {ecs(CS) : must_be_cs(CS)} 1.
is_process_of_ecs(J) :- is_process(CS,J), ecs(CS).
%predec_ecs(A) :- ecs(CS), action(A,_,CS,_,_).

% Contrainte : exactement un unique point fixe par configuration
1 {ep(A,I) : is_process(A,I)} 1 :- is_ph_predec(A,CS), ecs(CS).

% Pas de cycles dans les bounces
bounce_predec(X,Y) :- action(A,I,CS,X,Y), ecs(CS), ep(A,I).
bounce_predec(X,Z) :- ecs(CS), bounce_predec(X,Y), bounce_predec(Y,Z).

% Processus focaux (focals)
exists_bounce_from(J) :- bounce_predec(J,_).
ecs_p(J) :- is_process_of_ecs(J), not exists_bounce_from(J).
exists_ecs_p :- ecs_p(_).
error("Not a well-formed cooperative sort (config with several focals)",CS) :- ecs_p(I), ecs_p(J), I != J, ecs(CS).
error("Not a well-formed cooperative sort (config with a cycle, thus no focal)",CS) :- ecs(CS), bounce_predec(X,X).
%error("Not a well-formed cooperative sort (config with no focal)",CS) :- not exists_ecs_p, ecs(CS).


% Construction des coopérations
cooperation(CS,A,I,J) :- ecs(CS), ep(A,I), ecs_p(J).



%*
%ecs_not_p(J) :- ecs(CS), is_process_of_ecs(J), ep(A,I), not cooperation(CS,A,I,J).
%ecs_p(J) :- is_process_of_ecs(J), not ecs_not_p(J).

%exists_ecs_p :- ecs_p(J).
%cannot_be_cs(CS) :- ecs(CS), ecs_p(J), ecs_p(K), J != K.  % Non : cas de représentation de plusieurs processus
%                                                          % Si : comportement non désiré
%cannot_be_cs(CS) :- ecs(CS), is_process_of_ecs(J), not exists_ecs_p.
*%



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

