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



%%% Contraintes d'intégrité

% Sur le Process Hitting
:- sort(_,P), P <= 0.
:- action(A,I,_,_,_), not is_process(A,I).
:- action(_,_,B,J,K), not is_bounce(B,J,K).
:- action(A,I,A,J,K), I != J.



%%% Inférence des sortes coopératives
% Contraintes d'intégrité
must_be_cs(A) :- action(_,_,A,J,K), #abs(J-K) != 1.
cannot_be_cs_f(A) :- sort(A,P), P < 3.
temp_hitter(A) :- action(A,_,_,_,_).
cannot_be_cs_f(A) :- is_sort(A), not temp_hitter(A).
cannot_be_cs_f(A) :- action(A,_,A,_,_).
:- must_be_cs(A), cannot_be_cs_f(A).

% Sortes possiblement coopératives
%ecs(CS) :- is_sort(CS), not cannot_be_cs(CS).
1 {ecs(CS) : is_sort(CS)} 1.
:- ecs(CS), cannot_be_cs_f(CS).
is_process_of_ecs(J) :- is_process(CS,J), ecs(CS).
predec_ecs(A) :- ecs(CS), action(A,_,CS,_,_).

% Définition et contraintes sur les coopérations
cooperation(A,I,J) :- ecs(CS), action(A,I,CS,_,J).

:- cooperation(A,I,J), ecs(CS), action(A,I,CS,J,_).
temp_at_least_one_cooperation(A,J) :- is_process(A,I), is_process_of_ecs(J), predec_ecs(A), cooperation(A,I,J).
:- is_process_of_ecs(J), is_sort(A), predec_ecs(A), not temp_at_least_one_cooperation(A,J).

% Contrainte : un unique point fixe par config
1 {ep(A,I) : is_process(A,I)} 1 :- predec_ecs(A).
ecs_not_p(J) :- is_process_of_ecs(J), ep(A,I), not cooperation(A,I,J).
ecs_p(J) :- is_process_of_ecs(J), not ecs_not_p(J).
exists_ecs_p :- ecs_p(J).
cannot_be_cs(CS) :- ecs(CS), ecs_p(J), ecs_p(K), J != K.  % Non : cas de représentation de plusieurs processus
                                                          % Si : comportement non désiré
cannot_be_cs(CS) :- ecs(CS), is_process_of_ecs(J), not exists_ecs_p.



%%% Affichage des solutions
#hide.

%#show sort/2.
%#show is_cs/1.
%#show ep/3.
%#show ecs_p/1.
%#show ecs/1.
#show cannot_be_cs/1.
%#show cannot_be_cs_1/1.
%#show cannot_be_cs_2/1.
%#show cannot_be_cs_f/1.
%#show cooperative_sort/2.
%#show cooperation/4.

