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

%% Fonctions pour Process Hitting
gene(A,AP) :- sort(A,AP), not cooperative_sort(A).
is_gene(A) :- gene(A,_).
is_process(A,AI) :- gene(A,AP), AI=0..AP.
is_bounce(A,AI,AJ) :- is_process(A,AI), is_process(A,AJ), AI!=AJ.

cooperative_sort(CS,CSP) :- cooperative_sort(CS), sort(CS,CSP).
is_cs(CS) :- cooperative_sort(CS,_).  % cs = cooperative sort
is_gene_or_cs(A) :- is_gene(A).
is_gene_or_cs(CS) :- is_cs(CS).
cs_of(A,CS) :- cooperation(CS,A,_,_).
is_process_of_cs(CS,CSI) :- cooperative_sort(CS,CSP), CSI=0..CSP.
is_bounce_of_cs(CS,CSI,CSJ) :- is_process_of_cs(CS,CSI), is_process_of_cs(CS,CSJ), CSI!=CSJ.

is_ph_predec(A,B) :- action(A,_,B,_,_).
is_ph_pred(A,B) :- is_ph_predec(A,B).
is_ph_pred(A,B) :- is_ph_pred(A,CS), is_ph_pred(CS,B), is_cs(CS).

%% Fonctions pour le modèle de Thomas
is_ig_predec(A,B) :- edge(A,_,_,B).
%% Compatibilité
edge(A,"positive",X,B) :- edge(A,"+",X,B).
edge(A,"negative",X,B) :- edge(A,"-",X,B).
edge(A,"unsigned",X,B) :- edge(A,"?",X,B).



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
at_least_one_cooperation_debug(CS,A,CSJ) :- is_process(A,AI), is_process_of_cs(CS,CSJ),
  cs_of(A,CS), cooperation(CS,A,AI,CSJ).
:- is_process_of_cs(CS,CSJ), is_gene(A), cs_of(A,CS),
  not at_least_one_cooperation_debug(CS,A,CSJ).

%% Propriété 3 : bien-formé pour l'inférence des paramètres
:- is_ig_predec(B,A), levels(B,A,BI), levels(B,A,BJ), is_ph_pred(C,A), is_ph_predec(B,C),
  is_cs(C), action(B,BI,C,CK,CL), not action(B,BJ,C,CK,CL).
:- is_ig_predec(B,A), ulevels(B,A,BI), ulevels(B,A,BJ), is_ph_pred(C,A), is_ph_predec(B,C),
  is_cs(C), action(B,BI,C,CK,CL), not action(B,BJ,C,CK,CL).
:- is_ig_predec(B,A), levels(B,A,BI), levels(B,A,BJ), B!=A, action(B,BI,A,AK,AL),
  not action(B,BJ,A,AK,AL).
:- is_ig_predec(B,A), ulevels(B,A,BI), ulevels(B,A,BJ), B!=A, action(B,BI,A,AK,AL),
  not action(B,BJ,A,AK,AL).

% Sur le Graphe des Interactions (Thomas)
:- edge(A,_,AI,_), not is_process(A,AI).
:- edge(_,_,_,B), not is_gene(B).
:- edge(_,S,_,_), S!="positive", S!="negative", S!="unsigned", S!="+", S!="-", S!="?".

% Sur le paramètre
:- ka(A), not is_gene(A).
:- ka(A), ka(B), A!=B.
:- kA(SB), ka(A), not is_ig_predec(SB,A).
:- kB(SB), ka(A), not is_ig_predec(SB,A).
:- kA(SA), kB(SA).
:- ka(A), is_ig_predec(SB,A), not kA(SB), not kB(SB).



%%% Résolution de l'inférence de paramètres

%% Recherche de tous les paramètres
1 {ka(X) : is_gene(X)} 1.
{kA(SA) : is_ig_predec(SA, X)} :- ka(X).
{kB(SB) : is_ig_predec(SB, X)} :- is_gene(SB), ka(X), not kA(SB).

%% Niveaux_+(a -> b)
%% Niveaux_−(a -> b)
levels(A,B,AI) :- edge(A,_,AT,B), gene(A,AP), AI=AT..AP.
ulevels(A,B,AI) :- edge(A,_,AT,B), AI=0..AT-1.
%% Context^b_{a,A,B}   % Contexte
sContext(B,BI) :- ka(A), kA(B), levels(B,A,BI).
sContext(B,BI) :- ka(A), kB(B), ulevels(B,A,BI).
sContext(B,BI) :- gene(B,BP), BI=0..BP, ka(A), not is_ig_predec(B,A).
temp_context_cs(CS,B,CSI) :- cooperation(CS,B,BJ,CSI), sContext(B,BJ).
temp_context_cs_neg(CS,CSI) :- is_process_of_cs(CS,CSI), cs_of(B,CS),
  not temp_context_cs(CS,B,CSI).
sContext(CS,CSI) :- is_process_of_cs(CS,CSI), not temp_context_cs_neg(CS,CSI).
%% H_{a,A,B}           % Actions jouables dans le contexte
sH(B,BI,A,AJ,AK) :- action(B,BI,A,AJ,AK), ka(A), sContext(B,BI), sContext(A,AJ).
sHJ(A,AJ) :- is_process(B,BI), is_bounce(A,AJ,AK), sH(B,BI,A,AJ,AK).
sHJ(A,AJ) :- is_process_of_cs(B,BI), is_bounce(A,AJ,AK), sH(B,BI,A,AJ,AK).
%% L^?_{a,A,B}         % Processus atteignables
sL_att(AI) :- ka(A), sContext(A,AI).
sL_att(AK) :- ka(A), sH(_,_,A,_,AK).
%% L^*_{a,A,B}         % Processus focaux (atteignables non frappés)
sL_foc(AJ) :- sL_att(AJ), ka(A), not sHJ(A,AJ).

%% Les processus focaux sont attracteurs ?
temp_is_attractor(AF,AK,AJ) :- ka(A), sH(_,_,A,AJ,AK), sL_foc(AF), #abs(AF-AK)<#abs(AF-AJ).
not_attractor :- ka(A), sH(_,_,A,AJ,AK), sL_foc(AF), not temp_is_attractor(AF,AK,AJ).
attractor :- not not_attractor.



%%% Conclusion
exists_sL_foc :- sL_foc(_).
cannot_infer :- not exists_sL_foc.
cannot_infer :- not attractor.
result("no") :- cannot_infer.
result("ok") :- not cannot_infer.

error(A) :- ka(A), not attractor.



%%% Affichage des solutions
#hide.

#show ka/1.
#show kA/1.
#show kB/1.
#show sL_foc/1.

#show result/1.
#show error/1.

