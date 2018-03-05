%{
open Debug
open PintTypes
open AutomataNetwork
%}

%token <string> Label
%token <string> Name
%token <float> Float
%token <int> Int
%token Eof
%token WHEN EQUAL
%token AND OR NOT IN
%token ARROW INFTY
%token Initial_state Initial_context
%token COMMA LBRACKET LCURLY LPAREN RBRACKET RCURLY RPAREN SEMI SHARP STAR AT

%token <char> Sign

%left IN
%left AND COMMA
%left OR
%left NOT

%start main
%type <AutomataNetwork.t * AutomataNetwork.ctx> main

%start local_state
%type <AutomataNetwork.sig_local_state> local_state

%start local_state_list
%type <AutomataNetwork.sig_local_state list> local_state_list

%start automata_set
%type <PintTypes.SSet.t> automata_set

%start goals
%type <AutomataNetwork.sig_state list list> goals

%%

main :
  Eof { let an = new_an () in (an, ctx_of_siglocalstates ~complete:true an []) }
| content Eof				{ ($1,ctx_of_siglocalstates ~complete:true $1 []) }
| content initial_ctx Eof	{ ($1,ctx_of_siglocalstates ~complete:true $1 $2) }
;

content :
  content decl_automaton 	{ let a, states = $2 in
  								declare_automaton $1 a states; $1 }
| content decl_transition 	{ let a, sigi, sigj, sigconds = $2 in
								declare_transition $1 [a,sigi,sigj] sigconds; $1 }
| content decl_sync_transition
							{ let trs, trsconds = $2 in
								declare_transition $1 trs trsconds; $1 }
| decl_automaton			{ let an = new_an () in
								let a, states = $1 in
									declare_automaton an a states; an }
;

automaton:
  Label { $1 }
| Name	{ $1 }
;
state_sig:
  Int	{ StateId $1 }
| Label	{ StateLabel $1 }
| Name	{ StateLabel $1 }
;

decl_automaton:
  automaton state_sig_list	{ ($1, $2) }
;
state_sig_list: LBRACKET state_sig_list_t RBRACKET { $2 };
state_sig_list_t:
  state_sig { [$1] }
| state_sig COMMA state_sig_list_t { $1::$3 }
;

decl_transition:
  transition { let a,i,j = $1 in (a,i,j,SMap.empty) }
| transition WHEN transition_conds { let a,i,j = $1 in (a,i,j,$3) }
;
decl_sync_transition:
  transition_set { ($1,SMap.empty) }
| transition_set WHEN transition_conds { ($1,$3) }
;
transition_set: LCURLY transition_set_t RCURLY { $2 };
transition_set_t:
  transition SEMI transition { $1::$3::[] }
| transition SEMI transition_set_t { $1::$3 }
;
transition:
	automaton state_sig ARROW state_sig	{ ($1, $2, $4) }
;
transition_conds:
  local_state	{ let a,i = $1 in SMap.singleton a i }
| local_state AND transition_conds { let a,i = $1 and conds = $3 in
                                     (if SMap.mem a conds then
                                        failwith ("'"^a^"' referenced twice."));
                                     SMap.add a i conds }
;

state:
  local_state	{ let a,i = $1 in SMap.singleton a i }
| local_state COMMA state { let a,i = $1 and conds = $3 in
                                     (if SMap.mem a conds then
                                        failwith ("'"^a^"' referenced twice."));
                                     SMap.add a i conds }
;

local_state:
  automaton EQUAL state_sig	{ ($1,$3) }
;

local_state_list:
  local_state	{ $1::[] }
| local_state COMMA local_state_list	{ $1::$3 }
;

initial_ctx:
  Initial_state	state	{ SMap.bindings $2 }
| Initial_context local_state_list	{ $2 }
;

automata_set:
  automaton { SSet.singleton $1 }
| automaton Eof { SSet.singleton $1 }
| automaton COMMA automata_set { SSet.add $1 $3 }
;

goal:
  state { $1::[] }
| state goal { $1::$2 }
;

goals:
  goal  { $1::[] }
| goal Eof { $1::[] }
| goal OR goals { $1::$3 }
;

%%
