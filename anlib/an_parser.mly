%{
open Debug;;

open PintTypes;;
open AutomataNetwork;;

let ctx_of_siglocalstates an sls =
	let fold_localstate ctx (a,sig_i) =
		let i = get_automaton_state_id an a sig_i
		in
		Ph_types.ctx_add_proc (a,i) ctx
	in
	let ctx = List.fold_left fold_localstate Ph_types.ctx_empty sls
	in
	let complete_ctx a _ ctx =
		if not (SMap.mem a ctx) then
			SMap.add a (ISet.singleton 0) ctx
		else ctx
	in
	SMap.fold complete_ctx an.automata ctx


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
%left AND
%left OR
%left NOT

%start main
%type <AutomataNetwork.t * Ph_types.ctx> main

%%

main :
  content Eof				{ ($1,Ph_types.ctx_empty) }
| content initial_ctx Eof	{ ($1,ctx_of_siglocalstates $1 $2) }
;

content :
  content decl_automaton 	{ let a, states = $2 in 
  								declare_automaton $1 a states }
| content decl_transition 	{ let a, sigi, sigj, sigconds = $2 in
								declare_transition $1 a sigi sigj sigconds }
| decl_automaton			{ let a, states = $1 in
								declare_automaton (empty_an ()) a states }
;

automaton:
  Label { $1 }
;
state_sig:
  Int	{ StateId $1 }
| Label	{ StateLabel $1 }
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
  automaton state_sig ARROW state_sig						{ ($1, $2, $4, []) }
| automaton state_sig ARROW state_sig WHEN transition_conds	{ ($1, $2, $4, $6) }
;
transition_conds:
  local_state	{ [$1] }
| local_state AND transition_conds	{ $1::$3 }
;
local_state:
  automaton EQUAL state_sig	{ ($1,$3) }
;

local_state_list:
  local_state	{ $1::[] }
| local_state COMMA local_state_list	{ $1::$3 }
;

initial_ctx:
  Initial_state	local_state_list	{ $2 }
| Initial_context local_state_list	{ $2 }


%%
