%{
open Debug;;

open PintTypes;;
open Ph_types;;
open InteractionGraph;;


let interactiongraph_of_regulations regs =
	let fold_regulation ig = function
		Regulation (a, th, s, b, _) ->
			let preds = try SMap.find b ig with Not_found -> []
			in
			SMap.add b ((a,th,s)::preds) ig
	in
	List.fold_left fold_regulation SMap.empty regs
;;

type options_t = {
	mutable autoinit : bool;
}
let options = {
	autoinit = true;
}

let default_rsa () = 
	match directive.default_rate with
	  None -> Instantaneous
	| Some r -> RateSA (r, directive.default_sa)
;;

let set_option name b =
	match name with
	  "autoinit" -> options.autoinit <- b
	| _ -> failwith ("Unknown option '"^name^"'")
;;

let init_ph (mps, actions) = (SMap.bindings mps, actions);;

let init_content (ps, actions) =
	let hits = Hashtbl.create (List.length actions)
	in
	let register_action = function Hit (ai,bj,j'), rsa ->
		Hashtbl.add hits bj ((ai,rsa),j')
	in
	List.iter register_action actions;
	(ps, hits)
;;

let merge_decl (mps, actions) (a,l) =
	SMap.add a l mps, actions
;;

let get_sort_max mps a =
	try
		SMap.find a mps
	with Not_found -> 
		failwith ("get_sort_max: unknown sort '"^a^"'")
;;

let merge_instr (mps, actions) ((a,i),(b,j),k,stoch) = 
(* DISABLED FOR PERFORMANCE ISSUES
	let ma, mb = get_sort_max ps a, get_sort_max ps b
	in
	let check_boundaries (a,i) ml =
		if i < 0 || ml < i then 
			failwith (string_of_proc (a,i)^": level out of bound (max is "^(string_of_int ml)^")")
	in
	check_boundaries (a,i) ma;
	check_boundaries (b,j) mb;
	check_boundaries (b,k) mb;
	*)
	(mps, (Hit ((a,i),(b,j),k),stoch)::actions)
;;

let ph_add_hits actions stoch rawactions =
	let actions' = List.map (fun a -> (a,stoch)) rawactions
	in
	List.rev_append actions' actions
;;

let compute_init_context ph procs =


	let apply_settings ctx =
		(* override with model settings *)
		let ctx = ctx_override ctx procs
		in
		(* override with user settings *)
		ctx_override ctx !Ph_useropts.initial_procs
	in

	(* by default, each process 0 is present *)
	let ctx0 = ctx_of_state (state0 (fst ph))
	in
	(* validate settings *)
	let validate_proc (a,_) = if not (SMap.mem a ctx0) then failwith ("unknown sort '"^a^"'")
	in
	PSet.iter validate_proc procs;
	PSet.iter validate_proc !Ph_useropts.initial_procs;
	let ctx = apply_settings ctx0
	in
	(* apply cooperativities *)
	let fold c _ ctx =
		let is = Ph_cooperativity.resolve !Ph_instance.cooperativities ctx c
		in
		let fold ps i =
			PSet.add (c,i) ps
		in
		let procs_c = List.fold_left fold PSet.empty is
		in
		dbg ("- init cooperativity: "^string_of_procs procs_c);
		ctx_override ctx procs_c
	in
	(*TODO: handle nested cooperativities *)
	let ctx =
		if !Ph_useropts.autoinit = (Some true) ||
			!Ph_useropts.autoinit = None && options.autoinit then
			SMap.fold fold !Ph_instance.cooperativities ctx
		else ctx
	in
	(* re-apply settings (force cooperative states) *)
	apply_settings ctx
;;

(***
    ACTIONS UPDATE
***)
type levelmatch = MatchAnyLevel | MatchOnlyLevel of sortidx;;
type sortmatch = MatchAnySort | MatchOnlySort of sort;;

let match_sort = function 
	  MatchAnySort -> (fun _ -> true)
	| MatchOnlySort a' -> (fun a -> a = a')
;;
let match_level = function 
	  MatchAnyLevel -> (fun _ -> true)
	| MatchOnlyLevel i' -> (fun i -> i = i')
;;

let match_proc (ma,mi) (a,i) = match_sort ma a && match_level mi i
;;

let update (mps,actions) (regexps, rate) =
	let regexp_match a =
		let ai, bj, k = match a with Hit d -> d
		in
		function (mai,mbj,mk) ->
			match_proc mai ai && match_proc mbj bj && match_level mk k
	in
	let match_action a = List.exists (regexp_match a) regexps
	in
	let update_action (a,rsa) =
		a, if match_action a then rate else rsa
	in
	mps, List.map update_action actions
;;

(***
	MACROS
***)

type macro_arg_t =
	  Arg_Name of string
	| Arg_Int of int
	| Arg_Process of process
	| Arg_Actions of (process * process * int) list
	| Arg_Regulation of regulation_t
	| Arg_Regulations of regulation_t list
	| Arg_AnoStates of anostate list
	| Arg_Sorts of sort list
	| Arg_NamesHit of string list * process * int * stochatime
	| Arg_StateMatching of state_matching_t
;;

let build_reflection ?coop_label:(coop_label=None) (mps,actions) sigma =
	let (record, append) = Ph_cooperativity.build_reflection 
													~coop_label 
													~rsa:(default_rsa ())
													(get_sort_max mps) sigma
	in
	let mps, actions = match append with None -> (mps, actions) 
						| Some ((a,i), hs) -> SMap.add a i mps, actions @ hs
	in
	record, (mps, actions)


let macro_regulation = function
	  [Arg_Regulation regulation] -> (fun (mps,actions) ->
	  	match regulation with
	Regulation (a,t,s,b,stoch) ->
		let la = get_sort_max mps a and lb = get_sort_max mps b
		in
		let apply_regulation i =
			let r = if i >= t then s else match s with Positive -> Negative |
												Negative -> Positive
			in
			let make_hit j =
				Hit((a,i), (b,j), if r = Positive then j+1 else j-1)
			in
			let range_j = Util.range (if r = Positive then 0 else 1)
								(if r = Negative then lb else (lb-1))
			in
			let range_j = if a = b then 
					if List.mem i range_j then [i] else []
				else range_j
			in
			List.map make_hit range_j

		in
		mps, ph_add_hits actions stoch (List.flatten (List.map apply_regulation (Util.range 0 la)))
	)
	| _ -> failwith "macro_regulation: wrong arguments"
;;
let macro_brn do_unregulated = function
[Arg_Regulations regulations] -> (fun ctx ->

	Ph_instance.interaction_graph := 
			interactiongraph_of_regulations	regulations;

	let folder (regulations, genes, regulated) = function
		Regulation (a,t,s,b,stoch) ->
			let n_regulated = SSet.singleton b
			in
			let n_genes = SSet.add a n_regulated
			in
			Regulation (a,t,s,b,stoch)::regulations,
			SSet.union genes n_genes,
			SSet.union regulated n_regulated
	in
	let init = ([], SSet.empty, SSet.empty)
	in
	let regulations, genes, regulated = List.fold_left folder init regulations
	in
	let regulations = if not do_unregulated then regulations else (
		let unregulateds = SSet.diff genes regulated
		in
		let folder a regulations =
			Regulation (a,0,Negative,a, default_rsa ())::regulations
		in
		SSet.fold folder unregulateds regulations)
	in
	let folder ctx regulation =
		macro_regulation [Arg_Regulation regulation] ctx
	in
	List.fold_left folder ctx regulations
)
| _ -> failwith "macro_grn: wrong arguments"
;;

let filter_hits (mps, actions) pred =
	(mps, List.filter (fun (a,rsa) -> pred a) actions)
;;

let rec macro_cooperativity ?coop_label:(coop_label=None) autoremove = function
  (* COOPERATIVITY([a;b]): creates a cooperative sort between a and b *)
  [Arg_Sorts sorts] -> (fun (ps,actions) -> snd (build_reflection ~coop_label:coop_label (ps,actions) sorts))

| [Arg_NamesHit (sigma, ak, k', stoch); Arg_AnoStates top] -> (fun (ps,actions) ->
	let (sigma_n, (sigma, idx_from_state)), (ps,actions) = build_reflection ~coop_label:coop_label (ps,actions) sigma
	in
	let (ps, actions) = if not autoremove then (ps, actions) else
		filter_hits (ps,actions) (function Hit ((a,i),bj,j') -> 
						not (List.mem a sigma && bj = ak && j' = k'))
	in
	let h'coop = List.map (fun state ->
		Hit ((sigma_n, idx_from_state state), ak, k'))
			top
	in
	ps, ph_add_hits actions stoch h'coop
)

| [Arg_StateMatching sm; Arg_Name sort; Arg_Int l_true; Arg_Int l_false] -> (fun ctx ->

	let m_default_rsa = default_rsa ()
	in
	(* remove existing hits from sorts in sm to processes (sort,l_true|l_false) *)
	let rec matching_names = function
		  SM (sigma, _) -> sigma
		| SM_Not sm -> matching_names sm
		| SM_And (sm1, sm2) | SM_Or (sm1, sm2) -> matching_names sm1 @ matching_names sm2
	in
	let names = matching_names sm
	in
	let ctx = if not autoremove then ctx else
		filter_hits ctx (function Hit((b,k), (a,i), j) ->
			not (List.mem b names && a = sort &&
				(i == l_true && j == l_false || i == l_false && j == l_true)))
	in
	let mps, actions = ctx
	in
	let sigma_n, (top, bot), patch = Ph_cooperativity.build_cooperation
											~rsa:m_default_rsa 
											(get_sort_max mps) sm
	in
	let mps = List.fold_left (fun mps (a,i) -> SMap.add a i mps) mps (fst patch)
	and actions = actions @ (snd patch)
	in

	(* setup cooperativities *)
	let coop_hits ai j = List.map (fun s -> Hit ((sigma_n, s), ai, j))
	in
	let h'coop = coop_hits (sort,l_false) l_true top
				@ coop_hits (sort,l_true) l_false bot
	in
	mps, ph_add_hits actions m_default_rsa h'coop

)
| Arg_Name label::params ->
	macro_cooperativity ~coop_label:(Some label) autoremove params
| _ -> failwith "macro_cooperativity: wrong arguments"
;;

let macro_remove = function
  [Arg_Actions ractions] -> (fun (mps,actions) ->
  	(mps, List.filter (fun (Hit h, rsa) -> not (List.mem h ractions)) actions)
)
| _ -> failwith "macro_remove: wrong arguments"
;;

let macro_knockdown = function
[Arg_Process proc] -> (fun (mps, actions) ->
	(mps, List.filter (fun (Hit (ai,(b,j),j'),_) ->
		proc <> ai (*&& proc <> (b,j) && proc <> (b,j')*)) actions)
)
| _ -> failwith "macro_knockdown: wrong arguments"
;;

let precall_macro = function 
	  "BRN" -> macro_brn false
	| "COOPERATIVITY" -> macro_cooperativity true
	| "COOPERATIVITY_KEEP" -> macro_cooperativity false
	| "GRN" -> macro_brn true
	| "KNOCKDOWN" -> macro_knockdown
	| "REGULATION" -> macro_regulation
	| "RM" -> macro_remove
	| name -> failwith ("Unknown macro '"^name^"'")
;;

%}

%token <string> Name
%token <float> Float
%token <int> Int
%token New Art At Eof Initial_state Initial_context
%token Directive Option Sample Stoch_abs Absorb Default_rate Within 
%token AND OR NOT IN
%token ARROW INFTY
%token COMMA LBRACKET LCURLY LPAREN RBRACKET RCURLY RPAREN SEMI SHARP STAR

%token <char> Sign

%left IN
%left AND
%left OR
%left NOT

%start main
%type <Ph_types.ph * Ph_types.ctx> main

%start processlist
%type <Ph_types.process list> processlist

%start interaction_graph
%type <InteractionGraph.regulation_t list> interaction_graph

%%

content :
  content decl { merge_decl $1 $2 }
| content hit { merge_instr $1 $2 }
| content macro { $2 $1 }
| content update { update $1 $2 }
| decl		 { merge_decl (SMap.empty, []) $1 }
;
decl :
  New process	{ assert (snd $2 >= 0); $2 }
;
process :
  Name Int	{ ($1, $2) }
;
rate :
  INFTY { Instantaneous }
| Float { RateSA ($1, directive.default_sa) }
| Float Absorb Int { RateSA ($1, $3) }
| LBRACKET Float SEMI Float RBRACKET { FiringInterval ($2,$4,0.99) }
//| LBRACKET Float SEMI Float RBRACKET SHARP Float { FiringInterval ($2,$4,$7) }
;
hit : 
  process ARROW process Int At rate 					{ ($1, $3, $4, $6) }
| process ARROW process Int 		 				{ ($1, $3, $4, default_rsa ()) }
;
macro:
	Name LPAREN macro_args RPAREN { precall_macro $1 $3 }
;
macro_args:
	  macro_arg	{ [$1] }
	| macro_arg COMMA macro_args { $1::$3 }
;
macro_arg:
	  Name							{ Arg_Name $1 }
	| Int							{ Arg_Int $1 }
	| process						{ Arg_Process $1 }
	| action_list					{ Arg_Actions $1 }
	| regulation					{ Arg_Regulation $1 }
	| regulation_list				{ Arg_Regulations $1 }
	| state_list					{ Arg_AnoStates $1 }
	| name_list						{ Arg_Sorts $1 }
	| name_list ARROW process Int	{ Arg_NamesHit ($1,$3,$4,default_rsa ()) }
	| name_list ARROW process Int At rate	{ Arg_NamesHit ($1,$3,$4,$6) }
	| state_matchings				{ Arg_StateMatching $1 }
;

update: LCURLY actionmatch_list RCURLY At rate { ($2,$5) };
actionmatch_list:
	  actionmatch { [$1] }
	| actionmatch SEMI { [$1] }
	| actionmatch SEMI actionmatch_list { $1::$3 }
;
actionmatch:
	processmatch ARROW processmatch levelmatch { ($1,$3,$4) }
;
levelmatch: STAR { MatchAnyLevel } | Int { MatchOnlyLevel $1 };
sortmatch: STAR { MatchAnySort } | Name { MatchOnlySort $1 };
processmatch : sortmatch levelmatch { ($1,$2) };

action_list: LCURLY action_list_t RCURLY { $2 };
action_list_t:
	  action { [$1] }
	| action SEMI { [$1] }
	| action SEMI action_list_t { $1::$3 }
;
action:
	  process ARROW process Int { ($1,$3,$4) }
;
state_matchings:
	  state_matching	{ $1 }
	| NOT state_matchings { SM_Not $2 }
	| LPAREN state_matchings RPAREN { $2 }
	| state_matchings AND state_matchings { SM_And ($1, $3) }
	| state_matchings OR state_matchings { SM_Or ($1, $3) }
;
state_matching:
	name_list IN state_list { SM ($1, $3) }
;

regulation_spec :
	  Name Int ARROW Sign Name	{ ($1, $2, (if $4 = '+' then Positive else Negative), $5) }
;
regulation :
	  regulation_spec { let a,b,c,d = $1 in Regulation (a,b,c,d,default_rsa ()) }
	| regulation_spec At rate { let a,b,c,d = $1 in Regulation (a,b,c,d,$3) }
;
regulation_list: LBRACKET regulation_list_t RBRACKET { $2 }
regulation_list_t:
	  regulation	{ [$1] }
	| regulation SEMI	{ [$1] }
	| regulation SEMI regulation_list_t	{ $1::$3 }
;

name_list: LBRACKET name_list_t RBRACKET { $2 };
name_list_t:
	  Name { [$1] }
	| Name SEMI { [$1] }
	| Name SEMI name_list_t { $1::$3 }
;
state_list: LBRACKET state_list_t RBRACKET { $2 };
state_list_t:
	  state { [$1] }
	| state SEMI { [$1] }
	| state SEMI state_list_t { $1::$3 }
;
state:
	LBRACKET level_list RBRACKET { $2 }
;
level_list:
	  Int { [$1] }
	| Int SEMI { [$1] }
	| Int SEMI level_list { $1::$3 }
;

header :
  Directive Sample Float { directive.sample <- $3 }
| Directive Stoch_abs Int { assert ($3 > 0); directive.default_sa <- $3 }
| Directive Default_rate Float { assert ($3 >= 0.); directive.default_rate <- Some($3) }
| Directive Default_rate INFTY { directive.default_rate <- None }
| Option Sign Name { set_option $3 ($2 = '+') }
;
headers :
  header { }
| header headers{ }
;

processlist :
  process { $1::[] }
| process COMMA processlist { $1::$3 }
;
initstate :
  Initial_state processlist { procs_of_ps $2 }
;
initctx : 
  Initial_context processlist { procs_of_ps $2 }
;

footer :
  Eof { PSet.empty }
| initstate Eof { $1 }
| initctx Eof { $1 }
;

main :
  headers content footer { let ph = init_ph $2 in 
  			(init_content ph, compute_init_context ph $3)}
| content footer { let ph = init_ph $1 in
			(init_content ph, compute_init_context ph $2)}
;

interaction_graph : regulation_list_t Eof { $1 }
;
%%
