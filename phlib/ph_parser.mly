%{
open Debug;;

open Ph_types;;

type regulation_sign = Positive | Negative
type regulation_t = Regulation of (string * int * regulation_sign * string * stochatime)

let cooperativities = ref [];;
let __coop_counter = ref 0;;

let default_rsa () = 
	match directive.default_rate with
	  None -> Instantaneous
	| Some r -> RateSA (r, directive.default_sa)
;;

let init_content (ps, actions) =
	let hits = Hashtbl.create 200
	in
	let register_action = function Hit (ai,bj,j'), rsa ->
		Hashtbl.add hits bj ((ai,rsa),j')
	in
	List.iter register_action actions;
	(ps, hits)
;;

let merge_decl (ps, actions) p =
	let merge_metaproc ps (p,l) =
		try
			let ol = List.assoc p ps
			in
			if ol > l then
				failwith "Cannot decrease metaprocess levels"
			else (p,l)::List.remove_assoc p ps
		with Not_found -> (p,l)::ps
	in
	merge_metaproc ps p, actions
;;
let merge_instr (ps, actions) (ai,bj,k,stoch) = 
	let assert_p_exists (name,level) =
		let errstr = "Invalid reference to process "^name^(string_of_int level)^": "
		in
		try
			let ml = List.assoc name ps
			in
			if level < 0 || ml < level then 
				failwith (errstr^"level out of bound (max is "^(string_of_int ml)^")")
		with Not_found -> failwith (errstr^"undefined metaprocess")
	in
	assert_p_exists ai;
	assert_p_exists bj;
	assert_p_exists (fst bj, k);
	(ps, (Hit (ai,bj,k),stoch)::actions)
;;
let ph_add_hits (ps, actions) actions' stoch =
	actions @ (List.map (fun a -> (a, stoch)) actions')
;;

let get_sort_max ps a =
	try
		List.assoc a ps
	with Not_found -> 
		failwith ("get_sort_max: unknown sort '"^a^"'")
;;


let compute_init_state ph defaults =
	let state = merge_state_with_ps (state0 (fst ph)) defaults
	in
	let state = merge_state_with_ps state !Ph_useropts.initial_procs
	in
	(* apply cooperativities *)
	let fold state (c, (sigma, idx)) =
		let state_c = List.map (fun a -> SMap.find a state) sigma
		in
		let i = idx state_c
		in
		dbg ("- init cooperativity: "^string_of_proc (c,i));
		SMap.add c i state
	in
	(*TODO: handle nested cooperativities *)
	let state = List.fold_left fold state (List.rev !cooperativities)
	in
	(* re-apply default (force cooperative states) *)
	merge_state_with_ps state defaults
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

let update (ps,actions) (regexps, rate) =
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
	ps, List.map update_action actions
;;

(***
	MACROS
***)

type anostate = int list

type state_matching_t =
	  SM of (string list * anostate list)
	| SM_Not of state_matching_t
	| SM_And of (state_matching_t * state_matching_t)
;;

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

let reflection_name sigma = String.concat "" sigma
;;
let build_reflection (ps,actions) = function
  [a] -> (a, ([a], (function [s] -> s | _ -> invalid_arg "idx_from_state singleton"))), (ps,actions)
| sigma -> 
	let sigma_n = reflection_name sigma
	in
	if List.mem_assoc sigma_n ps then
		((sigma_n, List.assoc sigma_n !cooperativities), (ps,actions))
	else (
		let sigma_len = List.length sigma
		in
		let rec build_idx_sizes n prec_size =
			let my_size = if n = sigma_len-1 then 1 else ((1+get_sort_max ps (List.nth sigma (n+1))) * prec_size)
			and n' = n-1
			in
			(if n' < -1 then [] else build_idx_sizes (n-1) my_size)
			@ [my_size]
		in
		let idx_sizes = build_idx_sizes (sigma_len-1) 1
		in
		let lsigma = List.hd idx_sizes - 1
		and idx_sizes = List.tl idx_sizes
		in
		let sigma_p = (sigma_n, lsigma)
		in
		let idx_from_state state =
			let rec idx_from_state n = function
				  [] -> 0
				| i::tail -> i*(List.nth idx_sizes n) + idx_from_state (n+1) tail
			in
			idx_from_state 0 state
		in

		let get_sort_processes a = Util.range 0 (get_sort_max ps a)
		in
		let _S = Util.cross_list (List.map get_sort_processes (List.rev sigma))
		in

		let folder hsigma z =
			let n = Util.index_of z sigma
			in
			let folder hsigma i =
				let my_S = List.filter (fun state -> List.nth state n <> i) _S
				in
				let make_hit state =
					let shift = (i - List.nth state n)*(List.nth idx_sizes n)
					and state_id = idx_from_state state
					in
					let state'_id = state_id + shift
					in
					Hit ((z,i), (sigma_n,state_id), state'_id)
				in
				hsigma @ List.map make_hit my_S
			in
			List.fold_left folder hsigma (Util.range 0 (List.assoc z ps))
		in
		let hsigma = List.fold_left folder [] sigma
		in
		let ps = sigma_p::ps
		and actions = ph_add_hits (ps,actions) hsigma (default_rsa ())
		in
		let record = sigma_n, (sigma, idx_from_state)
		in
		cooperativities := record::!cooperativities;
		(record, (ps,actions))
	)
;;

let macro_regulation = function
	  [Arg_Regulation regulation] -> (fun (ps,actions) ->
	  	match regulation with
	Regulation (a,t,s,b,stoch) ->
		let la = get_sort_max ps a and lb = get_sort_max ps b
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
		ps, ph_add_hits (ps,actions) (List.flatten (List.map apply_regulation (Util.range 0 la))) stoch
	)
	| _ -> failwith "macro_regulation: wrong arguments"
;;
let macro_brn do_unregulated = function
[Arg_Regulations regulations] -> (fun ctx ->

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

let filter_hits (ps, actions) pred =
	(ps, List.filter (fun (a,rsa) -> pred a) actions)
;;

let macro_cooperativity autoremove = function

  (* COOPERATIVITY([a;b]): creates a cooperative sort between a and b *)
  [Arg_Sorts sorts] -> (fun (ps,actions) -> snd (build_reflection (ps,actions) sorts))

| [Arg_NamesHit (sigma, ak, k', stoch); Arg_AnoStates top] -> (fun (ps,actions) ->
	let (sigma_n, (sigma, idx_from_state)), (ps,actions) = build_reflection (ps,actions) sigma
	in
	let (ps, actions) = if not autoremove then (ps, actions) else
		filter_hits (ps,actions) (function Hit ((a,i),bj,j') -> 
						not (List.mem a sigma && bj = ak && j' = k'))
	in
	let h'coop = List.map (fun state ->
		Hit ((sigma_n, idx_from_state state), ak, k'))
			top
	in
	ps, ph_add_hits (ps,actions) h'coop stoch
)

| [Arg_StateMatching sm; Arg_Name sort; Arg_Int l_true; Arg_Int l_false] -> (fun ctx ->
	
	(* remove existing hits from sorts in sm to processes (sort,l_true|l_false) *)
	let rec matching_names = function
		  SM (sigma, _) -> sigma
		| SM_Not sm -> matching_names sm
		| SM_And (sm1, sm2) -> matching_names sm1 @ matching_names sm2
	in
	let names = matching_names sm
	in
	let ctx = filter_hits ctx (function Hit((b,k), (a,i), j) ->
		not (List.mem b names && a = sort &&
			(i = l_true && j = l_false || i = l_false && j = l_true)))
	in
	let separate_levels (ps, actions) sigma_n idx_from_state top =
		let idx_top = List.map idx_from_state top
		in
		let n = List.assoc sigma_n ps
		in
		let idx_bot = List.filter (fun i -> not (List.mem i idx_top)) (Util.range 0 n)
		in
		idx_top, idx_bot
	in

	(* build reflections *)
	let rec cooperative_matching ctx = function
		  SM (sigma, top) -> 
		  	let (sigma_n, (sigma, idx_from_state)), ctx = build_reflection ctx sigma
			in
			sigma_n, separate_levels ctx sigma_n idx_from_state top, ctx

		| SM_Not sm ->
			let sigma_n, (top,bot), ctx = cooperative_matching ctx sm
			in
			sigma_n, (bot, top), ctx

		| SM_And (sm1, sm2) ->
			let sig1, (top1,bot1), ctx = cooperative_matching ctx sm1
			in
			let sig2, (top2,bot2), ctx = cooperative_matching ctx sm2
			in
			let sigma_n = "__coop" ^ (string_of_int !__coop_counter)
			in __coop_counter := !__coop_counter + 1;

			let h_coop = List.flatten (
				  List.map (fun s -> [Hit ((sig1,s), (sigma_n,0), 2);
									Hit ((sig1,s), (sigma_n,1), 3)]) top1
				@ List.map (fun s -> [Hit ((sig1,s), (sigma_n,2), 0);
									Hit ((sig1,s), (sigma_n,3), 1)]) bot1
				@ List.map (fun s -> [Hit ((sig2,s), (sigma_n,0), 1);
									Hit ((sig2,s), (sigma_n,2), 3)]) top2
				@ List.map (fun s -> [Hit ((sig2,s), (sigma_n,1), 0);
									Hit ((sig2,s), (sigma_n,3), 2)]) bot2
			) in
			let ctx = (sigma_n,3)::(fst ctx), ph_add_hits ctx h_coop (default_rsa ())
			in
			cooperativities := (sigma_n, ([sig1;sig2], 
				function [s1;s2] ->
					let r1 = if List.mem s1 top1 then 2 else 0
					and r2 = if List.mem s2 top2 then 1 else 0
					in
					r1 + r2
				| _ -> invalid_arg "__coop idx_from_state"))::!cooperativities;
			sigma_n, ([3], [0;1;2]), ctx
	in
	let sigma_n, (top, bot), ctx = cooperative_matching ctx sm
	in

	(* setup cooperativities *)
	let coop_hits ai j = List.map (fun s -> Hit ((sigma_n, s), ai, j))
	in
	let h'coop = coop_hits (sort,l_false) l_true top
				@ coop_hits (sort,l_true) l_false bot
	in
	let ps, actions = ctx
	in
	ps, ph_add_hits (ps,actions) h'coop (default_rsa ())
)
| _ -> failwith "macro_cooperativity: wrong arguments"
;;

let macro_remove = function
  [Arg_Actions ractions] -> (fun (ps,actions) ->
  	(ps, List.filter (fun (Hit h, rsa) -> not (List.mem h ractions)) actions)
)
| _ -> failwith "macro_remove: wrong arguments"
;;

let macro_knockdown = function
[Arg_Process proc] -> (fun (ps, actions) ->
	(ps, List.filter (fun (Hit (ai,(b,j),j'),_) ->
		proc <> ai && proc <> (b,j) && proc <> (b,j')) actions)
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
%token New Art At Eof Initial
%token Directive Sample Stoch_abs Absorb Default_rate Within 
%token AND NOT IN
%token ARROW INFTY
%token COMMA LBRACKET LCURLY LPAREN RBRACKET RCURLY RPAREN SEMI SHARP STAR

%token <char> Sign

%left IN
%left AND
%left NOT

/* test */
%start main
%type <Ph_types.ph * Ph_types.state> main

%start processlist
%type <Ph_types.process list> processlist

%%

content :
  content decl { merge_decl $1 $2 }
| content hit { merge_instr $1 $2 }
| content macro { $2 $1 }
| content update { update $1 $2 }
| decl		 { merge_decl ([], []) $1 }
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
  process ARROW process Int 		 				{ ($1, $3, $4, default_rsa ()) }
| process ARROW process Int At rate 					{ ($1, $3, $4, $6) }
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
  Sample Float { directive.sample <- $2 }
| Stoch_abs Int { assert ($2 > 0); directive.default_sa <- $2 }
| Default_rate Float { assert ($2 >= 0.); directive.default_rate <- Some($2) }
| Default_rate INFTY { directive.default_rate <- None }
;

headers :
  header { $1::[] }
| header Directive headers { $1::$3 }
;

processlist :
  process { $1::[] }
| process COMMA processlist { $1::$3 }
;
initstate :
  Initial processlist { $2 }
;

footer :
  Eof { [] }
| initstate Eof { $1 }
;

main :
  Directive headers content footer { (init_content $3, compute_init_state $3 $4) }
| content footer { (init_content $1, compute_init_state $1 $2) }
;
%%
