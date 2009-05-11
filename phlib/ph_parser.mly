%{
open Ph_types;;
let merge_decl (ps,hits) p =
	let merge_metaproc ps (p,l) =
		try
			let ol = List.assoc p ps
			in
			if ol > l then
				failwith "Cannot decrease metaprocess levels"
			else (p,l)::List.remove_assoc p ps
		with Not_found -> (p,l)::ps
	in
	merge_metaproc ps p, hits
;;
let merge_instr (ps,hits) (p1,p2,l,r,sa) = 
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
	assert_p_exists p1;
	assert_p_exists p2;
	assert_p_exists (fst p2, l);
	Hashtbl.add hits p2 ((p1, (r,sa)),l);
	(ps,hits)
;;
let ph_add_hits (ps, hits) hits' =
	let iter = function
		Hit (ai, bj, j') -> Hashtbl.add hits bj ((ai, (1.,None)), j')
	in
	List.iter iter hits';
	hits
;;
let filter_hits (ps, hits) pred =
	let hits' = Hashtbl.create 0
	in
	let iter bj ((ai,param),j') =
		if pred (Hit (ai,bj,j')) then
			Hashtbl.add hits' bj ((ai,param),j')
	in
	Hashtbl.iter iter hits;
	hits'
;;

let macro_regulation regulation (ps,hits) = match regulation with
	Regulation (a,t,s,b) ->
		let la = List.assoc a ps and lb = List.assoc b ps
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
		ps, ph_add_hits (ps,hits) (List.flatten (List.map apply_regulation (Util.range 0 la)))
;;
let macro_grn regulations ctx =
	let folder (regulations, genes, regulated) = function
		Regulation (a,t,s,b) ->
			let n_regulated = SSet.singleton b
			in
			let n_genes = SSet.add a n_regulated
			in
			Regulation (a,t,s,b)::regulations,
			SSet.union genes n_genes,
			SSet.union regulated n_regulated
	in
	let init = ([], SSet.empty, SSet.empty)
	in
	let regulations, genes, regulated = List.fold_left folder init regulations
	in
	let unregulateds = SSet.diff genes regulated
	in
	let folder a regulations =
		Regulation (a,0,Negative,a)::regulations
	in
	let regulations = SSet.fold folder unregulateds regulations
	in
	let folder ctx regulation =
		macro_regulation regulation ctx
	in
	List.fold_left folder ctx regulations
;;

let macro_cooperativity sigma ak k' top (ps,hits) =

	let sigma_len = List.length sigma
	in
	let rec build_idx_sizes n prec_size =
		let my_size = if n = sigma_len-1 then 1 else ((1+List.assoc (List.nth sigma (n+1)) ps) * prec_size)
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
	let sigma_n = String.concat "" sigma
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

	let _S = Util.cross_list (List.map (fun a -> Util.range 0 (List.assoc a ps)) (List.rev sigma))
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

	and hits = filter_hits (ps,hits) (fun h ->
		match h with Hit ((a,i),bj,j') -> not (List.mem a sigma && bj = ak && j' = k'))

	and h'coop = List.map (fun state ->
		Hit ((sigma_n, idx_from_state state), ak, k'))
			top
	in
	let is_new = not (List.mem sigma_p ps)
	in
	let ps = if is_new then sigma_p::ps else ps
	in
	ps, ph_add_hits (ps,hits) (if is_new then h'coop@hsigma else h'coop)
;;

let macro_remove actions (ps,hits) =
	let remove_action (ai,bj,j') =
		let restore_stack stack =
			List.iter (fun elt -> Hashtbl.add hits bj elt) stack
		in
		let rec remove_from_stack stack =
			try 
				let (ai',p),j'' = Hashtbl.find hits bj
				in
				Hashtbl.remove hits bj;
				if ai' = ai && j'' = j' then
					restore_stack stack
				else
					remove_from_stack (((ai',p),j'')::stack)
			with Not_found -> restore_stack stack
		in
		remove_from_stack []
	in
	List.iter remove_action actions;
	ps, hits
;;

let precall_macro_action_list name = match name with
	  "RM" -> macro_remove
	| _ -> failwith ("Unkown macro '"^name^"'")
;;
let precall_macro_regulation name = match name with
	  "REGULATION" -> macro_regulation 
	| _ -> failwith ("Unkown macro '"^name^"'")
;;
let precall_macro_regulation_list name = match name with
	  "GRN" -> macro_grn
	| _ -> failwith ("Unkown macro '"^name^"'")
;;
let precall_macro_cooperativity name = match name with
	  "COOPERATIVITY" -> macro_cooperativity 
	| _ -> failwith ("Unkown macro '"^name^"'")
;;

%}

%token <string> Name
%token <float> Float
%token <int> Int
%token New Art At Eof Initial
%token Directive Sample Stoch_abs Absorb
%token ARROW
%token COMMA LBRACKET LCURLY LPAREN RBRACKET RCURLY RPAREN SEMI

%token <char> Sign

%start main
%type <(string * string) list * Ph_types.ph * (string * int) list> main

%%

content :
  content decl { merge_decl $1 $2 }
| content instr { merge_instr $1 $2 }
| content macro { $2 $1 }
| decl		 { merge_decl ([], Hashtbl.create 0) $1 }
;
decl :
  New process	{ assert (snd $2 > 0); $2 }
;
process :
  Name Int	{ ($1, $2) }
;
instr : 
  process ARROW process Int At Float 				{ ($1, $3, $4, $6, None) }
| process ARROW process Int At Float Absorb Int 	{ ($1, $3, $4, $6, Some $8) }
;
macro:
	  Name LPAREN LCURLY action_list RCURLY RPAREN { precall_macro_action_list $1 $4 }
	| Name LPAREN regulation RPAREN	{ precall_macro_regulation $1 $3 }
	| Name LPAREN LBRACKET regulation_list RBRACKET RPAREN { precall_macro_regulation_list $1 $4 }
	| Name LPAREN LBRACKET name_list RBRACKET ARROW process Int COMMA
				LBRACKET state_list RBRACKET RPAREN { precall_macro_cooperativity $1 $4 $7 $8 $11 }
;
action_list:
	  action { [$1] }
	| action SEMI { [$1] }
	| action SEMI action_list { $1::$3 }
;
action:
	  process ARROW process Int { ($1,$3,$4) }
;

regulation:
	  Name Int ARROW Sign Name	{ Regulation($1, $2, (if $4 = '+' then Positive else Negative), $5) }
;
regulation_list:
	  regulation	{ [$1] }
	| regulation SEMI	{ [$1] }
	| regulation SEMI regulation_list	{ $1::$3 }
;
name_list:
	  Name { [$1] }
	| Name SEMI { [$1] }
	| Name SEMI name_list { $1::$3 }
;
state_list:
	  state { [$1] }
	| state SEMI { [$1] }
	| state SEMI state_list { $1::$3 }
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
  Sample Float { ("sample",string_of_float $2) }
| Stoch_abs Int { assert ($2 > 0); ("stochasticity_absorption",string_of_int $2) }
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
  Directive headers content footer { ($2,$3,$4) }
| content footer { ([],$1,$2) }
;
%%
