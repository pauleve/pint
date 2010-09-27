%{
open Debug;;

open Ph_types;;

type t_directive = {
	mutable default_rate : float option;
	mutable default_sa : int;
	mutable sample : float
};;

let directive = {
	default_rate = None;
	default_sa = 1;
	sample = 1000.0
};;

let cooperativities = ref [];;

let default_rsa () = 
	match directive.default_rate with
	  None -> None
	| Some r -> Some (r, directive.default_sa)
;;

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
let merge_instr (ps,hits) (ai,bj,k,rsa_d) = 
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
	let rsa = 
		match rsa_d with
		   None -> default_rsa ()
		 | Some(rate_d) -> ( match rate_d with
		 	   None -> None
			 | Some(r, None) -> Some (r, directive.default_sa)
			 | Some(r, Some sa) -> Some (r,sa)
		)
	in
	Hashtbl.add hits bj ((ai, rsa),k);
	(ps,hits)
;;
let ph_add_hits (ps, hits) hits' =
	let iter = function
		Hit (ai, bj, j') -> Hashtbl.add hits bj ((ai, default_rsa ()), j')
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

let get_sort_max ps a =
	try
		List.assoc a ps
	with Not_found -> 
		failwith ("get_sort_max: unknown sort '"^a^"'")
;;


let compute_init_state ph defaults =
	let state = merge_state (state0 ph) defaults
	in
	(* apply cooperativities *)
	let fold state (c, (sigma, idx)) =
		let state_c = List.map (fun a -> SMap.find a state) sigma
		in
		let i = idx state_c
		in
		dbg ("- init cooperativity: "^string_of_process (c,i));
		SMap.add c i state
	in
	(*TODO: handle nested cooperativities *)
	let state = List.fold_left fold state (List.rev !cooperativities)
	in
	(* re-apply default (force cooperative states) *)
	merge_state state defaults
;;

(***
	MACROS
***)

type anostate = int list

type state_matching_t =
	  SM of (string list * anostate list)
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
	| Arg_NamesHit of string list * process * int
	| Arg_StateMatching of state_matching_t
;;

let reflection_name sigma = String.concat "" sigma
;;
let build_reflection (ps,hits) sigma =
	let sigma_n = reflection_name sigma
	in
	if List.mem_assoc sigma_n ps then
		((sigma_n, List.assoc sigma_n !cooperativities), (ps,hits))
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
		and hits = ph_add_hits (ps,hits) hsigma
		in
		let record = sigma_n, (sigma, idx_from_state)
		in
		cooperativities := record::!cooperativities;
		(record, (ps,hits))
	)
;;


let macro_binary_cooperativity = function
[Arg_StateMatching sm; Arg_Name sort; Arg_Int l_true; Arg_Int l_false] -> (fun ctx ->
	  	failwith "TO IMPLEMENT";
)
| _ -> failwith "macro_binary_cooperativity: wrong arguments"
;;

let macro_regulation = function
	  [Arg_Regulation regulation] -> (fun (ps,hits) ->
	  	match regulation with
	Regulation (a,t,s,b) ->
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
		ps, ph_add_hits (ps,hits) (List.flatten (List.map apply_regulation (Util.range 0 la)))
	)
	| _ -> failwith "macro_regulation: wrong arguments"
;;
let macro_grn = function
	  [Arg_Regulations regulations] -> (fun ctx ->

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
		macro_regulation [Arg_Regulation regulation] ctx
	in
	List.fold_left folder ctx regulations

	)
	| _ -> failwith "macro_grn: wrong arguments"
;;

let macro_cooperativity = function
[Arg_NamesHit (sigma, ak, k'); Arg_AnoStates top] -> (fun (ps,hits) ->
	let (sigma_n, (sigma, idx_from_state)), (ps,hits) = build_reflection (ps,hits) sigma
	in
	let hits = filter_hits (ps,hits) (fun h ->
		match h with Hit ((a,i),bj,j') -> not (List.mem a sigma && bj = ak && j' = k'))

	and h'coop = List.map (fun state ->
		Hit ((sigma_n, idx_from_state state), ak, k'))
			top
	in
	ps, ph_add_hits (ps,hits) h'coop
)
| _ -> failwith "macro_cooperativity: wrong arguments"
;;

let macro_remove = function
  [Arg_Actions actions] -> (fun (ps,hits) ->
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
)
| _ -> failwith "macro_remove: wrong arguments"
;;

let macro_knockdown = function
[Arg_Process proc] -> (fun (ps,hits) ->
	let hits' = Hashtbl.create (List.length ps)
	in
	let knockdown target ((hitter,p),bounce_idx) =
		let bounce = fst target, bounce_idx
		in
		if not (hitter = proc || target = proc || bounce = proc) then
			Hashtbl.add hits' target ((hitter,p),bounce_idx)
	in
	Hashtbl.iter knockdown hits;
	ps, hits' 
)
| _ -> failwith "macro_knockdown: wrong arguments"
;;

let precall_macro = function 
	  "BINARY_COOPERATIVITY" -> macro_binary_cooperativity
	| "COOPERATIVITY" -> macro_cooperativity
	| "GRN" -> macro_grn
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
%token Directive Sample Stoch_abs Absorb Default_rate
%token AND IN
%token ARROW INFTY
%token COMMA LBRACKET LCURLY LPAREN RBRACKET RCURLY RPAREN SEMI

%token <char> Sign

%left IN
%left AND

%start main
%type <Ph_types.ph * Ph_types.sortidx Ph_types.SMap.t> main

%%

content :
  content decl { merge_decl $1 $2 }
| content instr { merge_instr $1 $2 }
| content macro { $2 $1 }
| decl		 { merge_decl ([], Hashtbl.create 0) $1 }
;
decl :
  New process	{ assert (snd $2 >= 0); $2 }
;
process :
  Name Int	{ ($1, $2) }
;
rate :
  At INFTY { None }
| At Float { Some ($2,None) }
| At Float Absorb Int { Some ($2,Some $4) }
;
instr : 
  process ARROW process Int 		 				{ ($1, $3, $4, None) }
| process ARROW process Int rate 					{ ($1, $3, $4, Some($5)) }
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
	| name_list ARROW process Int	{ Arg_NamesHit ($1,$3,$4) }
	| state_matchings				{ Arg_StateMatching $1 }
;

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
	| state_matching AND state_matchings { SM_And ($1, $3) }
;
state_matching:
	name_list IN state_list { SM ($1, $3) }
;

regulation:
	  Name Int ARROW Sign Name	{ Regulation($1, $2, (if $4 = '+' then Positive else Negative), $5) }
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
  Directive headers content footer { ($3, compute_init_state $3 $4) }
| content footer { ($1, compute_init_state $1 $2) }
;
%%
