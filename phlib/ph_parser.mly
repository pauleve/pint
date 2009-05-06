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
	(ps,hits)
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
			List.map make_hit
				(Util.range (if r = Positive then 0 else 1)
							(if r = Negative then lb else (lb-1)))

		in
		List.flatten (List.map apply_regulation (Util.range 0 la))
;;
let macro_grn ctx regulations =
	let folder (hits, genes, regulated) = function
		Regulation (a,t,s,b) ->
			let n_regulated = SSet.singleton b
			in
			let n_genes = SSet.add a n_regulated
			in
			hits @ macro_regulation (Regulation (a,t,s,b)) ctx,
			SSet.union genes n_genes,
			SSet.union regulated n_regulated
	in
	let init = ([], SSet.empty, SSet.empty)
	in
	let hits, genes, regulated = List.fold_left folder init regulations
	in
	let unregulateds = SSet.diff genes regulated
	in
	let folder a hits =
		hits @ macro_regulation (Regulation (a,0,Negative,a)) ctx
	in
	SSet.fold folder unregulateds hits
;;


let precall_macro_regulation name regulation = match name with
	  "REGULATION" -> macro_regulation regulation
	| _ -> failwith ("Unkown macro '"^name^"'")
;;

%}

%token <string> Name
%token <float> Float
%token <int> Int
%token New Art At Eof Initial
%token Directive Sample Stoch_abs Absorb
%token ARROW
%token COMMA LBRACKET LPAREN RBRACKET RPAREN SEMI

%token <char> Sign

%start main
%type <(string * string) list * Ph_types.ph * (string * int) list> main

%%

content :
  content decl { merge_decl $1 $2 }
| content instr { merge_instr $1 $2 }
| content macro { let hits = $2 $1 in ph_add_hits $1 hits }
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
	  Name LPAREN regulation_def RPAREN	{ precall_macro_regulation $1 $3 }
;

regulation_def :
	  Name Int ARROW Sign Name	{ Regulation($1, $2, (if $4 = '+' then Positive else Negative), $5) }
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
