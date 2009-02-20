%{
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
%}

%token <string> Name
%token <float> Float
%token <int> Int
%token New Art Hit At Eof
%token Directive Sample Stoch_abs Absorb

%start main
%type <(string * string) list * Ph_types.ph> main

%%
process :
  Name Int	{ ($1, $2) }
;
decl :
  New process	{ assert (snd $2 > 0); $2 }
;
instr : 
  process Hit process Int						{ ($1, $3, $4, Ph_types.RateInf, None) }
| process Hit process Int At Float 				{ ($1, $3, $4, Ph_types.Rate $6, None) }
| process Hit process Int At Float Absorb Int 	{ ($1, $3, $4, Ph_types.Rate $6, Some $8) }
;
content :
  content decl { merge_decl $1 $2 }
| content instr { merge_instr $1 $2 }
| decl		 { merge_decl ([], Hashtbl.create 0) $1 }
;

header :
  Sample Float { ("sample",string_of_float $2) }
| Stoch_abs Int { assert ($2 > 0); ("stochasticity_absorption",string_of_int $2) }
;

headers :
  header { $1::[] }
| header Directive headers { $1::$3 }
;

main :
  Directive headers main2 { ($2,$3) }
| main2 { ([],$1) }
;
main2 :
  content Eof { $1 }
;
%%
