type token =
  | Name of (string)
  | Float of (float)
  | Int of (int)
  | New
  | Art
  | Hit
  | At
  | Eof
  | Directive
  | Sample
  | Stoch_abs

open Parsing;;
# 2 "phlib/ph_parser.mly"
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
let merge_instr (ps,hits) (p1,p2,l,r) = 
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
	Hashtbl.add hits p2 ((p1, r),l);
	(ps,hits)
;;
# 46 "phlib/ph_parser.ml"
let yytransl_const = [|
  260 (* New *);
  261 (* Art *);
  262 (* Hit *);
  263 (* At *);
  264 (* Eof *);
  265 (* Directive *);
  266 (* Sample *);
  267 (* Stoch_abs *);
    0|]

let yytransl_block = [|
  257 (* Name *);
  258 (* Float *);
  259 (* Int *);
    0|]

let yylhs = "\255\255\
\002\000\003\000\004\000\004\000\005\000\005\000\005\000\006\000\
\006\000\007\000\007\000\001\000\001\000\008\000\000\000"

let yylen = "\002\000\
\002\000\002\000\004\000\006\000\002\000\002\000\001\000\002\000\
\002\000\001\000\003\000\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\015\000\007\000\000\000\013\000\
\000\000\002\000\000\000\000\000\000\000\000\000\014\000\000\000\
\005\000\006\000\001\000\008\000\009\000\000\000\012\000\000\000\
\011\000\000\000\000\000\000\000\004\000"

let yydgoto = "\002\000\
\005\000\010\000\006\000\018\000\007\000\013\000\014\000\008\000"

let yysindex = "\002\000\
\002\255\000\000\006\255\003\255\000\000\000\000\000\255\000\000\
\007\255\000\000\010\255\012\255\009\255\015\255\000\000\014\255\
\000\000\000\000\000\000\000\000\000\000\003\255\000\000\006\255\
\000\000\013\255\016\255\019\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\017\000\000\000\000\000\000\000\003\000\012\000"

let yytablesize = 26
let yytable = "\016\000\
\009\000\003\000\001\000\003\000\003\000\003\000\009\000\015\000\
\003\000\019\000\004\000\020\000\011\000\012\000\021\000\027\000\
\026\000\022\000\003\000\024\000\029\000\010\000\028\000\017\000\
\025\000\023\000"

let yycheck = "\007\000\
\001\001\001\001\001\000\004\001\004\001\004\001\001\001\008\001\
\008\001\003\001\009\001\002\001\010\001\011\001\003\001\003\001\
\024\000\009\001\004\001\006\001\002\001\004\001\007\001\007\000\
\022\000\014\000"

let yynames_const = "\
  New\000\
  Art\000\
  Hit\000\
  At\000\
  Eof\000\
  Directive\000\
  Sample\000\
  Stoch_abs\000\
  "

let yynames_block = "\
  Name\000\
  Float\000\
  Int\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "phlib/ph_parser.mly"
           ( (_1, _2) )
# 134 "phlib/ph_parser.ml"
               : 'process))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'process) in
    Obj.repr(
# 47 "phlib/ph_parser.mly"
              ( assert (snd _2 > 0); _2 )
# 141 "phlib/ph_parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'process) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'process) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "phlib/ph_parser.mly"
                            ( (_1, _3, _4, Ph_types.RateInf) )
# 150 "phlib/ph_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'process) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'process) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 51 "phlib/ph_parser.mly"
                                   ( (_1, _3, _4, Ph_types.Rate _6) )
# 160 "phlib/ph_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'content) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 54 "phlib/ph_parser.mly"
               ( merge_decl _1 _2 )
# 168 "phlib/ph_parser.ml"
               : 'content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'content) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 55 "phlib/ph_parser.mly"
                ( merge_instr _1 _2 )
# 176 "phlib/ph_parser.ml"
               : 'content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 56 "phlib/ph_parser.mly"
         ( merge_decl ([], Hashtbl.create 0) _1 )
# 183 "phlib/ph_parser.ml"
               : 'content))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 60 "phlib/ph_parser.mly"
               ( ("sample",string_of_float _2) )
# 190 "phlib/ph_parser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "phlib/ph_parser.mly"
                ( assert (_2 > 0); ("stochasticity_absorption",string_of_int _2) )
# 197 "phlib/ph_parser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'header) in
    Obj.repr(
# 65 "phlib/ph_parser.mly"
         ( _1::[] )
# 204 "phlib/ph_parser.ml"
               : 'headers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'header) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'headers) in
    Obj.repr(
# 66 "phlib/ph_parser.mly"
                           ( _1::_3 )
# 212 "phlib/ph_parser.ml"
               : 'headers))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'headers) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'main2) in
    Obj.repr(
# 70 "phlib/ph_parser.mly"
                          ( (_2,_3) )
# 220 "phlib/ph_parser.ml"
               : (string * string) list * Ph_types.ph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'main2) in
    Obj.repr(
# 71 "phlib/ph_parser.mly"
        ( ([],_1) )
# 227 "phlib/ph_parser.ml"
               : (string * string) list * Ph_types.ph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'content) in
    Obj.repr(
# 74 "phlib/ph_parser.mly"
              ( _1 )
# 234 "phlib/ph_parser.ml"
               : 'main2))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * string) list * Ph_types.ph)
;;
