{
open Ph_parser;;

let line_incr lexbuf =
	let pos = Lexing.lexeme_end_p lexbuf in
	let pos = {pos with Lexing.pos_lnum = pos.Lexing.pos_lnum+1; Lexing.pos_bol = pos.Lexing.pos_cnum}
	in
	lexbuf.Lexing.lex_curr_p <- pos
;;

}

let digit = ['0'-'9']
let name = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*

rule lexer = parse
  [' ' '\t' '\r']	{ lexer lexbuf }
| '\n' {line_incr lexbuf; lexer lexbuf}
| "process" { New }
| "directive" { Directive }
| "sample" { Sample }
| "stochasticity_absorption" { Stoch_abs }
| "default_rate" { Default_rate }
| "and" { AND }
| "not" { NOT }
| "in" { IN }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LCURLY }
| "}" { RCURLY }
| "->" { ARROW }
| "@" { At }
| "#" { SHARP }
| "~" { Absorb }
| "," { COMMA }
| ";" { SEMI }
| "Inf" { INFTY }
| ['+' '-'] as sign	{ Sign sign }
| "initial_state" { Initial }
| digit+ as value { Int (int_of_string value) }
| name as n { Name n }
| digit+ "." digit* as rate	{ Float (float_of_string rate) }
| eof { Eof }
| "(*"	{ comment 1 lexbuf } 

and comment n = parse
 | "(*"  		{ comment (n+1) lexbuf } 
 | '\n' 		{ line_incr lexbuf; comment n lexbuf}
 | "*)" 		{ if (n-1) > 0 then comment (n-1) lexbuf else lexer lexbuf} 
 |  _ 			{ comment n lexbuf } 

