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
rule lexer = parse
  [' ' '\t' '\r']	{ lexer lexbuf }
| '\n' {line_incr lexbuf; lexer lexbuf}
| "process" { New }
| "directive" { Directive }
| "sample" { Sample }
| "stochasticity_absorption" { Stoch_abs }
| "->" { Hit }
| "@" { At }
| "~" { Absorb }
| digit+ as value { Int (int_of_string value) }
| ['A'-'z']+ as name { Name name }
| digit+ "." digit* as rate	{ Float (float_of_string rate) }
| eof { Eof }
