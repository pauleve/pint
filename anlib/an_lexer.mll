{
open An_parser
exception SyntaxError of string

let line_incr lexbuf =
	let pos = Lexing.lexeme_end_p lexbuf in
	let pos = {pos with Lexing.pos_lnum = pos.Lexing.pos_lnum+1; Lexing.pos_bol = pos.Lexing.pos_cnum}
	in
	lexbuf.Lexing.lex_curr_p <- pos
}

let digit = ['0'-'9']
let name = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*

rule lexer = parse
  [' ' '\t' '\r']	{ lexer lexbuf }
| '\n' {line_incr lexbuf; lexer lexbuf}
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "in" { IN }
| "when" { WHEN }
| "initial_state" { Initial_state }
| "initial_context" { Initial_context  }
| "=" { EQUAL }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LCURLY }
| "}" { RCURLY }
| "->" { ARROW }
| "@" { AT }
| "#" { SHARP }
| "*" { STAR }
| "," { COMMA }
| ";" { SEMI }
| "Inf" { INFTY }
| "\"" {p_string (Buffer.create 10) lexbuf}
| ['+' '-'] as sign	{ Sign sign }
| digit+ as value { Int (int_of_string value) }
| name as n { Name n }
| digit+ "." digit* as rate	{ Float (float_of_string rate) }
| eof { Eof }
| "(*"	{ comment 1 lexbuf }

and comment n = parse
 | "(*"  	{ comment (n+1) lexbuf }
 | '\n' 	{ line_incr lexbuf; comment n lexbuf}
 | "*)" 	{ if (n-1) > 0 then comment (n-1) lexbuf else lexer lexbuf}
 |  _ 		{ comment n lexbuf }

and p_string buf = parse
| "\\\""	{ Buffer.add_char buf '"'; p_string buf lexbuf}
| '"'		{ Label (Buffer.contents buf) }
| "\\\\"	{ Buffer.add_char buf '\\'; p_string buf lexbuf}
| "\""		{ lexer lexbuf }
| [^ '"' '\\' '\n' '\t' '\r']+
			{ Buffer.add_string buf (Lexing.lexeme lexbuf);
				p_string buf lexbuf }
| _ 		{ raise (SyntaxError
				("Illegal string character: '" ^ Lexing.lexeme lexbuf ^ "'"))}
| eof		{ raise (SyntaxError ("String is not terminated")) }

{
let is_name data =
	let lexing = Lexing.from_string data
	in
	try
		match lexer lexing with
		  Name _ ->
			(match lexer lexing with
				Eof -> true | _ -> false)
		| _ -> false
	with Parsing.Parse_error -> false
}
