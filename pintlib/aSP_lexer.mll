{
open ASP_parser
exception SyntaxError of string
}

let word = ['A'-'Z' 'a'-'z' '_' '0'-'9']*

rule lexer = parse
  [' ' '\t' '\r']	{ lexer lexbuf }
| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }
| "\"" {p_string (Buffer.create 10) lexbuf}
| word as w { Word w }
| eof { Eof }

and p_string buf = parse
| "\\\""	{ Buffer.add_char buf '"'; p_string buf lexbuf}
| '"'		{ Word (Buffer.contents buf) }
| "\\\\"	{ Buffer.add_char buf '\\'; p_string buf lexbuf}
| "\""		{ lexer lexbuf }
| [^ '"' '\\' '\n' '\t' '\r']+
			{ Buffer.add_string buf (Lexing.lexeme lexbuf);
				p_string buf lexbuf }
| _ 		{ raise (SyntaxError
				("Illegal string character: '" ^ Lexing.lexeme lexbuf ^ "'"))}
| eof		{ raise (SyntaxError ("String is not terminated")) }

