{
open Ph_parser;;
}
let digit = ['0'-'9']
rule lexer = parse
  [' ' '\t']	{ lexer lexbuf }
| ['\n' ';']	{ Sep }
| "artificial"  { Art }
| "metaprocess" { New }
| "->" { Hit }
| "@" { At }
| digit+ as level { Level (int_of_string level) }
| ['A'-'z']+ as name { Name name }
| digit+ "." digit* as rate	{ Rate (float_of_string rate) }
| eof { Eof }
