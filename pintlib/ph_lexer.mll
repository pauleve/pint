{
open Ph_parser;;
}
let digit = ['0'-'9']
rule lexer = parse
  [' ' '\t' '\r' '\n']	{ lexer lexbuf }
| "metaprocess" { New }
| "directive" { Directive }
| "sample" { Sample }
| "stochasticity_absorption" { StochAbs }
| "->" { Hit }
| "@" { At }
| digit+ as level { Level (int_of_string level) }
| ['A'-'z']+ as name { Name name }
| digit+ "." digit* as rate	{ Float (float_of_string rate) }
| ['1'-'9'] digit* as value { PosInt (int_of_string value) }
| eof { Eof }
