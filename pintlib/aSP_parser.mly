
%token <string> Word
%token Eof
%token LPAREN RPAREN COMMA

%start solution
%type <(string * string list) list> solution

%%
solution:
  answer Eof { [$1] }
| answer solution {$1::$2}
;
answer:
  Word { ($1,[]) }
| Word LPAREN arguments RPAREN { ($1,$3) }
;
arguments:
  Word { $1::[] }
| Word COMMA arguments { $1::$3 }
;

%%
