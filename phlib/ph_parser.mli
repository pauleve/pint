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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * string) list * Ph_types.ph
