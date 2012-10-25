type token =
  | INT of (string)
  | ID of (string)
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
