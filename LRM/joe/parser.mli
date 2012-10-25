type token =
  | INT of (string)
  | ID of (string)
  | CMP of (string)
  | AND
  | OR
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
