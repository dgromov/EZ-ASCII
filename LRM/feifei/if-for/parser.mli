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
  | LPAREN
  | RPAREN
  | IF
  | ELSE
  | ELSEIF
  | SEMICOLON
  | LCBRA
  | RCBRA
  | LARROW
  | SEPERATOR
  | FOR
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
