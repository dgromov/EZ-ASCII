type token =
  | INT of (int)
  | ID of (string)
  | CMP of (string)
  | STR of (string)
  | TRUE
  | FALSE
  | AND
  | OR
  | COMMA
  | SEMICOLON
  | COLON
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | EOL
  | LT
  | GT
  | EQ
  | LEQ
  | GEQ
  | NEQ
  | NEGATE
  | ATTR
  | MASK
  | IF
  | ELSE
  | FOR
  | FOR_SEP
  | INCLUDE
  | RETURN
  | LBRACE
  | RBRACE
  | FXN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | OUTPUT
  | ATTR_W
  | ATTR_H
  | ATTR_G
  | STDOUT
  | MAIN
  | BLANK
  | LOAD
  | MAP
  | SHIFT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
