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

open Parsing;;
# 5 "parser.mly"
 open Ast 
# 57 "parser.ml"
let yytransl_const = [|
  261 (* TRUE *);
  262 (* FALSE *);
  263 (* AND *);
  264 (* OR *);
  265 (* COMMA *);
  266 (* SEMICOLON *);
  267 (* COLON *);
  268 (* LBRACKET *);
  269 (* RBRACKET *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* EOL *);
  273 (* LT *);
  274 (* GT *);
  275 (* EQ *);
  276 (* LEQ *);
  277 (* GEQ *);
  278 (* NEQ *);
  279 (* NEGATE *);
  280 (* ATTR *);
  281 (* MASK *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* FOR *);
  285 (* FOR_SEP *);
  286 (* INCLUDE *);
  287 (* RETURN *);
  288 (* LBRACE *);
  289 (* RBRACE *);
  290 (* FXN *);
  291 (* PLUS *);
  292 (* MINUS *);
  293 (* TIMES *);
  294 (* DIVIDE *);
  295 (* MOD *);
  296 (* ASSIGN *);
  297 (* OUTPUT *);
  298 (* ATTR_W *);
  299 (* ATTR_H *);
  300 (* ATTR_G *);
  301 (* STDOUT *);
  302 (* MAIN *);
  303 (* BLANK *);
  304 (* LOAD *);
  305 (* MAP *);
  306 (* SHIFT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
  259 (* CMP *);
  260 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\004\000\004\000\004\000\007\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\000\000\001\000\006\000\008\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\003\000\000\000\022\000\021\000\020\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\005\000\006\000\015\000"

let yysindex = "\024\000\
\001\255\000\000\221\254\014\255\000\000\024\255\022\255\252\254\
\022\255\000\000\000\000\000\000\000\000\022\255\250\254\033\255\
\041\255\027\255\052\255\022\255\022\255\000\000\022\255\022\255\
\022\255\022\255\022\255\022\255\022\255\022\255\022\255\022\255\
\022\255\000\000\000\000\020\255\000\000\000\000\000\000\002\255\
\002\255\088\255\002\255\002\255\088\255\000\255\000\255\010\000\
\010\000\010\000\001\255\021\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\082\255\
\112\255\202\255\142\255\172\255\208\255\130\255\160\255\040\255\
\046\255\100\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000\239\000"

let yytablesize = 288
let yytable = "\016\000\
\020\000\021\000\003\000\022\000\007\000\008\000\020\000\021\000\
\020\000\021\000\023\000\024\000\025\000\026\000\027\000\028\000\
\023\000\024\000\025\000\026\000\027\000\028\000\011\000\012\000\
\001\000\013\000\004\000\009\000\029\000\030\000\031\000\032\000\
\033\000\020\000\021\000\014\000\031\000\032\000\033\000\010\000\
\017\000\036\000\034\000\023\000\024\000\025\000\026\000\027\000\
\028\000\011\000\035\000\051\000\052\000\053\000\011\000\012\000\
\000\000\000\000\020\000\021\000\012\000\029\000\030\000\031\000\
\032\000\033\000\037\000\000\000\023\000\024\000\025\000\026\000\
\027\000\028\000\011\000\011\000\011\000\011\000\011\000\000\000\
\012\000\012\000\012\000\012\000\012\000\000\000\029\000\030\000\
\031\000\032\000\033\000\016\000\000\000\000\000\020\000\021\000\
\016\000\000\000\016\000\016\000\016\000\016\000\016\000\016\000\
\023\000\024\000\000\000\026\000\027\000\013\000\000\000\000\000\
\000\000\000\000\013\000\000\000\016\000\016\000\016\000\016\000\
\016\000\017\000\000\000\000\000\000\000\000\000\017\000\000\000\
\017\000\017\000\017\000\017\000\017\000\017\000\013\000\013\000\
\013\000\013\000\013\000\009\000\000\000\000\000\000\000\000\000\
\009\000\000\000\017\000\017\000\017\000\017\000\017\000\018\000\
\000\000\000\000\000\000\000\000\018\000\000\000\018\000\018\000\
\018\000\018\000\018\000\018\000\009\000\009\000\000\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\010\000\000\000\
\018\000\018\000\018\000\018\000\018\000\019\000\000\000\000\000\
\000\000\000\000\019\000\000\000\019\000\019\000\019\000\019\000\
\019\000\019\000\010\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\019\000\
\019\000\019\000\019\000\014\000\000\000\000\000\000\000\000\000\
\014\000\015\000\000\000\000\000\014\000\000\000\015\000\014\000\
\000\000\000\000\015\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\014\000\014\000\014\000\
\014\000\000\000\015\000\015\000\015\000\015\000\015\000\018\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\038\000\039\000\000\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\020\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\024\000\025\000\026\000\027\000\028\000"

let yycheck = "\004\001\
\007\001\008\001\002\001\010\001\040\001\041\001\007\001\008\001\
\007\001\008\001\017\001\018\001\019\001\020\001\021\001\022\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\002\001\
\001\000\004\001\026\001\014\001\035\001\036\001\037\001\038\001\
\039\001\007\001\008\001\014\001\037\001\038\001\039\001\016\001\
\045\001\015\001\010\001\017\001\018\001\019\001\020\001\021\001\
\022\001\010\001\010\001\032\001\051\000\033\001\015\001\010\001\
\255\255\255\255\007\001\008\001\015\001\035\001\036\001\037\001\
\038\001\039\001\015\001\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\035\001\036\001\037\001\038\001\039\001\255\255\
\035\001\036\001\037\001\038\001\039\001\255\255\035\001\036\001\
\037\001\038\001\039\001\010\001\255\255\255\255\007\001\008\001\
\015\001\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\017\001\018\001\255\255\020\001\021\001\010\001\255\255\255\255\
\255\255\255\255\015\001\255\255\035\001\036\001\037\001\038\001\
\039\001\010\001\255\255\255\255\255\255\255\255\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\035\001\036\001\
\037\001\038\001\039\001\010\001\255\255\255\255\255\255\255\255\
\015\001\255\255\035\001\036\001\037\001\038\001\039\001\010\001\
\255\255\255\255\255\255\255\255\015\001\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\035\001\036\001\255\255\255\255\
\255\255\010\001\255\255\255\255\255\255\255\255\015\001\255\255\
\035\001\036\001\037\001\038\001\039\001\010\001\255\255\255\255\
\255\255\255\255\015\001\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\035\001\036\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\035\001\036\001\
\037\001\038\001\039\001\010\001\255\255\255\255\255\255\255\255\
\015\001\010\001\255\255\255\255\019\001\255\255\015\001\022\001\
\255\255\255\255\019\001\255\255\255\255\022\001\255\255\255\255\
\255\255\255\255\255\255\255\255\035\001\036\001\037\001\038\001\
\039\001\255\255\035\001\036\001\037\001\038\001\039\001\009\000\
\255\255\255\255\255\255\255\255\014\000\255\255\255\255\255\255\
\255\255\255\255\020\000\021\000\255\255\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\007\001\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  LBRACKET\000\
  RBRACKET\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  LT\000\
  GT\000\
  EQ\000\
  LEQ\000\
  GEQ\000\
  NEQ\000\
  NEGATE\000\
  ATTR\000\
  MASK\000\
  IF\000\
  ELSE\000\
  FOR\000\
  FOR_SEP\000\
  INCLUDE\000\
  RETURN\000\
  LBRACE\000\
  RBRACE\000\
  FXN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  ASSIGN\000\
  OUTPUT\000\
  ATTR_W\000\
  ATTR_H\000\
  ATTR_G\000\
  STDOUT\000\
  MAIN\000\
  BLANK\000\
  LOAD\000\
  MAP\000\
  SHIFT\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  CMP\000\
  STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 36 "parser.mly"
                                         ( _1  )
# 297 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                                       ( Assign(_1, _3) )
# 305 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 41 "parser.mly"
                                       ( OutputC(_1) )
# 312 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 42 "parser.mly"
                                       ( OutputF(_1) )
# 320 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 43 "parser.mly"
                                                      ( If(_3, _6) )
# 328 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 55 "parser.mly"
                                          ( IntLiteral(_1) )
# 335 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                                          ( StrLiteral(_1) )
# 342 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                          ( Id(_1) )
# 349 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                                          ( Binop(_1, Plus, _3) )
# 357 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                                          ( Binop(_1, Minus, _3) )
# 365 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                                          ( Binop(_1, Times, _3) )
# 373 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                          ( Binop(_1, Divide, _3) )
# 381 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                          ( Binop(_1, Mod, _3) )
# 389 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                          ( Binop(_1, Eq, _3) )
# 397 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                          ( Binop(_1, Neq, _3) )
# 405 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                          ( Binop(_1, Lt, _3) )
# 413 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                          ( Binop(_1, Gt, _3) )
# 421 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                          ( Binop(_1, Leq, _3) )
# 429 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                          ( Binop(_1, Geq, _3) )
# 437 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                          ( Binop(_1, Or, _3) )
# 445 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                          ( Binop(_1, And, _3) )
# 453 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                          ( _2 )
# 460 "parser.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)