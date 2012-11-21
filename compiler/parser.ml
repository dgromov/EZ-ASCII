type token =
  | INTLITERAL of (int)
  | BOOLLITERAL of (bool)
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
  | INT
  | BOOL
  | CANVAS

open Parsing;;
# 5 "compiler/parser.mly"
 open Ast 
# 61 "compiler/parser.ml"
let yytransl_const = [|
  262 (* TRUE *);
  263 (* FALSE *);
  264 (* AND *);
  265 (* OR *);
  266 (* COMMA *);
  267 (* SEMICOLON *);
  268 (* COLON *);
  269 (* LBRACKET *);
  270 (* RBRACKET *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* EOL *);
  274 (* LT *);
  275 (* GT *);
  276 (* EQ *);
  277 (* LEQ *);
  278 (* GEQ *);
  279 (* NEQ *);
  280 (* NEGATE *);
  281 (* ATTR *);
  282 (* MASK *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* FOR *);
  286 (* FOR_SEP *);
  287 (* INCLUDE *);
  288 (* RETURN *);
  289 (* LBRACE *);
  290 (* RBRACE *);
  291 (* FXN *);
  292 (* PLUS *);
  293 (* MINUS *);
  294 (* TIMES *);
  295 (* DIVIDE *);
  296 (* MOD *);
  297 (* ASSIGN *);
  298 (* OUTPUT *);
  299 (* ATTR_W *);
  300 (* ATTR_H *);
  301 (* ATTR_G *);
  302 (* STDOUT *);
  303 (* MAIN *);
  304 (* BLANK *);
  305 (* LOAD *);
  306 (* MAP *);
  307 (* SHIFT *);
  308 (* INT *);
  309 (* BOOL *);
  310 (* CANVAS *);
    0|]

let yytransl_block = [|
  257 (* INTLITERAL *);
  258 (* BOOLLITERAL *);
  259 (* ID *);
  260 (* CMP *);
  261 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\010\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\012\000\012\000\013\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\000\000\002\000\004\000\004\000\004\000\
\007\000\011\000\009\000\003\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\003\000\
\000\000\001\000\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\002\000\003\000\011\000\000\000\
\007\000\000\000\000\000\000\000\000\000\009\000\008\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\004\000\
\013\000\000\000\000\000\000\000\000\000\022\000\023\000\000\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\016\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\038\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\000\000\018\000"

let yydgoto = "\002\000\
\003\000\005\000\006\000\010\000\016\000\019\000\011\000\025\000\
\035\000\062\000\064\000\065\000\000\000"

let yysindex = "\005\000\
\000\000\000\000\012\255\036\255\000\000\000\000\000\000\032\255\
\000\000\022\255\034\255\013\255\046\255\000\000\000\000\052\255\
\045\255\000\000\016\255\227\254\042\255\051\255\152\255\000\000\
\000\000\152\255\252\254\152\255\031\255\000\000\000\000\053\255\
\000\000\152\255\077\255\100\255\056\255\060\255\125\255\152\255\
\152\255\150\255\152\255\152\255\000\000\152\255\152\255\152\255\
\152\255\152\255\152\255\152\255\152\255\152\255\152\255\152\255\
\000\000\000\000\000\000\048\255\173\255\049\255\173\255\066\255\
\079\255\000\000\000\000\000\000\044\255\044\255\022\000\044\255\
\044\255\022\000\196\255\196\255\247\255\247\255\247\255\051\255\
\051\255\000\000\152\255\067\255\069\255\173\255\059\255\051\255\
\070\255\072\255\051\255\000\000\073\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\112\000\000\000\000\000\000\000\000\000\109\255\
\000\000\000\000\110\255\000\000\000\000\000\000\000\000\231\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\111\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\098\255\000\000\254\254\000\000\
\113\255\000\000\000\000\000\000\210\255\241\255\078\000\016\000\
\047\000\089\000\094\255\173\000\120\000\131\000\162\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\255\007\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\116\000\000\000\000\000\000\000\000\000\000\000\234\255\
\233\255\000\000\000\000\000\000\000\000"

let yytablesize = 466
let yytable = "\029\000\
\037\000\012\000\036\000\012\000\039\000\001\000\012\000\043\000\
\012\000\017\000\042\000\026\000\027\000\043\000\004\000\044\000\
\061\000\063\000\020\000\067\000\068\000\044\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\017\000\009\000\017\000\017\000\012\000\017\000\017\000\
\017\000\038\000\021\000\013\000\022\000\014\000\007\000\023\000\
\015\000\024\000\008\000\043\000\044\000\020\000\017\000\007\000\
\028\000\084\000\085\000\086\000\040\000\025\000\025\000\025\000\
\025\000\090\000\058\000\041\000\093\000\025\000\059\000\025\000\
\025\000\025\000\025\000\025\000\025\000\021\000\081\000\022\000\
\080\000\082\000\023\000\025\000\043\000\044\000\089\000\045\000\
\083\000\025\000\025\000\025\000\025\000\025\000\046\000\047\000\
\048\000\049\000\050\000\051\000\087\000\088\000\091\000\026\000\
\026\000\092\000\094\000\043\000\044\000\026\000\057\000\046\000\
\052\000\053\000\054\000\055\000\056\000\046\000\047\000\048\000\
\049\000\050\000\051\000\026\000\005\000\006\000\041\000\021\000\
\042\000\026\000\026\000\018\000\043\000\044\000\000\000\052\000\
\053\000\054\000\055\000\056\000\060\000\000\000\046\000\047\000\
\048\000\049\000\050\000\051\000\000\000\000\000\000\000\000\000\
\030\000\031\000\032\000\000\000\033\000\043\000\044\000\000\000\
\052\000\053\000\054\000\055\000\056\000\066\000\034\000\046\000\
\047\000\048\000\049\000\050\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\043\000\044\000\000\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\046\000\047\000\
\048\000\049\000\050\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\043\000\044\000\000\000\000\000\000\000\
\052\000\053\000\054\000\055\000\056\000\046\000\047\000\048\000\
\049\000\050\000\051\000\033\000\033\000\000\000\000\000\000\000\
\000\000\033\000\000\000\033\000\033\000\033\000\033\000\033\000\
\033\000\054\000\055\000\056\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\000\000\000\000\033\000\033\000\033\000\
\033\000\033\000\034\000\034\000\000\000\000\000\043\000\044\000\
\034\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
\046\000\047\000\048\000\049\000\050\000\051\000\034\000\000\000\
\000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
\034\000\035\000\035\000\000\000\000\000\043\000\044\000\035\000\
\000\000\035\000\035\000\035\000\035\000\035\000\035\000\046\000\
\047\000\000\000\049\000\050\000\000\000\035\000\000\000\000\000\
\000\000\000\000\000\000\035\000\035\000\035\000\035\000\035\000\
\036\000\036\000\000\000\000\000\000\000\000\000\036\000\000\000\
\036\000\036\000\036\000\036\000\036\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\000\000\000\000\000\000\
\000\000\000\000\036\000\036\000\036\000\036\000\036\000\031\000\
\031\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\031\000\032\000\032\000\031\000\000\000\000\000\000\000\
\032\000\000\000\000\000\031\000\032\000\000\000\000\000\032\000\
\000\000\031\000\031\000\031\000\031\000\031\000\032\000\000\000\
\000\000\000\000\000\000\000\000\032\000\032\000\032\000\032\000\
\032\000\028\000\028\000\000\000\000\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\029\000\029\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\028\000\028\000\028\000\028\000\028\000\
\029\000\000\000\000\000\000\000\000\000\000\000\029\000\029\000\
\029\000\029\000\029\000\030\000\030\000\000\000\000\000\000\000\
\000\000\030\000\000\000\000\000\000\000\000\000\027\000\027\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\030\000\
\000\000\000\000\000\000\000\000\000\000\030\000\030\000\030\000\
\030\000\030\000\027\000\000\000\000\000\000\000\000\000\000\000\
\027\000\027\000"

let yycheck = "\022\000\
\005\001\027\001\026\000\029\001\028\000\001\000\032\001\010\001\
\034\001\003\001\034\000\041\001\042\001\016\001\003\001\010\001\
\040\000\041\000\003\001\043\000\044\000\016\001\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\027\001\003\001\029\001\030\001\016\001\032\001\033\001\
\034\001\046\001\027\001\010\001\029\001\033\001\011\001\032\001\
\003\001\034\001\015\001\008\001\009\001\003\001\003\001\011\001\
\015\001\080\000\081\000\083\000\030\001\008\001\009\001\010\001\
\011\001\088\000\011\001\015\001\091\000\016\001\011\001\018\001\
\019\001\020\001\021\001\022\001\023\001\027\001\030\001\029\001\
\033\001\016\001\032\001\030\001\008\001\009\001\028\001\011\001\
\010\001\036\001\037\001\038\001\039\001\040\001\018\001\019\001\
\020\001\021\001\022\001\023\001\034\001\033\001\033\001\010\001\
\011\001\034\001\034\001\008\001\009\001\016\001\011\001\000\000\
\036\001\037\001\038\001\039\001\040\001\018\001\019\001\020\001\
\021\001\022\001\023\001\030\001\016\001\016\001\016\001\030\001\
\016\001\036\001\037\001\016\000\008\001\009\001\255\255\036\001\
\037\001\038\001\039\001\040\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\255\255\
\001\001\002\001\003\001\255\255\005\001\008\001\009\001\255\255\
\036\001\037\001\038\001\039\001\040\001\016\001\015\001\018\001\
\019\001\020\001\021\001\022\001\023\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\008\001\009\001\255\255\255\255\
\255\255\036\001\037\001\038\001\039\001\040\001\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\008\001\009\001\255\255\255\255\255\255\
\036\001\037\001\038\001\039\001\040\001\018\001\019\001\020\001\
\021\001\022\001\023\001\010\001\011\001\255\255\255\255\255\255\
\255\255\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\038\001\039\001\040\001\255\255\255\255\255\255\030\001\
\255\255\255\255\255\255\255\255\255\255\036\001\037\001\038\001\
\039\001\040\001\010\001\011\001\255\255\255\255\008\001\009\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\018\001\019\001\020\001\021\001\022\001\023\001\030\001\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\039\001\
\040\001\010\001\011\001\255\255\255\255\008\001\009\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\023\001\018\001\
\019\001\255\255\021\001\022\001\255\255\030\001\255\255\255\255\
\255\255\255\255\255\255\036\001\037\001\038\001\039\001\040\001\
\010\001\011\001\255\255\255\255\255\255\255\255\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\255\255\255\255\
\255\255\255\255\255\255\255\255\030\001\255\255\255\255\255\255\
\255\255\255\255\036\001\037\001\038\001\039\001\040\001\010\001\
\011\001\255\255\255\255\255\255\255\255\016\001\255\255\255\255\
\255\255\020\001\010\001\011\001\023\001\255\255\255\255\255\255\
\016\001\255\255\255\255\030\001\020\001\255\255\255\255\023\001\
\255\255\036\001\037\001\038\001\039\001\040\001\030\001\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\039\001\
\040\001\010\001\011\001\255\255\255\255\255\255\255\255\016\001\
\255\255\255\255\255\255\255\255\010\001\011\001\255\255\255\255\
\255\255\255\255\016\001\255\255\255\255\030\001\255\255\255\255\
\255\255\255\255\255\255\036\001\037\001\038\001\039\001\040\001\
\030\001\255\255\255\255\255\255\255\255\255\255\036\001\037\001\
\038\001\039\001\040\001\010\001\011\001\255\255\255\255\255\255\
\255\255\016\001\255\255\255\255\255\255\255\255\010\001\011\001\
\255\255\255\255\255\255\255\255\016\001\255\255\255\255\030\001\
\255\255\255\255\255\255\255\255\255\255\036\001\037\001\038\001\
\039\001\040\001\030\001\255\255\255\255\255\255\255\255\255\255\
\036\001\037\001"

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
  INT\000\
  BOOL\000\
  CANVAS\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  BOOLLITERAL\000\
  ID\000\
  CMP\000\
  STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "compiler/parser.mly"
                                  ( [],[] )
# 377 "compiler/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 38 "compiler/parser.mly"
                        ( (_2 :: fst _1), snd _1 )
# 385 "compiler/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "compiler/parser.mly"
                        ( fst _1, (_2 :: snd _1) )
# 393 "compiler/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 43 "compiler/parser.mly"
    ( { fname = _1;
					formals = _3;
					locals = List.rev _6;
					body = List.rev _7 } )
# 406 "compiler/parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "compiler/parser.mly"
                ( [] )
# 412 "compiler/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 51 "compiler/parser.mly"
                ( List.rev(_1) )
# 419 "compiler/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "compiler/parser.mly"
        ( [_1] )
# 426 "compiler/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "compiler/parser.mly"
                         ( _3 :: _1 )
# 434 "compiler/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "compiler/parser.mly"
                ( [] )
# 440 "compiler/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 58 "compiler/parser.mly"
                     ( _2 :: _1 )
# 448 "compiler/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 60 "compiler/parser.mly"
               ( _1 )
# 455 "compiler/parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "compiler/parser.mly"
                   ( [] )
# 461 "compiler/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "compiler/parser.mly"
                     ( _2 :: _1 )
# 469 "compiler/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "compiler/parser.mly"
                                       ( Assign(_1, _3) )
# 477 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 69 "compiler/parser.mly"
                                       ( OutputC(_1) )
# 484 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "compiler/parser.mly"
                                       ( OutputF(_1) )
# 492 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 71 "compiler/parser.mly"
                                                                ( If(_3, _6, Block([])) )
# 500 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 72 "compiler/parser.mly"
                                                                             ( If(_3, _6, _10) )
# 509 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'stmt) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 73 "compiler/parser.mly"
                                                                    ( For(_2, _4, _6, _8) )
# 519 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "compiler/parser.mly"
                                    ( Return(_2) )
# 526 "compiler/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "compiler/parser.mly"
                                        ( _1 )
# 533 "compiler/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 84 "compiler/parser.mly"
                                          ( IntLiteral(_1) )
# 540 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 85 "compiler/parser.mly"
                         ( BoolLiteral(_1))
# 547 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "compiler/parser.mly"
                                          ( StrLiteral(_1) )
# 554 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "compiler/parser.mly"
                                          ( Id(_1) )
# 561 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "compiler/parser.mly"
                                          ( Binop(_1, Plus, _3) )
# 569 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "compiler/parser.mly"
                                          ( Binop(_1, Minus, _3) )
# 577 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "compiler/parser.mly"
                                          ( Binop(_1, Times, _3) )
# 585 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "compiler/parser.mly"
                                          ( Binop(_1, Divide, _3) )
# 593 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "compiler/parser.mly"
                                          ( Binop(_1, Mod, _3) )
# 601 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "compiler/parser.mly"
                                          ( Binop(_1, Eq, _3) )
# 609 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "compiler/parser.mly"
                                          ( Binop(_1, Neq, _3) )
# 617 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "compiler/parser.mly"
                                          ( Binop(_1, Lt, _3) )
# 625 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "compiler/parser.mly"
                                          ( Binop(_1, Gt, _3) )
# 633 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "compiler/parser.mly"
                                          ( Binop(_1, Leq, _3) )
# 641 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "compiler/parser.mly"
                                          ( Binop(_1, Geq, _3) )
# 649 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "compiler/parser.mly"
                                          ( Binop(_1, Or, _3) )
# 657 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "compiler/parser.mly"
                                          ( Binop(_1, And, _3) )
# 665 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 101 "compiler/parser.mly"
                                        ( Call(_1, _3) )
# 673 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "compiler/parser.mly"
                              ( _2 )
# 680 "compiler/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "compiler/parser.mly"
                ( [] )
# 686 "compiler/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 108 "compiler/parser.mly"
                 ( List.rev _1 )
# 693 "compiler/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "compiler/parser.mly"
       ( [_1] )
# 700 "compiler/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "compiler/parser.mly"
                            ( _3 :: _1 )
# 708 "compiler/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 115 "compiler/parser.mly"
                                ("include [filepath]")
# 715 "compiler/parser.ml"
               : 'include_stmt))
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
