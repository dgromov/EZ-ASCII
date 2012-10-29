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

open Parsing;;
let yytransl_const = [|
  260 (* AND *);
  261 (* OR *);
  262 (* COMMA *);
  263 (* COLON *);
  264 (* LBRACKET *);
  265 (* RBRACKET *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* IF *);
  269 (* ELSE *);
  270 (* ELSEIF *);
  271 (* SEMICOLON *);
  272 (* LCBRA *);
  273 (* RCBRA *);
  274 (* LARROW *);
  275 (* SEPERATOR *);
  276 (* FOR *);
  277 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
  259 (* CMP *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\007\000\
\007\000\007\000\008\000\008\000\004\000\004\000\009\000\009\000\
\005\000\005\000\010\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\004\000\001\000\001\000\001\000\003\000\
\007\000\005\000\005\000\002\000\002\000\001\000\001\000\002\000\
\004\000\004\000\002\000\001\000\005\000\007\000\001\000\005\000\
\012\000\013\000\003\000\011\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\029\000\000\000\
\005\000\006\000\007\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\013\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\018\000\019\000\000\000\000\000\000\000\010\000\000\000\022\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\028\000\025\000\
\000\000\000\000\000\000\026\000\000\000\024\000"

let yydgoto = "\002\000\
\007\000\034\000\019\000\035\000\010\000\011\000\020\000\061\000\
\062\000\000\000"

let yysindex = "\005\000\
\043\255\000\000\000\000\003\255\014\255\039\255\000\000\033\255\
\000\000\000\000\000\000\034\255\043\255\047\255\000\000\253\254\
\058\255\068\255\063\255\000\000\062\255\072\255\074\255\075\255\
\026\255\000\000\000\000\043\255\065\255\070\255\076\255\082\255\
\082\255\073\255\000\000\020\255\043\255\086\255\089\255\000\000\
\000\000\000\000\043\255\081\255\077\255\000\000\085\255\000\000\
\043\255\043\255\092\255\083\255\079\255\000\000\043\255\043\255\
\066\255\080\255\043\255\088\255\000\000\087\255\000\000\000\000\
\043\255\043\255\090\255\000\000\069\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\250\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\093\255\000\000\000\000\000\000\000\000\094\255\000\000\
\095\255\000\000\000\000\000\000\000\000\096\255\000\000\000\000\
\000\000\000\000\006\255\041\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\097\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\001\000\000\000\000\000\014\000\229\255\
\030\000\000\000"

let yytablesize = 106
let yytable = "\008\000\
\036\000\009\000\023\000\024\000\003\000\001\000\020\000\020\000\
\003\000\003\000\012\000\021\000\003\000\009\000\003\000\048\000\
\020\000\020\000\020\000\020\000\005\000\020\000\020\000\013\000\
\020\000\020\000\020\000\057\000\058\000\032\000\033\000\064\000\
\043\000\044\000\016\000\045\000\017\000\009\000\068\000\018\000\
\014\000\021\000\021\000\003\000\004\000\040\000\041\000\052\000\
\053\000\009\000\009\000\021\000\021\000\015\000\005\000\021\000\
\021\000\021\000\025\000\021\000\021\000\021\000\006\000\067\000\
\022\000\009\000\003\000\004\000\026\000\003\000\004\000\027\000\
\028\000\029\000\030\000\031\000\038\000\005\000\059\000\060\000\
\005\000\039\000\060\000\037\000\017\000\006\000\046\000\042\000\
\006\000\047\000\049\000\051\000\054\000\055\000\056\000\050\000\
\063\000\065\000\070\000\066\000\069\000\014\000\012\000\016\000\
\008\000\011\000"

let yycheck = "\001\000\
\028\000\001\000\006\001\007\001\011\001\001\000\001\001\002\001\
\015\001\016\001\008\001\013\000\019\001\013\000\021\001\043\000\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\010\001\
\019\001\020\001\021\001\055\000\056\000\004\001\005\001\059\000\
\013\001\014\001\001\001\037\000\003\001\037\000\066\000\006\001\
\002\001\001\001\002\001\001\001\002\001\032\000\033\000\049\000\
\050\000\049\000\050\000\011\001\012\001\021\001\012\001\015\001\
\016\001\017\001\001\001\019\001\020\001\021\001\020\001\065\000\
\018\001\065\000\001\001\002\001\001\001\001\001\002\001\009\001\
\011\001\002\001\001\001\001\001\007\001\012\001\013\001\014\001\
\012\001\006\001\014\001\019\001\003\001\020\001\001\001\015\001\
\020\001\001\001\010\001\007\001\001\001\011\001\016\001\019\001\
\017\001\010\001\069\000\013\001\011\001\009\001\009\001\009\001\
\009\001\009\001"

let yynames_const = "\
  AND\000\
  OR\000\
  COMMA\000\
  COLON\000\
  LBRACKET\000\
  RBRACKET\000\
  LPAREN\000\
  RPAREN\000\
  IF\000\
  ELSE\000\
  ELSEIF\000\
  SEMICOLON\000\
  LCBRA\000\
  RCBRA\000\
  LARROW\000\
  SEPERATOR\000\
  FOR\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  CMP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 12 "parser.mly"
                                          ( _1 )
# 173 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 14 "parser.mly"
                                          ( _1 )
# 180 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 15 "parser.mly"
                                          ( _1 )
# 187 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'select_stmt) in
    Obj.repr(
# 16 "parser.mly"
                                          ( _3 )
# 195 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_stmt) in
    Obj.repr(
# 17 "parser.mly"
                                          ( _1 )
# 202 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'elseif_stmt) in
    Obj.repr(
# 18 "parser.mly"
                                          ( _1 )
# 209 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'for_stmt) in
    Obj.repr(
# 19 "parser.mly"
                                          ( _1 )
# 216 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 22 "parser.mly"
                                          ( "selection by point" )
# 224 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 23 "parser.mly"
                                          ( "selection by rectangle" )
# 234 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 24 "parser.mly"
                                          ( "selection by vertical slice" )
# 243 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 25 "parser.mly"
                                          ( "selection by horizontal slice" )
# 252 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 26 "parser.mly"
                                          ( "selection by full vertical slice" )
# 259 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 27 "parser.mly"
                                          ( "selection by full horizontal slice" )
# 266 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
                                          ( "selection by all" )
# 272 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 29 "parser.mly"
                                          ( _1 )
# 279 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "parser.mly"
                                          ( "selection by bool expression" )
# 287 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 33 "parser.mly"
                                          ( _4 )
# 296 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 34 "parser.mly"
                                          ( _4 )
# 305 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                          ( _1 )
# 312 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_stmt) in
    Obj.repr(
# 38 "parser.mly"
                                          ( "if-else part" )
# 319 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 41 "parser.mly"
                                          ( "if statment" )
# 327 "parser.ml"
               : 'if_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 42 "parser.mly"
                                             ("if-else statment")
# 336 "parser.ml"
               : 'if_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 45 "parser.mly"
                                          ( _1 )
# 343 "parser.ml"
               : 'stmt2))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt2) in
    Obj.repr(
# 46 "parser.mly"
                                          ( "elseif part" )
# 351 "parser.ml"
               : 'stmt2))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 9 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 7 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _12 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 49 "parser.mly"
                                                                                      ( "else-if statment" )
# 362 "parser.ml"
               : 'elseif_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 10 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : 'stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 3 : 'stmt) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : 'stmt2) in
    let _13 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 50 "parser.mly"
                                                                                      ( "else-if statment" )
# 374 "parser.ml"
               : 'elseif_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                             ( "set value" )
# 382 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                                                                                      ( "for statement" )
# 393 "parser.ml"
               : 'for_stmt))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string)
