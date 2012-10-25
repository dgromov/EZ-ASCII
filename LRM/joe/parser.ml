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

open Parsing;;
let yytransl_const = [|
  260 (* AND *);
  261 (* OR *);
  262 (* COMMA *);
  263 (* COLON *);
  264 (* LBRACKET *);
  265 (* RBRACKET *);
  266 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
  259 (* CMP *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\004\000\003\000\007\000\005\000\005\000\
\002\000\002\000\001\000\001\000\002\000\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\016\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\010\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\015\000\007\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\005\000\006\000\012\000\013\000"

let yysindex = "\011\000\
\002\255\000\000\000\000\249\254\000\000\003\255\255\254\000\000\
\000\255\013\255\014\255\007\255\000\000\016\255\017\255\004\255\
\000\000\000\000\012\255\015\255\019\255\019\255\022\255\023\255\
\000\000\000\000\000\000\018\255\025\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\010\255\000\000\000\000\000\000\000\000\
\000\000\000\000\020\255\000\000\000\000\021\255\000\000\024\255\
\000\000\000\000\026\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\245\255"

let yytablesize = 36
let yytable = "\009\000\
\007\000\010\000\003\000\004\000\011\000\014\000\015\000\021\000\
\022\000\025\000\026\000\001\000\008\000\016\000\017\000\018\000\
\019\000\020\000\023\000\003\000\024\000\010\000\027\000\028\000\
\029\000\030\000\000\000\000\000\011\000\009\000\000\000\000\000\
\013\000\000\000\005\000\008\000"

let yycheck = "\001\001\
\008\001\003\001\001\001\002\001\006\001\006\001\007\001\004\001\
\005\001\021\000\022\000\001\000\010\001\001\001\001\001\009\001\
\001\001\001\001\007\001\010\001\006\001\003\001\001\001\001\001\
\007\001\001\001\255\255\255\255\009\001\009\001\255\255\255\255\
\009\001\255\255\009\001\009\001"

let yynames_const = "\
  AND\000\
  OR\000\
  COMMA\000\
  COLON\000\
  LBRACKET\000\
  RBRACKET\000\
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
# 101 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 14 "parser.mly"
                                          ( _1 )
# 108 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 15 "parser.mly"
                                          ( _1 )
# 115 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'select_stmt) in
    Obj.repr(
# 16 "parser.mly"
                                          ( _3 )
# 123 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 20 "parser.mly"
                                          ( "selection by point" )
# 131 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 21 "parser.mly"
                                          ( "selection by rectangle" )
# 141 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 22 "parser.mly"
                                          ( "selection by vertical slice" )
# 150 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 23 "parser.mly"
                                          ( "selection by horizontal slice" )
# 159 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 24 "parser.mly"
                                          ( "selection by full vertical slice" )
# 166 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 25 "parser.mly"
                                          ( "selection by full horizontal slice" )
# 173 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
                                          ( "selection by all" )
# 179 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 27 "parser.mly"
                                          ( _1 )
# 186 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 30 "parser.mly"
                                          ( "selection by bool expression" )
# 194 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 31 "parser.mly"
                                          ( _4 )
# 203 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 32 "parser.mly"
                                          ( _4 )
# 212 "parser.ml"
               : 'bool_expr))
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
