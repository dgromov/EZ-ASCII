type token =
  | INT of (string)
  | ID of (string)
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | EOL

open Parsing;;
let yytransl_const = [|
  259 (* COMMA *);
  260 (* COLON *);
  261 (* LBRACKET *);
  262 (* RBRACKET *);
  263 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\004\000\003\000\007\000\005\000\005\000\
\002\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\012\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\010\000\004\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\005\000\006\000\011\000"

let yysindex = "\004\000\
\002\255\000\000\000\000\252\254\000\000\001\255\255\254\000\000\
\003\255\008\255\004\255\010\255\011\255\000\000\000\000\009\255\
\012\255\013\255\015\255\000\000\014\255\016\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\017\255\000\000\000\000\000\000\000\000\
\000\000\019\255\000\000\020\255\000\000\000\000\000\000\021\255\
\000\000\000\000\000\000\000\000\022\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000"

let yytablesize = 28
let yytable = "\009\000\
\007\000\010\000\003\000\004\000\001\000\012\000\013\000\008\000\
\014\000\015\000\016\000\017\000\018\000\020\000\019\000\021\000\
\023\000\022\000\000\000\000\000\000\000\000\000\000\000\003\000\
\011\000\009\000\005\000\008\000"

let yycheck = "\001\001\
\005\001\003\001\001\001\002\001\001\000\003\001\004\001\007\001\
\001\001\006\001\001\001\001\001\004\001\001\001\003\001\001\001\
\001\001\004\001\255\255\255\255\255\255\255\255\255\255\007\001\
\006\001\006\001\006\001\006\001"

let yynames_const = "\
  COMMA\000\
  COLON\000\
  LBRACKET\000\
  RBRACKET\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 11 "parser.mly"
                                          ( _1 )
# 87 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 13 "parser.mly"
                                          ( _1 )
# 94 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 14 "parser.mly"
                                          ( _1 )
# 101 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'select_stmt) in
    Obj.repr(
# 15 "parser.mly"
                                          ( _3 )
# 109 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 19 "parser.mly"
                                          ( "selection by point" )
# 117 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 20 "parser.mly"
                                          ( "selection by rectangle" )
# 127 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 21 "parser.mly"
                                          ( "selection by vertical slice" )
# 136 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 22 "parser.mly"
                                          ( "selection by horizontal slice" )
# 145 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 23 "parser.mly"
                                          ( "selection by full vertical slice" )
# 152 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 24 "parser.mly"
                                          ( "selection by full horizontal slice" )
# 159 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
                                          ( "selection by all" )
# 165 "parser.ml"
               : 'select_stmt))
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
