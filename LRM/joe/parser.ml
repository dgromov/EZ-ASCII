type token =
  | INT of (string)
  | ID of (string)
  | LBRACKET
  | RBRACKET
  | EOL

open Parsing;;
let yytransl_const = [|
  259 (* LBRACKET *);
  260 (* RBRACKET *);
  261 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\005\000\000\000\000\000\001\000\
\000\000\004\000"

let yydgoto = "\002\000\
\005\000\006\000"

let yysindex = "\003\000\
\255\254\000\000\000\000\002\255\000\000\001\255\255\254\000\000\
\003\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\254\254\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\001\000"

let yytablesize = 8
let yytable = "\003\000\
\004\000\003\000\003\000\001\000\007\000\008\000\010\000\009\000"

let yycheck = "\001\001\
\002\001\004\001\005\001\001\000\003\001\005\001\004\001\007\000"

let yynames_const = "\
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
# 70 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 15 "parser.mly"
                                          ( _1 )
# 77 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 16 "parser.mly"
                                          ( _1 )
# 84 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 17 "parser.mly"
                                          ( "selection" )
# 92 "parser.ml"
               : 'expr))
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
