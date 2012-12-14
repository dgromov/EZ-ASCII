(* FILENAME :  sast.ml
 * AUTHOR(s):  Joe Lee (jyl2157)
 * PURPOSE  :  Defines sast.
 *)

type expr_detail = 
    IntLiteral of int
  | StrLiteral of string
  | BoolLiteral of bool
  | Binop of Ast.expr * Ast.op * Ast.expr 
  | Load of Ast.expr * Ast.expr
