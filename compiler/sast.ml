(* FILENAME :  sast.ml
 * AUTHOR(s):  Joe Lee (jyl2157)
 * PURPOSE  :  Converts ast to sast.  
 *)

open Ast

type exp_det = 
    IntLiteral of int
  | StrLiteral of string
  | BoolLiteral of bool
  | 

type ezatypes = 
    Int 
  | Bool
  | Char 
  | String 
  | Canvas

(* takes Ast program and runs static semantic analysis (type errors, etc..) *)
let semantic_checker program = 

  (* eval returns a two-tuple consisting of the converted Sast element, and
   * the Sast type *)
  let eval expr env = function
      Ast.IntLiteral(i) -> Sast.IntLiteral
