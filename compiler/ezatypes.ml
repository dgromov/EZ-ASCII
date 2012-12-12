(* FILENAME :  ezatypes.ml
 * AUTHOR(s):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720)
 * PURPOSE  :  Define types for semantic analysis.
 *)

type t = 
    Void 
  | Int 
  | Bool
  | Char 
  | String 
  | Canvas of int array
