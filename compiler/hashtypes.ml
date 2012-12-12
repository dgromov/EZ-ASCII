(* FILENAME :  hashtypes.ml
 * AUTHOR(s):  Joe Lee (jyl2157)
 * PURPOSE  :  Define custom types for hash map values to support EZ-ASCII's
 * types.
 *)

type ct =
    (* Note: The compiler will not add any int types to the hash map 
     * but the bytecode executor might during binop operations *)
    Int of int
  | String of string
  | Bool of bool
(*  | Canvas of  *)

let string_of_ct = function
    Int(i) -> string_of_int i
  | String(s) -> (Scanf.unescaped s)
  | Bool(b) -> string_of_bool b
(* | Canvas(c) ->    *)
