(* FILENAME :  reuse.ml
 * AUTHOR(S):  Joe Lee (jyl2157)
 * PURPOSE  :  Functions for re-use.
 *)

(* given a string, splits it into a list of chars *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];

(* given a list of chars, joins them and returns a string *)
let implode lst =
  let res = String.create (List.length lst) in
  let rec imp i = function
    | [] -> res
    | c :: lst -> res.[i] <- c; imp (i + 1) lst in
    imp 0 lst;

(* debug function to inspect environment *)
let env_to_str m =
  let bindings = StringMap.bindings m in
  let rec print_map_helper s = function
      [] -> s
    | hd :: tl -> print_map_helper ((fst hd) ^ " = " ^ (string_of_int (snd hd)) ^ "\n" ^ s) tl
  in print_map_helper "" bindings;
