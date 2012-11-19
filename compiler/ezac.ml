open Parser
open Ast

module NameMap = 
  Map.Make(struct
             type t = string
             let compare x y = Pervasives.compare x y
           end)

exception ReturnException of int * int NameMap.t

(* given a NameMap, print its bindings
 * *)
let env_to_str m =
  let bindings = NameMap.bindings m in
  let rec print_map_helper s = function
      [] -> s
    | hd :: tl -> print_map_helper ((fst hd) ^ " = " ^ (string_of_int (snd hd)) ^ "\n" ^ s) tl
  in print_map_helper "" bindings

let bool_of_int i =
  if i > 0 then true
  else false

let _ =
  try 
    let lexbuf =
      if Array.length Sys.argv > 1 then Lexing.from_channel(open_in Sys.argv.(1))
      else Lexing.from_channel stdin in
    let rec parseline lineno env =
      try 

        (* eval: evaluates expressions and returns (value, updated env) *)
        let rec eval env = function
            IntLiteral(e1)    -> string_of_int e1, env
          | StrLiteral(e1)    -> e1, env
          | Id(var)           -> 
              if NameMap.mem var env then
                (NameMap.find var env), env
              else raise (Failure ("Undefined identifier: " ^ var))
          | Binop(e1, op, e2) -> 
              let v1, env = eval env e1 in
              let v2, env = eval env e2 in
              let boolean i = if i then 1 else 0 in
              string_of_int (match op with
                    Plus   -> (int_of_string v1) + (int_of_string v2)
                  | Minus  -> (int_of_string v1) - (int_of_string v2)
                  | Times  -> (int_of_string v1) * (int_of_string v2)
                  | Divide -> (int_of_string v1) / (int_of_string v2)
                  | Mod    -> (int_of_string v1) mod (int_of_string v2)
                  | Eq     -> boolean((int_of_string v1) == (int_of_string v2))
                  | Neq    -> boolean((int_of_string v1) != (int_of_string v2))
                  | Lt     -> boolean((int_of_string v1) <  (int_of_string v2))
                  | Gt     -> boolean((int_of_string v1) >  (int_of_string v2))
                  | Leq    -> boolean((int_of_string v1) <= (int_of_string v2))
                  | Geq    -> boolean((int_of_string v1) >= (int_of_string v2))
                  | Or     -> boolean((bool_of_int (int_of_string v1)) || (bool_of_int (int_of_string v2)))
                  | And    -> boolean((bool_of_int (int_of_string v1)) && (bool_of_int (int_of_string v2)))
                ), env
        in
        (* execute statements and return updated environments *)                               
        let rec exec env = function
            Assign(var, e) ->
              (* update the environment for the expression first *)
              let e_val, e_env = eval env e 
              in
                var, (NameMap.add var e_val e_env)
          | If(cond, s, s2) ->
              let c1, c_env = eval env cond in
                if (bool_of_int (int_of_string c1)) then
                  exec c_env s
                else exec c_env s2
          | For(e1, e2, e3, s) ->
              let _, env = exec env e1 in 
              let rec loop env =
                let v, env = eval env e2 in
                if (bool_of_int (int_of_string v)) then
                  let v2, env = exec env e3 in
                  let _, env = exec env s in
                  loop env
                else
                  exec env s
              in loop env
        
        in
        let (var, updated_env) = 
          (* read one line of code (input) and output result *)
          exec env (Parser.program Scanner.token lexbuf)
        in match var with
          "" -> parseline (lineno + 1) updated_env
          | _ -> 
            print_endline ("> " ^ var ^ " assigned " ^ (NameMap.find var updated_env));
            flush stdout;
            parseline (lineno + 1) updated_env;

          (*print_endline ("> " ^ " = " ^ (string_of_int (NameMap.find var
           * updated_env)));*)


      with 
        | Parsing.Parse_error -> 
            print_endline ("> *** Syntax error at line " ^ string_of_int(lineno) ^ " ***");
            exit 0; 
    in parseline 1 NameMap.empty 
  with Scanner.Eof -> 
    exit 0


