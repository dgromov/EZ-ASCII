open Parser
open Ast

module NameMap = Map.Make(struct
                            type t = string
                            let compare x y = Pervasives.compare x y
                          end)

exception ReturnException of int * int NameMap.t

let _ =
  try
    let lexbuf =
      if Array.length Sys.argv > 1 then Lexing.from_channel(open_in Sys.argv.(1))
      else Lexing.from_channel stdin in
        let rec parseline lineno =
          try
            let rec eval = function
                Literal(e1) -> e1 
              | Binop(e1, op, e2) -> 
                  let v1 = eval e1 in
                  let v2 = eval e2 in
                    match op with
                        Plus   -> v1 + v2
                      | Minus  -> v1 - v2
                      | Times  -> v1 * v2
                      | Divide -> v1 / v2
                      | Mod    -> v1 mod v2

            in let result = eval (Parser.program Scanner.token lexbuf) in
            (* let result = Parser.main Scanner.token lexbuf in *)
            print_string (string_of_int result); 
            print_newline();
            flush stdout;
            parseline (lineno + 1)
          with 
            | Parsing.Parse_error -> 
                print_string ("> *** Syntax error at line " ^ string_of_int(lineno) ^ " ***");
                print_newline();
                exit 0;
        in parseline 1 
  with Scanner.Eof -> 
    exit 0


