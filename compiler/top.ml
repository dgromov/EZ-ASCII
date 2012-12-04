(*
 *
 *
 *)

open Ast
open Parser
open Scanner
open Bytecode
open Compiler
open Execute

type action = Ast | Interpret | Bytecode | Compile

let _ =
  let (action, debug_flag, filepath) =
    let param_count = Array.length Sys.argv in
      if param_count > 2 then
        let option = (List.assoc Sys.argv.(1) [ ("-a", (Ast, false)); 
                                                ("-i", (Interpret, false));
                                                ("-b", (Bytecode, false)); 
                                                ("-c", (Compile, false));
                                                ("-cd", (Compile, true)) ])
        in (fst option), (snd option), Sys.argv.(2)
      else 
        if param_count = 2 then
          (Compile, false, Sys.argv.(1))
        else raise (Failure ("Invalid number of arguments."))
  in
  let lexbuf = Lexing.from_channel (open_in filepath) in
  let program = Parser.program Scanner.token lexbuf in
    match action with
        Ast -> let listing = "AST: nada at the moment" (* let listing = Ast.string_of_program program *)
        in print_string listing
      | Interpret -> print_string "Interpret: nada at the moment" (* ignore (Interpret.run program) *)
      | Bytecode -> 
          let listing = Bytecode.string_of_prog 
                          (Compiler.translate program) 
          in print_endline listing
      | Compile -> 
          let program = Compiler.translate program in
            Execute.execute_prog program debug_flag