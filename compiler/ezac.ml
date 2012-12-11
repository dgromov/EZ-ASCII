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
open Preprocess 

type action = Ast | Interpret | Bytecode | Compile

let _ =
  let (action, debug_flag, filepath) =
    let param_count = Array.length Sys.argv in
      if param_count > 2 then
        let option = (List.assoc Sys.argv.(1) 
            [ ("-a",  (Ast, false)); 
              ("-i",  (Interpret, false));
              ("-b",  (Bytecode, false)); 
              ("-c",  (Compile, false));
              ("-cd", (Compile, true)) 
            ])
        in (fst option), (snd option), Sys.argv.(2)
      else 
        if param_count = 2 then
          (Compile, false, Sys.argv.(1))
        else raise (Failure ("Invalid number of arguments."))
  in


  let preprocessed = Preprocess.run (filepath) in 
  let lexbuf = Lexing.from_string preprocessed in
  let program = try 
        (Parser.program Scanner.token lexbuf)
      with Parsing.Parse_error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in 
          print_endline (">>> Parse error at line " ^ (string_of_int line) ^ ", character " ^ (string_of_int cnum) ^ ": '" ^ tok ^ "'");
          exit 0;
  in
    match action with
        Ast -> let listing = Ast.string_of_program program in print_string listing
      | Interpret -> print_string "Interpret: nada at the moment" (* ignore (Interpret.run program) *)
      | Bytecode -> let listing = Bytecode.string_of_prog (Compiler.translate program) 
                    in print_endline listing
      | Compile -> let program = Compiler.translate program
                   in Execute.execute_prog program debug_flag
