(* FILENAME :  ezac.ml
 * AUTHOR(S):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720)
 * PURPOSE  :  Top-level file providing command-line interface to
 *             bytecode executor, compiler, interpreter. 
 *)

open Ast
open Parser
open Scanner
open Bytecode
open Ssanalyzer
open Compiler
open Execute
open Preprocess 

type action = Ast | StaticSemanticChecker | Interpret | Bytecode | Compile

let _ =
  let (action, debug_flag, filepath) =
    let param_count = Array.length Sys.argv in
      if param_count > 2 then
        let option = (List.assoc Sys.argv.(1) 
            [ ("-a",  (Ast, false));
              ("-s",  (StaticSemanticChecker, false));
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
  let run_ssanalyzer program = 
    (try
       let ret = Ssanalyzer.semantic_checker program
       in ret
     with 
         TypeException(astexpr1, astexpr2, expected_typ, actual_typ) ->
           print_endline("Type error at subexpression " ^ (Ast.string_of_expr astexpr1) ^ " in expression " ^ (Ast.string_of_expr astexpr2) ^ ".  Expected type " ^ (Ssanalyzer.string_of_t expected_typ) ^ " but got type " ^ (Ssanalyzer.string_of_t actual_typ ^ "."));
           exit(0)
       | UndefinedVarException(astexpr) ->
           print_endline("Undefined variable " ^ (Ast.string_of_expr astexpr)); 
           exit(0) 
       | UndefinedFxnException(fxn_name, astexpr2) ->
           print_endline("Undefined function " ^ fxn_name ^ " in expression " ^ (Ast.string_of_expr astexpr2));
           exit(0)    
       | Failure(s) ->
           print_endline(s);
           exit(0)
    )
  in 
    match action with
        Ast -> let listing = Ast.string_of_program program in print_string listing
      | StaticSemanticChecker ->
          let _ = run_ssanalyzer program in 
            print_endline("Static semantic checker finished with no errors.")
      | Interpret -> 
          print_string "Interpret: nada at the moment" (* ignore (Interpret.run program) *)
      | Bytecode -> 
          let listing = Bytecode.string_of_prog (Compiler.translate program) 
          in print_endline listing
      | Compile -> 
          let checked_prog = run_ssanalyzer program
          in 
          let program = Compiler.translate checked_prog
          in Execute.execute_prog program debug_flag
