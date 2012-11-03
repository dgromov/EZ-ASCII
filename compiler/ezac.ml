let _ =
  try
    let lexbuf =
      if Array.length Sys.argv > 1 then Lexing.from_channel(open_in Sys.argv.(1))
      else Lexing.from_channel stdin in
        let rec parseline lineno =
          try
            let result = Parser.main Scanner.token lexbuf in
            print_string result; 
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
