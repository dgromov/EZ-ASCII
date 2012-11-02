let _ =
  try
    let lexbuf =
      if Array.length Sys.argv > 1 then Lexing.from_channel(open_in Sys.argv.(1))
      else Lexing.from_channel stdin in
        while true do
          try
            let result = Parser.main Scanner.token lexbuf in
            print_string result; 
            print_newline();
            flush stdout;
          with 
            | Parsing.Parse_error -> 
                print_string "> *** Syntax error. ***";
                print_newline();
                flush stdout;
      done
  with Scanner.Eof -> 
    exit 0
