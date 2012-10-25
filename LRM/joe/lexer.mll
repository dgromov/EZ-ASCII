(* File lexer.mll *)
{
        open Parser
        exception Eof
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
      [' ' '\t']                                { token lexbuf }
      | ['\n' ]                                 { EOL }
      | ['[']                                   { LBRACKET }
      | [']']                                   { RBRACKET }
      | letter (letter | digit | '_')* as id    { ID(id) }
      | digit+ as lit                           { INT(lit) }
      | eof                                     { raise Eof }



