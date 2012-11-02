(* File scanner.mll *)
{
        open Parser
        exception Eof
}

let letter    = ['a'-'z' 'A'-'Z']
let digit     = ['0'-'9']
let bool_oper = ['<' '>' '='] | "<=" | ">=" | "~="

rule token = parse
      [' ' '\t']                                { token lexbuf }
      | ['\n' ]                                 { EOL }
      | [',']                                   { COMMA }
      | [':']                                   { COLON }
      | ['[']                                   { LBRACKET }
      | [']']                                   { RBRACKET }
      | bool_oper as bo                         { CMP(bo) }
      | "&&"                                    { AND }
      | "||"                                    { OR }
      | letter (letter | digit | '_')* as id    { ID(id) }
      | digit+ as lit                           { INT(lit) }
      | eof                                     { raise Eof }



