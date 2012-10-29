(* File lexer.mll *)
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
      | ['(']                                   { LPAREN }
      | [')']                                   { RPAREN }
      | ['{']                                   { LCBRA }
      | ['}']                                   { RCBRA }
      | "<-"                                    { LARROW }
      | bool_oper as bo                         { CMP(bo) }
      | ";"                                     {SEMICOLON}
      | "&&"                                    { AND }
      | "||"                                    { OR }
      | "|"                                     { SEPERATOR }
      | "if"                                    { IF }
      | "else"                                  { ELSE }
      | "elseif"                                { ELSEIF }
      | "for"                                   { FOR }
      | letter (letter | digit | '_')* as id    { ID(id) }
      | digit+ as lit                           { INT(lit) }
      | eof                                     { raise Eof }

