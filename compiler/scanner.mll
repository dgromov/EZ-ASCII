(* File scanner.mll *)
{
        open Parser
        exception Eof
}

let letter    = ['a'-'z' 'A'-'Z']
let digit     = ['0'-'9']
let dblquote  = '"'
let printable = ['!' '#'-'.' '0'-'~'] (* printable ASCII chars, excluding double
quote and forward slash *)   
let esc_char  = "\\\"" | "\\/"             (* escape double quote and forward slash *)
let strchar   = printable | ' ' | '\t' | esc_char    (* allowable characters for strings *)
let bool_oper = ['<' '>' '='] | "<=" | ">=" | "~="


rule token = parse
        [' ' '\t']                              { token lexbuf }
      | ['\n' '\r']                             { EOL }
      | ','                                     { COMMA }
      | ':'                                     { COLON }
      | '['                                     { LBRACKET }
      | ']'                                     { RBRACKET }
      | dblquote strchar* dblquote as str       { STR(str) }
      | bool_oper as bo                         { CMP(bo) }
      | "&&"                                    { AND }
      | "||"                                    { OR }
      | letter (letter | digit | '_')* as id    { ID(id) }
      | digit+ as lit                           { INT(lit) }
      | eof                                     { raise Eof }



