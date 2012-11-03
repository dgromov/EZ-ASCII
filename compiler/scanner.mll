(* FILE:  scanner.mlll *)
{
        open Parser
        exception Eof
}

let letter    = ['a'-'z' 'A'-'Z']
let digit     = ['0'-'9']
let dblquote  = '"'

(* printable ASCII chars, excluding double quote and forward slash *)   
let printable = ['!' '#'-'.' '0'-'~']

(* escape double quote and forward slash *)
let esc_char  = "\\\"" | "\\/"

(* allowable characters for strings *)
let strchar   = printable | ' ' | '\t' | esc_char 

rule token = parse
        [' ' '\t']                              { token lexbuf }
      | "//"                                    { comment lexbuf } 
      | ['\n' '\r']                             { EOL }
      | ","                                     { COMMA }
      | ";"                                     { SEMICOLON }

      (* arithmetic operators *)
      | "+"                                     { PLUS }
      | "-"                                     { MINUS }
      | "*"                                     { TIMES }
      | "/"                                     { DIVIDE }
      | "%"                                     { MOD }

      (* relational operators *)
      | "&&"                                    { AND }
      | "||"                                    { OR }

      (* boolean operators *)
      | "<"                                     { LT }
      | ">"                                     { GT }
      | "="                                     { EQ }
      | "<="                                    { LEQ }
      | ">="                                    { GEQ }
      | "~="                                    { NEQ }

      (* canvas operators *)
      | "&"                                     { MASK }
      | "["                                     { LBRACKET }
      | "]"                                     { RBRACKET }
      | ":"                                     { COLON }
      | "out"                                   { STDOUT }

      (* statement operators/keywords *)
      | "if"                                    { IF }
      | "for"                                   { FOR }
      | "Fun"                                   { FXN }
      | "return"                                { RETURN }
      | "{"                                     { LBRACE }
      | "}"                                     { RBRACE }
      | "<-"                                    { ASSIGN }
      | "->"                                    { OUTPUT }

      | "true"                                  { TRUE }
      | "false"                                 { FALSE }
      | letter (letter | digit | '_')* as id    { ID(id) }
      | digit+ as lit                           { INT(lit) }
      | dblquote strchar* dblquote as str       { STR(str) }
      | eof                                     { raise Eof }

and comment = parse
      (* end of line marks end of comment *)
        ['\n' '\r']                             { token lexbuf }

      (* ignore everything else *)
      | _                                       { comment lexbuf }

