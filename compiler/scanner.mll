(* FILE:  scanner.mll *)
{
        open Parser
        exception Eof 
}

let letter    = ['a'-'z' 'A'-'Z']
let digit     = ['0'-'9']
let dblquote  = '"'

(* printable ASCII chars, excluding double quote and forward slash *)   
let printable = ['!' '#'-'.' '0'-'~']

(* escape sequences: newline, horiz tab, single/double quote, back/forw slash *)
let esc_char  = "\\n" | "\\t" | "\\\"" | "\\\'" | "\\/" | "\\"

let comment = "//" _* ['\r' '\n']

(* allowable characters for strings *)
let strchar   = printable | ' ' | '\t' | esc_char 

rule token = parse
        [' ' '\t']                              { token lexbuf }
      | ['\n' '\r']                             { token lexbuf }
      | "//"                                    { comment lexbuf }
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

      (* boolean operators/keywords *)
      | "<"                                     { LT }
      | ">"                                     { GT }
      | "="                                     { EQ }
      | "<="                                    { LEQ }
      | ">="                                    { GEQ }
      | "~="                                    { NEQ }
      | "~"                                     { NEGATE }
      | "true"                                  { TRUE }
      | "false"                                 { FALSE }

      (* canvas operators/keywords/constants *)
      | "&"                                     { MASK }
      | "["                                     { LBRACKET }
      | "]"                                     { RBRACKET }
      | ":"                                     { COLON }
      | "out"                                   { STDOUT }
      (*| "SHIFT_UP"                              { INT(0) }
      | "SHIFT_LEFT"                            { INT(1) }
      | "SHIFT_DOWN"                            { INT(2) }
      | "SHIFT_RIGHT"                           { INT(3) }*)
      | "$w"                                    { ATTR_W }
      | "$h"                                    { ATTR_H }
      | "$g"                                    { ATTR_G }

      (* statement operators/keywords *)
      | "if"                                    { IF }
      | "else"                                  { ELSE }
      | "for"                                   { FOR }
      | "|"                                     { FOR_SEP }
      | "Fun"                                   { FXN }
      | "include"                               { INCLUDE }
      | "return"                                { RETURN }
        (* remove leading/trailing newlines
         * for braces *)
      | "{"                                     { LBRACE }
      | "}"                                     { RBRACE }
      | "("                                     { LPAREN }
      | ")"                                     { RPAREN }
      | "<-"                                    { ASSIGN }
      | "->"                                    { OUTPUT }

      (* built-in functions *)
      | "main"                                  { MAIN }
      | "blank"                                 { BLANK }
      | "load"                                  { LOAD }
      | "map"                                   { MAP }
      | "shift"                                 { SHIFT }

      | letter (letter | digit | '_')* as id    { ID(id) }
      | digit+ as lit                           { INTLITERAL(int_of_string lit) }

      | dblquote strchar* dblquote as str       { STR(str) }
      | eof                                     { EOF } (* raise Eof } *)

and comment = parse
      (* end of line marks end of comment *)
        ['\n' '\r']                             { token lexbuf }

      (* ignore everything else *)
      | _                                       { comment lexbuf }

