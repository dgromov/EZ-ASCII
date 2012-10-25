/* File parser.mly */
%token <string> INT 
%token <string> ID
%token LBRACKET, RBRACKET, EOL

%start main             /* the entry point */
%type <string> main

%%
main:
      expr EOL                            { $1 }
;

expr:
      INT                                 { $1 }
      | ID                                { $1 }
      | ID LBRACKET expr RBRACKET         { "selection" }
;
