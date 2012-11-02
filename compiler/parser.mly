/* File parser.mly */
%token <string> INT 
%token <string> ID
%token <string> CMP
%token <string> STR
%token AND, OR, COMMA, COLON, LBRACKET, RBRACKET, EOL

%start main             /* the entry point */
%type <string> main

%%
main:
      expr EOL                            { $1 }

expr:   
        literal                           { $1 }
      | ID                                { $1 }
      | ID LBRACKET select_stmt RBRACKET  { $3 }

literal:
        INT                               { "integer: " ^ $1 }
      | STR                               { "string: " ^ $1 }  

select_stmt:
        INT COMMA INT                     { "selection by point" }
      | INT COLON INT COMMA INT COLON INT { "selection by rectangle" }
      | INT COMMA INT COLON INT           { "selection by vertical slice" }
      | INT COLON INT COMMA INT           { "selection by horizontal slice" }
      | INT COMMA                         { "selection by full vertical slice" }
      | COMMA INT                         { "selection by full horizontal slice" }
      | COMMA                             { "selection by all" }
      | bool_expr                         { $1 }

bool_expr:
        CMP INT                           { "selection by bool expression" }
      | CMP INT AND bool_expr             { $4 }
      | CMP INT OR bool_expr              { $4 }
