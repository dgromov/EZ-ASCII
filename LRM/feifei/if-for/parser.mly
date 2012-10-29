/* File parser.mly */
%token <string> INT 
%token <string> ID
%token <string> CMP
%token AND, OR, COMMA, COLON, LBRACKET, RBRACKET, LPAREN, RPAREN, IF, ELSE, ELSEIF, SEMICOLON, LCBRA, RCBRA, LARROW, SEPERATOR, FOR, EOL

%start main             /* the entry point */
%type <string> main

%%
main:
      expr EOL                            { $1 }

expr:   INT                               { $1 }
      | ID                                { $1 }
      | ID LBRACKET select_stmt RBRACKET  { $3 }
      | if_stmt                           { $1 }
      | elseif_stmt                       { $1 }
      | for_stmt                          { $1 }

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

stmt:
        expr SEMICOLON                    { $1 }
      | if_stmt                           { "if-else part" }

if_stmt:
        IF LPAREN expr RPAREN stmt        { "if statment" }
      | IF LPAREN expr RPAREN stmt ELSE stmt {"if-else statment"}

stmt2:
        stmt                              { $1 }
      | ELSEIF LPAREN expr RPAREN stmt2   { "elseif part" }

elseif_stmt:
        IF LPAREN expr RPAREN stmt ELSEIF LPAREN expr RPAREN stmt ELSE stmt           { "else-if statment" }
      | IF LPAREN expr RPAREN stmt ELSEIF LPAREN expr RPAREN stmt stmt2 ELSE stmt     { "else-if statment" }

expr2:
        INT LARROW INT                       { "set value" }

for_stmt:
        FOR ID LARROW ID SEPERATOR expr SEPERATOR expr LCBRA stmt RCBRA               { "for statement" }



