/* File parser.mly */
%token <string> INT 
%token <string> ID
%token <string> CMP
%token <string> STR
%token TRUE, FALSE
%token AND, OR, COMMA, SEMICOLON, COLON, LBRACKET, RBRACKET
%token LT, GT, EQ, LEQ, GEQ, NEQ 
%token MASK, IF, FOR, RETURN, LBRACE, RBRACE, FXN 
%token PLUS, MINUS, TIMES, DIVIDE, MOD
%token ASSIGN, OUTPUT, STDOUT

%right ASSIGN
%left PLUS, MINUS
%left TIMES, DIVIDE, MOD
%left EQ, NEQ
%left LT, GT, GEQ, LEQ
%left AND, OR

%start main             /* the entry point */
%type <string> main

%%
main:
        stmt SEMICOLON                    { $1 }

expr:  
        INT                               { "integer: " ^ $1 }
      | STR                               { "string " ^ $1 }
      | expr PLUS expr                    { "addition" }
      | expr MINUS expr                   { "subtraction" }
      | expr TIMES expr                   { "multiplication" }
      | expr DIVIDE expr                  { "division" }
      | expr MOD expr                     { "modulus" }
      | ID LBRACKET select_stmt RBRACKET  { $3 }

stmt:
        ID ASSIGN expr                    { "assignment" }
      | expr OUTPUT STDOUT                { "output to stdout" }
      | expr OUTPUT STR                   { "output to filepath" }

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
        LT INT                            { "selection by bool expression (<)" }
      | GT INT                            { "selection by bool expression (>)" }
      | EQ INT                            { "selection by bool expression (=)" }
      | LEQ INT                           { "selection by bool expression (<=)" }
      | GEQ INT                           { "selection by bool expression (>=)" }
      | NEQ INT                           { "selection by bool expression (~=)" }
      | bool_expr AND bool_expr       { $3 }
      | bool_expr OR bool_expr        { $3 }
