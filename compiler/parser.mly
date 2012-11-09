/* File parser.mly      */
/*                      */
/*                      */

%{ open Ast %}

%token <int> INT 
%token <string> ID
%token <string> CMP
%token <string> STR
%token TRUE, FALSE
%token AND, OR, COMMA, SEMICOLON, COLON, LBRACKET, RBRACKET, EOL
%token LT, GT, EQ, LEQ, GEQ, NEQ, NEGATE
%token ATTR, MASK, IF, ELSE, FOR, FOR_SEP, INCLUDE, RETURN, LBRACE, RBRACE, FXN 
%token PLUS, MINUS, TIMES, DIVIDE, MOD
%token ASSIGN, OUTPUT, ATTR_W, ATTR_H, ATTR_G, STDOUT
%token MAIN, BLANK, LOAD, INCLUDE, MAP, SHIFT

%left PLUS, MINUS        /* lowest precedence */
%left TIMES, DIVIDE, MOD
%left EQ, NEQ
%left LT, GT, GEQ, LEQ
%left AND, OR
%right ASSIGN
%nonassoc UMINUS         /* highest precedence */

%start program           /* the entry point */
%type <Ast.program> program 

%%
program:
        /* nothing                      { [] }  */
        stmt EOL                         { $1  }
   /*   | program fdecl                     { fst $1, ($2 :: snd $1) } */

stmt:
        ID ASSIGN expr SEMICOLON       { Assign($1, $3) }
      | ID OUTPUT STDOUT SEMICOLON     { OutputC($1) }
      | ID OUTPUT STR SEMICOLON        { OutputF($1) }

expr:  
        INT                               { IntLiteral($1) }
      | STR                               { StrLiteral($1) } 
      | ID                                { Id($1) }
      | expr PLUS expr                    { Binop($1, Plus, $3) }
      | expr MINUS expr                   { Binop($1, Minus, $3) }
      | expr TIMES expr                   { Binop($1, Times, $3) }
      | expr DIVIDE expr                  { Binop($1, Divide, $3) } 
      | expr MOD expr                     { Binop($1, Mod, $3) }
      /*| MINUS expr %prec UMINUS         { "unary minus" }
      | ID LBRACKET select_stmt RBRACKET  { $3 } */ 
/*

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
      | bool_expr AND bool_expr           { $3 }
      | bool_expr OR bool_expr            { $3 }

      */
