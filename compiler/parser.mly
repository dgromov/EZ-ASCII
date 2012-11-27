/* File parser.mly      */
/*                      */
/*                      */

%{ open Ast %}

%token <int> INTLITERAL
%token <bool> BOOLLITERAL
%token <string> ID
%token <string> CMP
%token <string> STR
%token TRUE, FALSE
%token AND, OR, COMMA, SEMICOLON, COLON, LBRACKET, RBRACKET, LPAREN, RPAREN, EOF
%token LT, GT, EQ, LEQ, GEQ, NEQ, NEGATE
%token ATTR, MASK, IF, ELSE, FOR, FOR_SEP, INCLUDE, RETURN, LBRACE, RBRACE, FXN 
%token PLUS, MINUS, TIMES, DIVIDE, MOD
%token ASSIGN, OUTPUT, ATTR_W, ATTR_H, ATTR_G, STDOUT
%token MAIN, BLANK, LOAD, INCLUDE, MAP, SHIFT
%token CANVAS

%nonassoc NOELSE
%nonassoc ELSE
%left PLUS, MINUS        /* lowest precedence */
%left TIMES, DIVIDE, MOD
%left EQ, NEQ
%left LT, GT, GEQ, LEQ
%left AND, OR
%left LPAREN, RPAREN
%right ASSIGN
%nonassoc UMINUS         /* highest precedence */

%start program           /* the entry point */
%type <Ast.program> program 

%%
program:
         /*nothing  */                 { [],[] } 
	| program stmt			{ 	List.rev($2 :: List.rev(fst $1)), snd $1 }
	| program funcdecl 		{ List.rev (fst $1), ($2 :: snd $1) }

funcdecl:
	 ID LPAREN param_list RPAREN LBRACE stmt_list RBRACE
															{ { fname = $1;
																params = List.rev $3;
																body  = List.rev $6 } }	

param_list:
	/*nothing	*/						{ [] }
	| ID 								{ [$1] }
	| param_list COMMA ID  				{ $3 :: $1 }


stmt_list:
	/* nothing */ 						{ [] }
	| stmt_list stmt                    { $2 :: $1 }


        
stmt:
        ID ASSIGN expr SEMICOLON       { Assign($1, $3) }
      | ID OUTPUT STDOUT SEMICOLON        { OutputC(Id($1)) }
      | ID OUTPUT STR SEMICOLON           { OutputF($1) }
/*	    | RETURN expr SEMICOLON          { Return($2) }*/
      | IF LPAREN expr RPAREN cond_body %prec NOELSE { If($3, $5) }
      | IF LPAREN expr RPAREN cond_body ELSE cond_body { If_else($3, $5, $7) }
      | FOR stmt_in_for FOR_SEP expr FOR_SEP stmt_in_for LBRACE
      stmt_list RBRACE   { For($2, $4, $6, List.rev $8) }



stmt_in_for:
        ID ASSIGN expr          { Assign($1, $3) }

cond_body:
        /* If no braces are supplied in a 
         * conditional body, we expect one statement;
         * Otherwise, we expect a statement list.
         */
        stmt                              { [$1] }
      | LBRACE stmt_list RBRACE           { List.rev $2 }
      
expr:  
        INTLITERAL		            { IntLiteral($1) }
      | BOOLLITERAL                       { BoolLiteral($1) }
      | STR                               { StrLiteral($1) } 
      | ID                                { Id($1) }
      | expr PLUS expr                    { Binop($1, Plus, $3) }
      | expr MINUS expr                   { Binop($1, Minus, $3) }
      | expr TIMES expr                   { Binop($1, Times, $3) }
      | expr DIVIDE expr                  { Binop($1, Divide, $3) } 
      | expr MOD expr                     { Binop($1, Mod, $3) }
/*    | ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) }*/
      | LPAREN expr RPAREN 			{ $2 }
/*    | MINUS expr %prec UMINUS         { "unary minus" }
      | ID LBRACKET select_stmt RBRACKET  { $3 } */ 

      | expr EQ expr                      { Binop($1, Eq, $3) }
      | expr NEQ expr                     { Binop($1, Neq, $3) }
      | expr LT expr                      { Binop($1, Lt, $3) }
      | expr GT expr                      { Binop($1, Gt, $3) }
      | expr LEQ expr                     { Binop($1, Leq, $3) }
      | expr GEQ expr                     { Binop($1, Geq, $3) }
      | expr OR expr                      { Binop($1, Or, $3) }
      | expr AND expr                     { Binop($1, And, $3) }

/*	  
actuals_opt:
		/* nothing  { [] }
		| actuals_list { List.rev $1 }
actuals_list:
		expr { [$1] }
		| actuals_list COMMA expr { $3 :: $1 }
		

include_stmt:
		| LBRACKET STR RBRACKET     		{"include [filepath]"}
*/
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
