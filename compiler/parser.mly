/* FILENAME :  parser.mly
 * AUTHOR(S):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720), 
 *             Yilei Wang (yw2493), Peter Ye (xy2190), Feifei Zhong (fz2185)
 * PURPOSE  :  Parser definition for EZ-ASCII.
 */

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
      /* nothing */                     { [],[] } 
	| program stmt                        { List.rev($2 :: List.rev (fst $1)), snd $1 }
	| program funcdecl 	                  { (fst $1), List.rev ($2 :: List.rev (snd $1)) }

funcdecl:
      FXN ID LPAREN param_list RPAREN LBRACE stmt_list RBRACE
      {  
            {  
                  fname = $2; 
                  params = List.rev $4; 
                  body = List.rev $7 
            } 
      }	

param_list:
	/* nothing */						{ [] }
	| ID 								{ [$1] }
	| param_list COMMA ID  				      { $3 :: $1 }


stmt_list:
	/* nothing */ 						{ [] }
	| stmt_list stmt                                { $2 :: $1 }

stmt:
    ID ASSIGN expr SEMICOLON                  { Assign($1, $3) }
  | ID OUTPUT STDOUT SEMICOLON                { OutputC(Id($1)) }
  | ID OUTPUT STDOUT COMMA expr SEMICOLON     { OutputCR(Id($1), $5)}
  | ID OUTPUT STR SEMICOLON                   { OutputF(Id($1), $3) }
  | ID OUTPUT STR COMMA expr SEMICOLON        { OutputFR(Id($1), $3, $5)}

  | IF LPAREN expr RPAREN cond_body %prec NOELSE { If($3, $5) }
  | IF LPAREN expr RPAREN cond_body ELSE cond_body { If_else($3, $5, $7) }
  | FOR stmt_in_for FOR_SEP expr FOR_SEP stmt_in_for LBRACE
  stmt_list RBRACE   { For($2, $4, $6, List.rev $8) }
  | RETURN expr SEMICOLON             { Return($2) }
  | INCLUDE STR SEMICOLON             { Include($2) }
  | ID LBRACKET select_expr RBRACKET ASSIGN expr SEMICOLON {CanSet($1, $3, $6)}

stmt_in_for:
  ID ASSIGN expr          { Assign($1, $3) }

 
select_bool_expr:
    LT expr                           { Select_Binop (Lt, $2) }
  | GT expr                           { Select_Binop (Gt, $2) }
  | EQ expr                           { Select_Binop (Eq, $2) }
  | LEQ expr                          { Select_Binop (Leq, $2) }
  | GEQ expr                          { Select_Binop (Geq, $2) }
  | NEQ expr                          { Select_Binop (Neq, $2) }
/*   | bool_expr AND bool_expr           { $1, $3 }
  | bool_expr OR bool_expr            { $1, $3 } */


select_expr:
    expr COMMA expr                       { Select_Point($1, $3) }
  | expr COLON expr COMMA expr COLON expr { Select_Rect($1, $3, $5, $7) }
  | expr COMMA expr COLON expr            { Select_VSlice($1, $3, $5) }
  | expr COLON expr COMMA expr            { Select_HSlice($1, $3, $5) }
  | expr COMMA                            { Select_VSliceAll($1) }
  | COMMA expr                            { Select_HSliceAll($2) }
  | COMMA                                 { Select_All }
  | select_bool_expr                      { Select_Bool($1) } 

cond_body:
        /* If no braces are supplied in a 
         * conditional body, we expect one statement;
         * Otherwise, we expect a statement list.
         */
        stmt                              { [$1] }
      | LBRACE stmt_list RBRACE           { List.rev $2 }
      
expr_list:
	/* nothing */ 				{ [] }
      | expr                              { [$1] }
      | expr_list COMMA expr              { $3 :: $1 }

expr:  
        INTLITERAL		                    { IntLiteral($1) }
      | BOOLLITERAL                       { BoolLiteral($1) }
      | STR                               { StrLiteral($1) } 
      | ID                                { Id($1) }
      | expr PLUS expr                    { Binop($1, Plus, $3) }
      | expr MINUS expr                   { Binop($1, Minus, $3) }
      | expr TIMES expr                   { Binop($1, Times, $3) }
      | expr DIVIDE expr                  { Binop($1, Divide, $3) } 
      | expr MOD expr                     { Binop($1, Mod, $3) }
      | ID LPAREN expr_list RPAREN        { Call($1, List.rev $3) }
      | LPAREN expr RPAREN 			{ $2 }
/*    | MINUS expr %prec UMINUS         { "unary minus" } */ 
      | ID LBRACKET select_expr RBRACKET  { Select(Id($1), $3) } 
      | expr EQ expr                      { Binop($1, Eq, $3) }
      | expr NEQ expr                     { Binop($1, Neq, $3) }
      | expr LT expr                      { Binop($1, Lt, $3) }
      | expr GT expr                      { Binop($1, Gt, $3) }
      | expr LEQ expr                     { Binop($1, Leq, $3) }
      | expr GEQ expr                     { Binop($1, Geq, $3) }
      | expr OR expr                      { Binop($1, Or, $3) }
      | expr AND expr                     { Binop($1, And, $3) }
      | expr MASK expr                    { Binop($1, Mask, $3) }
      | LOAD LPAREN expr COMMA expr RPAREN { Load($3, $5) } 
      | BLANK LPAREN expr COMMA expr COMMA expr RPAREN { Blank ($3, $5, $7 ) }
      | SHIFT LPAREN ID COMMA INTLITERAL COMMA expr RPAREN { Shift ($3, $5, $7 ) }





/*	  
actuals_opt:
		/* nothing  { [] }
		| actuals_list { List.rev $1 }
actuals_list:
		expr { [$1] }
		| actuals_list COMMA expr { $3 :: $1 }
*/
