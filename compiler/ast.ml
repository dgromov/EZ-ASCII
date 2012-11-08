type op = 	Plus | Minus | Times | Divide | Mod | And | Or | Lt | Gt | Eq | Leq | Geq | Neq | Mask

type expr = (* Expressions *)
Literal of int (* 42 *)
| Id of string (* foo *)
| Binop of expr * op * expr (* a + b *)
| Assign of string * expr (* foo <- 42 *)
| Output of string * expr (* canvas -> out *)
| Call of string * expr list (* foo(1, 25) *)
| Noexpr (* for (;;) *)

type stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expr (* foo <- bar + 3; *)
| Return of expr (* return 42; *)
| If of expr * stmt (* if (foo = 42) {} *)
| If_else of expr * stmt * stmt (* if (foo = 42) {} else {} *)
| For of expr * expr * expr * stmt (* for i<-0|i<10|i<-i+1 { ... } *)

type func_decl = {
fname : string; (* Name of the function *)
args : string list; (* Formal argument names *)
locals : string list; (* Locally defined variables *)
body : stmt list;
}

type program = expr (* stmt list * func_decl list (* global vars, funcs *) *)

