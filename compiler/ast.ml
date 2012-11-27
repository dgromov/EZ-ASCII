(* File: Ast.ml   *)
(*                *)

type op = Plus | Minus | Times | Divide | Mod 
               | And | Or | Lt | Gt | Eq | Leq | Geq | Neq | Mask

type expr =                                 (* Expressions *)
    IntLiteral of int                       (* 42 *)
  | StrLiteral of string                    (* "this is a string" *)
  | BoolLiteral of bool                     (* true *)
  | Id of string                            (* foo *)
  | Binop of expr * op * expr               (* a + b *)
  | Call of string * expr list              (* foo(1, 25) *)


type stmt =                                 (* Statements *)
    Assign of string * expr                 (* foo <- 42 *)
  | OutputC of expr                         (* canvas -> out *)
  | OutputF of string                       (* canvas -> "C:\test.png" *)
  | If of expr * stmt list                  (* if (foo = 42) {} *)
  | If_else of expr * stmt list * stmt list (* if (foo = 42) {} else {} *)
  | For of stmt * expr * stmt * stmt list   (* for i <- 0 | i < 10 | i <- i + 1 { ... } *)
  | Return of expr                          (* return 42; *)

(*
type init = WithoutInit of string
			|WithInit of string * expr
			
type var_decl = init list
*)
  
type func_decl = {
  fname : string;                           (* Name of the function *)
  params : string list;                       (* Formal argument names *)
  body : stmt list;
}

type program = 	stmt list * func_decl list (* global vars, funcs *)  

(*type program = var_decl list * func_decl list*)
