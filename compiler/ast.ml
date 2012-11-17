(* File: Ast.ml   *)
(*                *)

type op = Plus | Minus | Times | Divide | Mod 
               | And | Or | Lt | Gt | Eq | Leq | Geq | Neq | Mask

type expr =                                 (* Expressions *)
    IntLiteral of int                       (* 42 *)
  | StrLiteral of string                    (* "this is a string" *)
  | Id of string                            (* foo *)
  | Binop of expr * op * expr               (* a + b *)
  | Call of string * expr list              (* foo(1, 25) *)

type stmt =                                 (* Statements *)
    Block of stmt list                      (* { ... } *)
  | Return of expr                          (* return 42; *)
  | Assign of string * expr                 (* foo <- 42 *)
  | OutputC of string                       (* canvas -> out *)
  | OutputF of string                       (* canvas -> "C:\test.png" *)
  | If of expr * stmt * stmt                      (* if (foo = 42) {} *)
  | If_else of expr * stmt list * stmt list (* if (foo = 42) {} else {} *)
  | For of expr * expr * expr * stmt list   (* for i <- 0 | i < 10 | i <- i + 1 { ... } *)

type func_decl = {
  fname : string;                           (* Name of the function *)
  args : string list;                       (* Formal argument names *)
  locals : string list;                     (* Locally defined variables *)
  body : stmt list;
}

type program = stmt  (* * func_decl list (* global vars, funcs *)  *)
