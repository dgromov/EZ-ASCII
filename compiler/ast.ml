(* File: Ast.ml   *)
(*                *)

type op = Plus | Minus | Times | Divide  | Mod
        | And | Or
        | Lt  | Gt  | Eq | Leq | Geq | Neq
        | Mask

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
  | OutputF of expr * string                (* canvas -> "C:\test.png" *)
  | If of expr * stmt list                  (* if (foo = 42) {} *)
  | If_else of expr * stmt list * stmt list (* if (foo = 42) {} else {} *)
  | For of stmt * expr * stmt * stmt list   (* for i <- 0 | i < 10 | i <- i + 1 { ... } *)
  | Return of expr                          (* return 42; *)
  | Expr of expr                            (* blah *) 
  
type func_decl = {
  fname : string;                             (* Name of the function *)
  params : string list;                       (* Formal argument names *)
  body : stmt list;
}

                   
type program = stmt list * func_decl list (* global vars, funcs *) 

 let rec string_of_expr = function
    IntLiteral(l) -> string_of_int l
  | StrLiteral(l) -> l 
  | BoolLiteral(l) -> if l == true then "true" else "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (
        match o with
         Plus -> "+"      
       | Minus-> "-"      
       | Times -> "*"     
       | Divide -> "/"   
       | Mod -> "%"   
       | And -> "&&"
       | Or -> "||"   
       | Eq -> "=="       
       | Neq -> "!="     
       | Lt -> "<"        
       | Leq -> "<="      
       | Gt -> ">"        
       | Geq -> ">=" 
       | Mask -> "MASK" (* Not sure this should ever happen*)
      )  
      ^ " " ^
      string_of_expr e2
  | Call(f, el)  ->  f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ ";" 
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";"
  | If(e, sl1) -> "if (" ^ string_of_expr e ^  ")
                  {\n"  ^ String.concat "\n" (List.map string_of_stmt sl1)  ^ "\n}"
  | If_else(e, sl1, sl2) ->  
      "if (" ^ string_of_expr e  
      ^ "){\n" ^ String.concat "\n" (List.map string_of_stmt sl1)  
      ^ "\n}\nelse{\n"  
      ^ String.concat "\n" (List.map string_of_stmt sl2)  ^ "\n}"
  | For(s1, e2, s3, sl4) ->
      "for (" ^ string_of_stmt s1  ^ " | " ^ string_of_expr e2 ^ " | " ^ string_of_stmt s3  ^ ")\n
      {\n" ^ String.concat "\n" (List.map string_of_stmt sl4)  ^ "\n}"
  | OutputC(e) -> 
      string_of_expr e ^ " -> out"
  | OutputF(e, f) -> 
      string_of_expr e ^ " -> " ^ f 
  | Assign(v, e) -> 
      v ^ " <- " ^ string_of_expr e

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.params ^ ")\n{\n" 
              ^ String.concat "\n" (List.map string_of_stmt fdecl.body) 
              ^ "\n}\n"

let string_of_program (vars, funcs) =
  String.concat "\n" (List.map string_of_stmt vars) ^ "\n\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
  
