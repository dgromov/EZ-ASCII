(* FILENAME :  ast.ml
 * AUTHOR(S):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720), 
 *             Yilei Wang (yw2493), Peter Ye (xy2190)
 * PURPOSE  :  Define abstract syntax tree for EZ-ASCII.
 *)

type op = Plus | Minus | Times | Divide  | Mod
        | And | Or
        | Lt  | Gt  | Eq | Leq | Geq | Neq
        | Mask

type attr = W | H | G 

type expr = 
    IntLiteral of int                            (* 42 *)
  | StrLiteral of string                         (* "this is a string" *)
  | BoolLiteral of bool                          (* true *)
  | Id of string                                 (* foo *)
  | Binop of expr * op * expr                    (* a + b *)
  | Call of string * expr list                   (* foo(1, 25) *)
  | Load of expr * expr                          (* load("filename", 10) *)
  | Blank of expr * expr * expr                  (* blank(x, y, g) *)
  | Shift of string * int * expr
  | Select_Point  of expr * expr                 (* [1,2] *)
  | Select_Rect   of expr * expr * expr * expr   (* [1:2, 3:4] *)
  | Select_VSlice of expr * expr * expr          (* [1, 3:4] *)
  | Select_HSlice of expr * expr * expr          (* [1:2, 3] *)
  | Select_VSliceAll of expr                     (* [3, ] *) 
  | Select_HSliceAll of expr                     (* [, 3] *) 
  | Select_All                                   (* [,] *)       
  | Select of expr * expr                      (* canv[...] *)
  | Select_Binop of op * expr                    (* canv[<5] *)
  | Select_Bool of expr                          (* <5 *)
  | GetAttr of string * attr                        (* canv$w *)

type stmt =                                      (* Statements *)
    Assign of string * expr                      (* foo <- 42 *)
  | OutputC of expr * expr                             (* canvas -> out *)
  | OutputF of expr * expr * expr                     (* canvas -> "C:\test.png" *)
  | If of expr * stmt list                       (* if (foo = 42) {} *)
  | If_else of expr * stmt list * stmt list      (* if (foo = 42) {} else {} *)
  | For of stmt * expr * stmt * stmt list        (* for i <- 0 | i < 10 | i <- i + 1 { ... } *)
  | Return of expr                               (* return 42; *)
  | Include of string                            (* include super_awesome.eza *)
  | CanSet of string * expr * expr               (* can[..] <- 1 *)
  
type func_decl = {
  fname : string;                                (* Name of the function *)
  params : string list;                          (* Formal argument names *)
  body : stmt list;
}

type program = stmt list * func_decl list        (* global vars, fxn declarations *) 


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
  | Load(e1, gran) -> "Load(" ^ string_of_expr e1 ^ ", " ^ string_of_expr gran ^ ")"
  | Blank(e1, e2, e3) -> "Blank(" ^ string_of_expr e1 ^ ", " ^  string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ")"
  | Shift(e1, dir, e3) -> "Shift(" ^ e1 ^ ", " ^  string_of_int dir ^ ", " ^ string_of_expr e3 ^ ")"

  | Select_Point (x, y) -> "[" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ "] -- point select"
  | Select_Rect (x1, x2, y1, y2) ->  "[" ^ string_of_expr x1 ^  ":" ^ string_of_expr x2 ^ ", " 
                                         ^ string_of_expr y1 ^  ":" ^  string_of_expr y2 ^ "] -- rect select"
  | Select_VSlice (x1, y1, y2)  ->  "[" ^ string_of_expr x1 ^ ", " 
                                        ^ string_of_expr y1 ^ ":" ^ string_of_expr y2 ^ "] -- vslice"
  | Select_HSlice (x1, x2, y1) ->  "[" ^ string_of_expr x1 ^ ":" ^ string_of_expr x2 
                                       ^ ", " ^ string_of_expr y1 ^ "] -hslice"
  | Select_VSliceAll x1 -> "[" ^ string_of_expr x1 ^ ",] - vslice_all" 
  | Select_HSliceAll y1 -> "[," ^ string_of_expr y1 ^ "] - hslice all"
  | Select_All -> "[,] - select all"
  | Select_Binop(o, e2) -> "i " ^
      (
        match o with
       | And -> "&&"
       | Or -> "||"   
       | Eq -> "=="       
       | Neq -> "!="     
       | Lt -> "<"        
       | Leq -> "<="      
       | Gt -> ">"        
       | Geq -> ">=" 
       | _ -> "error"
       )  
      ^ " " ^
      string_of_expr e2
  | Select_Bool(e1) -> "[" ^ string_of_expr e1  ^ "] "
  | Select (canv, selection) -> string_of_expr canv ^ string_of_expr selection  

let rec string_of_stmt = function
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
  | OutputC(e, render_expr) -> 
      string_of_expr e ^ ", " ^ string_of_expr render_expr ^ " -> out"
  | OutputF(e, fname, render_expr) -> 
      string_of_expr e ^ ", " ^ string_of_expr render_expr ^ " -> " ^ string_of_expr fname
  | Assign(v, e) -> 
      v ^ " <- " ^ string_of_expr e
  | Include(str) ->
     "include " ^ str

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.params ^ ")\n{\n" 
              ^ String.concat "\n" (List.map string_of_stmt fdecl.body) 
              ^ "\n}\n"

let string_of_program (vars, funcs) =
  String.concat "\n" (List.map string_of_stmt vars) ^ "\n\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
  
