open Ast
open Bytecode
open Ezatypes

module StringMap = Map.Make(String)

(* Translation environment *)
type env = {
  function_idx          : int StringMap.t;   (* Index for each function *)
  mutable global_idx    : (int * Ezatypes.t) StringMap.t;   (* "Address" for global vars *)
  (* need to add typing for locals *)
  local_idx             : int StringMap.t;   (* FP offset for args, locals *)
  num_formals           : int;               (* Number of parameters *)
}

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
      
(* enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* string_map_pairs : StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(* object representing an environment *)
(* type trans_env = {
  
  return_type = Eatypes.t;
  scope = symbol_table;
} *)

(* Translate a program in AST form into a bytecode program.
 * Throw an exception if something is wrong, (e.g. reference
 * to an unknown var or function.
 *)
let translate (stmt_lst, func_decls) =

  let built_in_functions = 
    let rec bif_helper map counter = function
        [] -> map
      | hd :: tl -> 
          (bif_helper (StringMap.add hd (counter) map) (counter-1)) tl
    (* add built-in functions here *)
    (* reserve -1, -2, and -3 for printing ints, strings, and bools respectively
     *)
    in (bif_helper StringMap.empty (-4)) ["load"; "blank"; "main"]
  in  

  let function_indexes = string_map_pairs built_in_functions
                           (* start built-in functions at 2, reserve 1 for
                            * top-level statements pseudofunction *)
                           (enum 1 2 (List.map (fun f -> f.fname) func_decls)) in

  (* Translate an expr *)
  let rec expr env = function
      Ast.IntLiteral(i) -> [Lit i], Ezatypes.Int
    | Ast.StrLiteral(s) -> 
        let ints_list = (List.map Char.code (explode s)) in
        let size = (List.length ints_list) in
        let rec add_int_lits accum i =
          if i > size
          then accum
          else
            add_int_lits ([Lit (List.nth ints_list (i-1))] @ accum) (i+1)
        in 
          ((List.rev (add_int_lits [] 1)) @ [Lit size]), Ezatypes.String

    | Ast.BoolLiteral(b) -> if b then ([Lit 1], Ezatypes.Bool) else ([Lit 0], Ezatypes.Bool)
    | Ast.Id(s) ->
        (try 
           let search_local = (StringMap.find s env.local_idx) in
             (* NEED TO ADD TYPE, currently returns Void every time *)
             (* [Lfp (fst search_local)], snd search_local *)
             [Lfp search_local], Ezatypes.Void
         with Not_found -> 
           try
             let search_global = StringMap.find s env.global_idx in
             [Lod (fst search_global)], snd search_global
           with Not_found ->
             raise (Failure ("Undeclared variable " ^ s)))
    | Ast.Binop(e1, op, e2) ->
        let ev1 = (expr env) e1
        and ev2 = (expr env) e2 in
        ((fst ev1) @ (fst ev2) @ [Bin op]), (snd ev1)
    | Ast.Call(fname, actuals) ->
        (try
           let res = (List.map (expr env) (List.rev actuals)) in
           ((List.concat (List.map fst res)) @ [Jsr (StringMap.find fname env.function_idx)]), Ezatypes.Void
         with Not_found ->
           raise (Failure ("Undefined function: " ^ fname)))

  in let rec stmt env = function
      (* need to update assign later *)
      Ast.Assign(var, e) -> 
        let ev = (expr env e) in
          fst ev @
          [Str
             (if (StringMap.mem var env.global_idx) 
              then fst (StringMap.find var env.global_idx) 
              else 
                (let new_global_idx = (List.length (StringMap.bindings env.global_idx))
                 in 
                   env.global_idx <- (StringMap.add var (new_global_idx, (snd ev)) env.global_idx);
                   new_global_idx))] 
    | Ast.OutputC(var) ->
        let (bc, typ) = (expr env var) in
          (match typ with
               Ezatypes.Int -> bc @ [Jsr (-1)]
             | Ezatypes.String -> bc @ [Jsr (-2)]
             | Ezatypes.Bool -> bc @ [Jsr (-3)]
             | _ -> bc @ [Jsr (-1)])
    | Ast.OutputF(var, dest) ->
        []
    | Ast.If(cond, stmt_lst) ->
        []
    | Ast.If_else(cond, stmt_lst1, stmt_lst2) ->
        []
    | Ast.For(s1, e1, s2, stmt_lst) ->
        []
    | Ast.Return(e) ->
        fst (expr env e) @ [Rts env.num_formals]  
    
    | Ast.Expr(e) -> []

  
  in let translate env fdecl = 
    (* Bookkeeping: FP offsets for locals and args *)
    let num_formals = List.length fdecl.params

    (* we don't currently have locals...*)
    and num_locals = 0 (* List.length *)

    and formal_offsets = (enum (-1) (-2) fdecl.params)
    in
    let env = { env with local_idx = string_map_pairs StringMap.empty formal_offsets;
                         num_formals = num_formals } 

    in let env_to_str m =
      let bindings = StringMap.bindings m in
      let rec print_map_helper s = function
          [] -> s
        | hd :: tl -> print_map_helper ((fst hd) ^ " = " ^ (string_of_int (snd hd)) ^ "\n" ^ s) tl
      in print_map_helper "" bindings

    in (* print_endline (env_to_str env.local_idx);  *)
      [Ent num_locals] @                                   (* Entry: allocate space for locals *) 
       (List.concat (List.map (stmt env) fdecl.body)) @           (* Body *) 
       [Lit 0; Rts num_formals]                             (* Default - return 0 *)

  in let env = { 
    function_idx  = function_indexes;
    global_idx    = StringMap.empty; (* global_indexes; *)
    local_idx     = StringMap.empty; 
    num_formals   = 0
  } in

  (* we will stick top-level global statements in a psuedo-function at Jsr 1
   * still need to handle an actual main() function later
   *)
  (* let entry_function = 
     [Jsr 1; Hlt] in*)

  (* Compile the global statement list *)
  let glob_stmts = (List.concat (List.map (stmt env) stmt_lst)) in

  (* Compile the functions *)
  let func_bodies = glob_stmts :: [Hlt] :: List.map (translate env) func_decls in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
                               (fun (l,i) f -> (i :: l, (i + List.length f))) 
                               ([], 0)
                               func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) 
  in
    {
      num_globals = List.length (StringMap.bindings env.global_idx); 
      (* Concatenate the compiled functions and replace the
       * function indexes in Jsr statements with PC values *)
      text = Array.of_list 
               (List.map (function Jsr i when i > 0 ->
                            Jsr func_offset.(i)
                            | _ as s -> s) 
                  (List.concat func_bodies))
    }



