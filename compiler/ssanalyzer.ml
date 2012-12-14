(* FILENAME :  ssanalyzer.ml
 * AUTHOR(s):  Joe Lee (jyl2157)
 * PURPOSE  :  Converts ast to sast.  
 *)

open Ast
open Sast

type t = 
    Void
  | Int 
  | Bool
  | Char 
  | String 
  | Canvas

let string_of_t = function
    Int -> "Int"
  | Bool -> "Bool"
  | Char -> "Char"
  | String -> "String"
  | Canvas -> "Canvas"

module StringMap = Map.Make(String)

type fxn_env = {
  mutable local_env     : (Sast.expr_detail * t) StringMap.t;
  mutable ret_type      : (Sast.expr_detail * t);
  fxn_name              : string;
  fxn_params            : string list;
  fxn_body              : Ast.stmt list;
}

(* Translation environment *)
type env = {
  mutable global_env    : (Sast.expr_detail * t) StringMap.t;
  mutable fxn_envs      : fxn_env StringMap.t; 
}

exception TypeException of string

(* takes Ast program and runs static semantic analysis (type errors, etc..) *)
let semantic_checker (stmt_lst, func_decls) =

  (* Check an expr *)
  let rec expr env scope = function
      Ast.IntLiteral(i) -> 
        Sast.IntLiteral(i), Int 
    | Ast.StrLiteral(s) -> 
        Sast.StrLiteral(s), String
    | Ast.BoolLiteral(b) -> 
        Sast.BoolLiteral(b), Bool
    | Ast.Id(s) ->
        if scope <> "*global*"
        then 
          (try 
             let search_local = (StringMap.find s (StringMap.find scope env.fxn_envs).local_env) in
               search_local
           with Not_found -> 
             (try
                let search_global = StringMap.find s env.global_env in
                  search_global
              with Not_found ->
                raise (Failure ("Undeclared variable " ^ s))))
(*
        else
          if scope = "*global*"
          then
 *)
        else
          (try
             let search_global = StringMap.find s env.global_env in
               search_global
           with Not_found ->
             raise (Failure ("Undeclared variable " ^ s)))
    | Ast.Binop(e1, op, e2) ->
        let (v1, t1) = expr env scope e1
        and (v2, t2) = expr env scope e2 in
          (match op with
              Ast.Plus ->
                (match (t1, t2) with
                     (Int, Int) ->
                       Sast.Binop(e1, op, e2), Int
                   | (String, String) ->
                       Sast.Binop(e1, op, e2), String
                   | (_, _) ->
                       raise(TypeException("Type error: " ^ (Ast.string_of_expr (Ast.Binop(e1, op, e2))) ^ ".  Operands must be both string or int types."));
                )
          );
    | Ast.Call(fname, actuals) ->
        (try
           (* first evaluate the actuals *)
           let res = (List.map (expr env scope) (List.rev actuals)) in
           let fxn_env_lookup = (StringMap.find fname env.fxn_envs) in
           let bindings = List.combine (fxn_env_lookup.fxn_params) res in
           let rec init_loc_env accum_env = function
               [] -> accum_env
             | (param_name, (sast_elem, typ)) :: tail ->
                 init_loc_env (StringMap.add param_name (sast_elem, typ) accum_env) tail
           in
             (* Side effect: Initialize a local environment with the new parameter values *)
             fxn_env_lookup.local_env <- init_loc_env StringMap.empty bindings;
             (* execute the function_body, which will eventually
              * update the return type *)
             List.map (stmt env fxn_env_lookup.fxn_name) fxn_env_lookup.fxn_body;
             (* finally, return the possibly updated return type *)
             fxn_env_lookup.ret_type
         with Not_found ->
           raise (Failure ("Undefined function: " ^ fname)))
    | Ast.Load(filepath_expr, gran_expr) ->
        let (v1, t1) = (expr env scope) filepath_expr
        and (v2, t2) = (expr env scope) gran_expr in
          if not (t1 = String && t2 = Int)
          then 
            raise(TypeException("Type error: " ^ (Ast.string_of_expr (Ast.Load(filepath_expr, gran_expr))) ^ ".  Load expects a string filepath and an integer granularity."))
          else Sast.Load(filepath_expr, gran_expr), Canvas
    | Ast.Select_Point (x, y) -> Sast.IntLiteral(1), Canvas 
    | Ast.Select_Rect (x1, x2, y1, y2) -> Sast.IntLiteral(1), Canvas 
    | Ast.Select_VSlice (x1, y1, y2)  -> Sast.IntLiteral(1), Canvas 
(*
    | Ast.Select_HSlice (x1, x2, y1) -> 
    | Ast.Select_VSliceAll x -> [Lit 1]
    | Ast.Select_HSliceAll y -> [Lit 1]
    | Ast.Select_All -> [Lit (-1)]
    | Ast.Select (canv, selection) -> [Lit (-16)]
    | Ast.Select_Binop(op, e) -> [Lit 1]
    | Ast.Select_Bool(e) -> [Lit 1]
 *)
   
  and stmt env scope = function
      (* need to update assign later *)
      Ast.Assign(var, e) -> 
        let ev = (expr env scope e) in
          if scope <> "*global*"
          then 
            (*
             * if we are in a function, variable lookup proceeds as:
             * 1) Check if the variable is a formal (parameter)
             * 2) Check if the variable is declared globally
             * 3) Finally if both 1 and 2 don't hold, create a new local
             *)
            (
              let f_env = (StringMap.find scope env.fxn_envs)
              in
                if (StringMap.mem var f_env.local_env)
                then 
                  f_env.local_env <- (StringMap.add var ev f_env.local_env)
                else 
                  if (StringMap.mem var env.global_env) 
                  then 
                    env.global_env <- (StringMap.add var ev env.global_env)
                  else 
                    f_env.local_env <- StringMap.add var ev f_env.local_env
            )
          else 
              env.global_env <- (StringMap.add var ev env.global_env)
    | Ast.OutputC(var) ->
        ();
(*    | Ast.OutputF(var, oc) -> *)
(*
    | Ast.If(cond, stmt_lst) ->
        let eval_cond = expr env scope cond in
          if 
        let t_stmts = (List.iter (stmt env scope) stmt_lst)
        in 
          (expr env cond) @
          [Beq (1 + List.length t_stmts)] @
          t_stmts
    | Ast.If_else(cond, stmt_lst1, stmt_lst2) ->
        let t_stmts = (List.concat (List.map (stmt env scope) stmt_lst1))
        and f_stmts = (List.concat (List.map (stmt env scope) stmt_lst2))
        in 
          (expr env cond) @
          [Beq (2 + List.length t_stmts)] @
          t_stmts @
          [Bra (1 + List.length f_stmts)] @
          f_stmts
    | Ast.For(s1, e1, s2, stmt_lst) ->
        (* note: order of executing statements and evaluating expressions here
         * matters since the environment can be updated on each
         * execution/evaluation
         *)
        let s1' = (stmt env scope s1)
        and e1' = (expr env e1)
        and for_body_stmts = (List.concat (List.map (stmt env scope) stmt_lst)) @ (stmt env scope s2)
        in 
         let 
          for_body_length = (List.length for_body_stmts) in
           s1' @
           [Bra (1 + for_body_length)] @
           for_body_stmts @
           e1' @
           [Bne (-(for_body_length + List.length e1'))]
 *)
    | Ast.Return(e) ->
        let (v, typ) = expr env scope e 
        and fxn_env_lookup = StringMap.find scope env.fxn_envs
        in
          fxn_env_lookup.ret_type <- (v, typ);
(*
    | Ast.Include(str) -> 
        []
 *)
    
    
  (* 
   * Translates a function 
   *)  

  in let env = { 
    global_env    = StringMap.empty; 
    fxn_envs      = StringMap.empty; 
  } in

  let rec add_fxn accum_env = function
      [] -> accum_env
    | fxn_decl :: rest ->
        let f_env = 
          { 
            local_env  = StringMap.empty;
            ret_type   = (Sast.IntLiteral(0), Int); (* return 0 by default *)
            fxn_name   = fxn_decl.fname;
            fxn_params = fxn_decl.params;
            fxn_body   = fxn_decl.body;
          }
        in
          env.fxn_envs <- StringMap.add fxn_decl.fname f_env env.fxn_envs;
          add_fxn env rest
  in
  (* add func_decls to env.fxn_envs *) 
  let env = add_fxn env func_decls
  in
    (* execute the global statements *)
    List.iter (stmt env "*global*") stmt_lst;
    (* return the ast program unchanged for now - need to return sast.program
     * later *)
    (stmt_lst, func_decls)
