(* FILENAME :  ssanalyzer.ml
 * AUTHOR(s):  Joe Lee (jyl2157)
 * PURPOSE  :  Checks for type errors, undefined var/fxn errors, converts ast to sast.  
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
    Void -> "Void"
  | Int -> "Int"
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

exception TypeException of Ast.expr * Ast.expr * t * t
exception UndefinedVarException of Ast.expr
exception UndefinedFxnException of string * Ast.expr

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
                raise (UndefinedVarException(Ast.Id(s)))))
        else
          (try
             let search_global = StringMap.find s env.global_env in
               search_global
           with Not_found ->
             raise (UndefinedVarException (Ast.Id(s))))
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
                        raise(TypeException(e2, Ast.Binop(e1, op, e2), t1, t2))
                 )
             | Ast.Minus | Ast.Times | Ast.Divide | Ast.Mod ->
                 (match (t1, t2) with
                      (Int, Int) ->
                        Sast.Binop(e1, op, e2), Int
                    | (_, _) ->
                        raise(TypeException(e2, Ast.Binop(e1, op, e2), Int, t2))
                 )
             | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq ->
                 (match (t1, t2) with
                      (Int, Int) ->
                        Sast.Binop(e1, op, e2), Bool 
                    | (_, _) ->
                        raise(TypeException(e2, Ast.Binop(e1, op, e2), Int, t2))
                 )
             | Ast.Or | Ast.And ->
                 (match (t1, t2) with
                      (Bool, Bool) ->
                        Sast.Binop(e1, op, e2), Bool 
                    | (_, _) ->
                        raise(TypeException(e2, Ast.Binop(e1, op, e2), Bool, t2))
                 )
             | Ast.Mask -> 
                 Sast.Binop(e1, op, e2), Canvas (* need to do *)  
          );
    | Ast.Call(fname, actuals) ->
        (* no need to execute recursive calls *)
        if (scope <> "*global*") && (fname = scope)
        then
         (try 
            let fxn_env_lookup = (StringMap.find fname env.fxn_envs) in
              fxn_env_lookup.ret_type
          with Not_found ->
             raise (UndefinedFxnException (fname, Ast.Call(fname, actuals))))
        else
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
               let _ = List.map (stmt env fxn_env_lookup.fxn_name) fxn_env_lookup.fxn_body
               in 
                 (* finally, return the possibly updated return type
                  *  (it is already initialized to (IntLiteral(0), Int)) *)
                 fxn_env_lookup.ret_type
           with Not_found ->
             raise (UndefinedFxnException (fname, Ast.Call(fname, actuals))))
    | Ast.Load(filepath_expr, gran_expr) ->
        let (v1, t1) = (expr env scope) filepath_expr
        and (v2, t2) = (expr env scope) gran_expr in
          if not (t1 = String)
          then 
            raise(TypeException(filepath_expr, Ast.Load(filepath_expr, gran_expr), String, t1))
          else 
            if not (t2 = Int)
            then
              raise(TypeException(gran_expr, Ast.Load(filepath_expr, gran_expr), Int, t2))
            else 
              Sast.Load(filepath_expr, gran_expr), Canvas
    | Ast.Blank(height, width, granularity) ->
        let (v1, t1) = (expr env scope) height
        and (v2, t2) = (expr env scope) width
        and (v3, t3) = (expr env scope) granularity 
        in
          (match (t1, t2, t3) with
               (Int, Int, Int) ->
                 Sast.Canvas(height, width, granularity), Canvas
             | (_, Int, Int) ->
                 raise(TypeException(height, Ast.Blank(height, width, granularity), Int, t1))
             | (Int, _, Int) ->
                 raise(TypeException(width, Ast.Blank(height, width, granularity), Int, t2))
             | (Int, Int, _) ->
                 raise(TypeException(granularity, Ast.Blank(height, width, granularity), Int, t3))
             | (_, _, _) ->
                 raise(TypeException(height, Ast.Blank(height, width, granularity), Int, t1))
          )
    | Ast.Select_Point (x, y) -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_Rect (x1, x2, y1, y2) -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_VSlice (x1, y1, y2)  -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_HSlice (x1, x2, y1) ->
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_VSliceAll x ->
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_HSliceAll y ->
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_All -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Select (canv, selection) -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_Binop(op, e) -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Select_Bool(e) -> 
        Sast.IntLiteral(1), Canvas 
    | Ast.Shift(canv, dir, count) ->
        Sast.IntLiteral(1), Canvas 
  
  (* execute statement *)                              
  and stmt env scope = function
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
        (); (* nothing to check *)
    | Ast.OutputF(var, oc) ->
        (); (* nothing to check *)
    | Ast.If(cond, stmt_lst) ->
        let (cond_val, cond_typ) = expr env scope cond in
          (match cond_typ with
               Bool ->
                 ();
             | _ ->
                 raise(TypeException(cond, cond, Bool, cond_typ))
          );
          (* regardless of the condition, check the statements *)
          List.iter (stmt env scope) stmt_lst;
          ();
    | Ast.If_else(cond, stmt_lst1, stmt_lst2) ->
        let (cond_val, cond_typ) = expr env scope cond in
          (match cond_typ with
               Bool -> ();
             | _ -> raise(TypeException(cond, cond, Bool, cond_typ))
          );
          (* regardless of the condition, check both blocks *)
          List.iter (stmt env scope) stmt_lst1;
          List.iter (stmt env scope) stmt_lst2;
          ();
    | Ast.For(s1, e1, s2, stmt_lst) ->
        (stmt env scope s1);
        let (e1_val, e1_typ) = (expr env scope e1)
        in 
          (match e1_typ with
               Bool -> ();
             | _ -> raise(TypeException(e1, e1, Bool, e1_typ))
          );
          (* we only need to check the statement body once *)
          List.iter (stmt env scope) stmt_lst;
          stmt env scope s2;
          ();
    | Ast.Return(e) ->
        let (v, typ) = expr env scope e 
        and fxn_env_lookup = StringMap.find scope env.fxn_envs
        in
          fxn_env_lookup.ret_type <- (v, typ);
    | Ast.Include(str) -> 
        (); (* no type checking needed since we know it's already a string *)
  
  (************************ 
   * start main code here 
   ************************)  
  
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
