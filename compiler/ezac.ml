open Parser
open Scanner
open Ast

module NameMap = 
  Map.Make(struct
             type t = string
             let compare x y = Pervasives.compare x y
           end)

exception ReturnException of int * int NameMap.t


(* given a NameMap, print its bindings
 *)
let env_to_str m =
  let bindings = NameMap.bindings m in
  let rec print_map_helper s = function
      [] -> s
    | hd :: tl -> print_map_helper ((fst hd) ^ " = " ^ (string_of_int (snd hd)) ^ "\n" ^ s) tl
  in print_map_helper "" bindings

(* helper function *)
let bool_of_int i =
  if i > 0 then true
  else false


(* =====================================================
 * eval function
 *
 * Takes Ast.expr type and evaluates it, returning a
 * string value and updated environment pair.
 * ===================================================== *)
let rec eval env scope = function
    IntLiteral(e1)      -> string_of_int e1, env
  | StrLiteral(e1)      -> e1, env
  | BoolLiteral(e1)     -> 
      if e1 then 
        "1", env
      else 
        "0", env
  | Id(var)             -> 
      let local_decls = (NameMap.find scope env)
      in
        (* Look for the variable in the function's local scope *)
        if NameMap.mem var local_decls
        then (NameMap.find var local_decls), env
        (* If variable not found in local scope, look in global scope *)
        else 
          let global_decls = (NameMap.find "*global*" env)
          in
            if NameMap.mem var global_decls
            then (NameMap.find var global_decls), env
            (* Otherwise, error. *)                                                   
            else raise (Failure (">>> Undefined identifier: " ^ var))
  | Binop(e1, op, e2)   -> 
      let v1, env = eval env scope e1 in
      let v2, env = eval env scope e2 in
      let boolean i = if i then 1 else 0 in
        string_of_int (
          match op with
              Plus   -> (int_of_string v1) + (int_of_string v2)
            | Minus  -> (int_of_string v1) - (int_of_string v2)
            | Times  -> (int_of_string v1) * (int_of_string v2)
            | Divide -> (int_of_string v1) / (int_of_string v2)
            | Mod    -> (int_of_string v1) mod (int_of_string v2)
            | Eq     -> boolean((int_of_string v1) == (int_of_string v2))
            | Neq    -> boolean((int_of_string v1) != (int_of_string v2))
            | Lt     -> boolean((int_of_string v1) <  (int_of_string v2))
            | Gt     -> boolean((int_of_string v1) >  (int_of_string v2))
            | Leq    -> boolean((int_of_string v1) <= (int_of_string v2))
            | Geq    -> boolean((int_of_string v1) >= (int_of_string v2))
            | Or     -> boolean((bool_of_int (int_of_string v1)) || (bool_of_int (int_of_string v2)))
            | And    -> boolean((bool_of_int (int_of_string v1)) && (bool_of_int (int_of_string v2)))
            | Mask   -> 1 (* NEED TO DO *)
        ), env


(* =====================================================
 * exec function
 *
 * Takes Ast.stmt type and executes it, returning an 
 * updated environment.
 * ===================================================== *)
let rec exec (env, scope, fxnlist) = function
    Assign(var, e) ->
      (* update the environment for the expression first *)
      let e_val, e_env = eval env scope e in
        (* print_endline (">>> " ^ var ^ " assigned " ^ e_val); *)
      let updated_submap = (NameMap.add var e_val (NameMap.find scope e_env))
      in (NameMap.add scope updated_submap e_env), scope, fxnlist
  | OutputC(var) ->
      let e_val, e_env = eval env scope var 
      in 
        (*print_endline(e_val);*)
        
        Printf.printf "%s\n" (Scanf.unescaped e_val);
        e_env, scope, fxnlist;
  | OutputF(s) ->
      (* No-op, NEED TO DO) *)
      env, scope, fxnlist;
  | If(cond, stmt_lst) ->
      let c1, c_env = eval env scope cond in
        if (bool_of_int (int_of_string c1)) then
          List.fold_left (exec) (c_env, scope, fxnlist) stmt_lst
        else env, scope, fxnlist
  | If_else(cond, stmt_lst1, stmt_lst2) ->
      let c1, c_env = eval env scope cond in
        if (bool_of_int (int_of_string c1)) then
          List.fold_left (exec) (c_env, scope, fxnlist) stmt_lst1
        else
          List.fold_left (exec) (c_env, scope, fxnlist) stmt_lst2
  | For(s1, e1, s2, stmt_lst) ->
      let (env, scope, fxnlist) = (exec (env, scope, fxnlist)) s1 in 
      let rec loop (env, scope, fxnlist) =
        let v, env = eval env scope e1 in
          if (bool_of_int (int_of_string v)) then
            let (body_env, body_scope, fxnlist) = List.fold_left (exec) (env, scope, fxnlist) stmt_lst in
              loop (exec (body_env, body_scope, fxnlist) s2)
          else env, scope, fxnlist
      in loop (env, scope, fxnlist)
  | Return(exp) ->
      (* No-op, NEED TO DO) *)
      env, scope, fxnlist
  | Call(fxn_name, param_exprs)   ->
      (* Find the target function from the list of function declarations.
       * Target function is as defined in the Ast:
       *   { 
       *     fname :  string; 
       *     params : string list;
       *     body :   stmt list;
       *   }                                      
       *   
       * Note: Need to add error handling for when user tries to call
       * an undefined function. *)
      let target_fxn = List.find (fun s -> s.fname = fxn_name) fxnlist
      in
      (* Initialize the function environment with the function
       * declaration's parameter list (keys), with empty values.
       *
       * Note: This should be moved into the main loop code before 
       * executing any statements *)
      let fxn_env = (List.fold_left 
                       (fun tmp_env param_id -> NameMap.add param_id "" tmp_env)
                       (NameMap.find fxn_name env) 
                       target_fxn.params)
      in
      (* For every parameter in the function environment,
       * initialize it to the VALUE of the parameter expression
       * that the user passed in.
       * (e.g. if the function declaration is foo(a, b, c),
       * and the user calls with foo(1+2, 3+4, 5+6),
       * evaluate each parameter expression and update the
       * function environment accordingly. 
       * 
       * Note: error handling needs to be added for the case where
       * the user supplies the incorrect number of arguments. *)
      let rec setparams fxn_env'  = function
          [] -> fxn_env' 
        | hd :: tail ->
            (* make sure we evaluate the parameter
             * expressions in their CURRENT environments *)
            let (param_expr_val, _) = eval env scope (snd hd)
            in 
            setparams (NameMap.add (fst hd) param_expr_val fxn_env') tail
      in let fxn_env = 
        setparams fxn_env (List.combine target_fxn.params param_exprs)
      in
      (* Update the global env with the fxn_env 
       * before executing the function body. *) 
      let update_env = (NameMap.add target_fxn.fname fxn_env env) in
        List.fold_left (exec) 
          (update_env, target_fxn.fname, fxnlist) 
          target_fxn.body


(* =====================================================
 * Top-level function
 *
 *
 * ===================================================== *)
let _ =
  if Array.length Sys.argv < 2 then
    raise (Failure ("File path not provided."))
  else
    let lexbuf = Lexing.from_channel(open_in Sys.argv.(1))
    in
    let rec parse_stmts env scope fxns_lst = function
        [] -> env 
      | hd :: tail -> 
          (*      try  *)
          (* Put function declarations in a symbol table *)
          (*        let func_decls = List.fold_left
           (fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
           NameMap.empty funcs 
           in*)
          (*        let rec call fdecl actuals globals =  *)
          (* eval: evaluates expressions and returns (value, updated env) *)

          (*
           | Call("print", [e]) ->
           let v, env = eval env e in
           print_endline (string_of_int v);
           0, env  
           | Call(f, actuals) ->
           let fdecl = 
           try NameMap.find f func_decls
           with Not_found -> raise (Failure ("undefined function " ^ f))
           in
           let ractuals, env = List.fold_left
           (fun (actuals, env) actual ->
           let v, env = eval env actual in v :: actuals, env)
           ([], env) actuals
           in
           let (locals, globals) = env in
           try
           let globals = call fdecl (List.rev ractuals) globals
           in 0, (locals, globals)
           with ReturnException(v, globals) -> v, (locals, globals)							
           in *)
          (* execute statements and return updated environments *)                               
          let (updated_env, scope, fxns_list) = exec (env, scope, fxns_lst) hd 
          in
            (parse_stmts updated_env scope fxns_lst) tail
    in
      try
        let (stmt_lst, fxns_lst) = (Parser.program Scanner.token lexbuf) in
        (* Add every function name to our initial env *)
        let init_env = 
          (List.fold_left
             (fun new_env fxn_decl -> 
                NameMap.add fxn_decl.fname NameMap.empty new_env)
             (NameMap.add "*global*" NameMap.empty NameMap.empty) 
             fxns_lst) in
          (parse_stmts init_env "*global*" fxns_lst) stmt_lst;
      with 
        | Failure(s) -> 
            print_endline s;
            exit 0;
        | Parsing.Parse_error ->
            let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbuf in 
              print_endline (">>> Parse error at line " ^ (string_of_int line) ^ ", character " ^ (string_of_int cnum) ^ ": '" ^ tok ^ "'");
              exit 0;


