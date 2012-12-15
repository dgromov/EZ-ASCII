(* FILENAME :  compiler.ml
 * AUTHOR(S):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720)
 * PURPOSE  :  Translate abstract syntax tree to bytecode.
 *)

open Ast
open Bytecode
(*open Ezatypes*)
open Hashtypes
open Canvas 

(* global hash table
 * keys are absolute integer addresses
 * values are of type Hashtypes.ct *)
let glob_ht = Hashtbl.create 2048

(* initialize hash counter to keep track of next available key *)
let hash_counter = ref 0;

module StringMap = Map.Make(String)

(* Translation environment *)
type env = {
  function_idx          : int StringMap.t;   (* Index for each function *)
  mutable global_idx    : int StringMap.t;   (* "Address" for global vars *)
  mutable local_idx     : int StringMap.t;   (* FP offset for args, locals *)
  num_formals           : int;               (* Number of parameters *)
}

(* enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* string_map_pairs : StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

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
    (* reserve -1 for printing *)
    (* reserve -2 for printing to file *)
    in (bif_helper StringMap.empty (-3)) ["load"; "blank"; "shift"]
  in  

  let function_indexes = string_map_pairs built_in_functions
                           (* start built-in functions at 2, reserve 1 for
                            * top-level statements pseudofunction *)
                           (enum 1 2 (List.map (fun f -> f.fname) func_decls)) in

  (* Translate an expr *)
  let rec expr env = function
      Ast.IntLiteral(i) -> [Lit i]
    | Ast.StrLiteral(s) -> 
        Hashtbl.add glob_ht !hash_counter (Hashtypes.String s);
        let ret_val = [Lct !hash_counter] in
          hash_counter := !hash_counter+1; (* incr value of hash_counter ref *)
          ret_val
    | Ast.BoolLiteral(b) -> 
        Hashtbl.add glob_ht !hash_counter (Hashtypes.Bool b);
        let ret_val = [Lct !hash_counter] in
          hash_counter := !hash_counter+1; (* incr hash_counter in-place *)
          ret_val
    | Ast.Id(s) ->
        (try 
           let search_local = (StringMap.find s env.local_idx) in
             [Lfp search_local]
         with Not_found -> 
           (try
              let search_global = StringMap.find s env.global_idx in
                [Lod search_global]
            with Not_found ->
              raise (Failure ("Undeclared variable " ^ s))))
    | Ast.Binop(e1, op, e2) ->
        let ev1 = (expr env) e1
        and ev2 = (expr env) e2 in
        ev1 @ ev2 @ [Bin op]

    | Ast.Call(fname, actuals) ->
        (try
           (* first evaluate the actuals *)
           let res = (List.map (expr env) (List.rev actuals)) 
           in
             (List.concat res) @ [Jsr (StringMap.find fname env.function_idx)]
         with Not_found ->
           raise (Failure ("Undefined function: " ^ fname)))
    | Ast.Load(filepath_expr, gran_expr) ->
        let ev1_val = (expr env) filepath_expr
        and ev2_val = (expr env) gran_expr in
          ev1_val @ ev2_val @ [Jsr (-3)]
    
    | Ast.Blank(height, width, granularity) -> 
        let ev1_val = (expr env) height
        and ev2_val = (expr env) width 
        and ev3_val = (expr env) granularity in 
          ev1_val @ ev2_val @ ev3_val @ [Jsr (-4)]

    | Ast.Select_Point (x, y) -> 
        let ev1_val = (expr env) x 
        and ev2_val = (expr env) y in 
          ev1_val @ ev2_val @ [Lit (Canvas.select_type (Canvas.POINT))]

    | Ast.Select_Rect (x1, x2, y1, y2) -> 
        let ev1_val = (expr env) x1
        and ev2_val = (expr env) x2
        and ev3_val = (expr env) y1
        and ev4_val = (expr env) y2 in 
          ev1_val @ ev2_val  @ ev3_val @ ev4_val  @  [Lit (Canvas.select_type (Canvas.RECT))]

    | Ast.Select_VSlice (x1, y1, y2)  -> 
        let ev1_val = (expr env) x1
        and ev2_val = (expr env) y1
        and ev3_val = (expr env) y2 in 
          ev1_val @ ev2_val  @ ev3_val @  [Lit (Canvas.select_type (Canvas.VSLICE))]

    | Ast.Select_HSlice (x1, x2, y1) -> 
        let ev1_val = (expr env) x1
        and ev2_val = (expr env) x2
        and ev3_val = (expr env) y1 in
          ev1_val @ ev2_val  @ ev3_val @ [Lit (Canvas.select_type (Canvas.HSLICE))] 

    | Ast.Select_VSliceAll x ->
        let ev1_val = (expr env) x in 
          ev1_val @ [Lit (Canvas.select_type (Canvas.VSLICE_ALL))]  
    
    | Ast.Select_HSliceAll y -> 
        let ev1_val = (expr env) y in 
          ev1_val @ [Lit (Canvas.select_type (Canvas.HSLICE_ALL))] 

    | Ast.Select_All -> 
       [Lit (Canvas.select_type (Canvas.ALL))] 
   
    | Ast.Select (canv, selection) ->
        let ev1_val = (expr env) canv
        in 
          (expr env) selection @ ev1_val @ [Jsr (-6)] 
    | Ast.Select_Binop(op, e) -> [Lit 1]
    | Ast.Select_Bool(e) -> [Lit 1]
    | Ast.Shift(canv, dir, count) ->
        let canv_val = (expr env) canv
        and dir_val = (expr env) dir 
        and count_val = (expr env) count in 
        count_val @ dir_val  @ canv_val @[Jsr (-5)]

    | Ast.GetAttr(canv, attr) -> 
        let canv_val = (expr env) canv 
        in
          canv_val @ [CAtr attr]
  (* *) 
  and  stmt env scope = function
      (* need to update assign later *)
      Ast.Assign(var, e) -> 
        let ev = (expr env e) in
          ev @
          if scope = "*local*"
          then 
            (*
             * if we are in a function, variable lookup proceeds as:
             * 1) Check if the variable is a formal (parameter)
             * 2) Check if the variable is declared globally
             * 3) Finally if both 1 and 2 don't hold, create a new local
             *)
            if (StringMap.mem var env.local_idx) 
            then 
              let exis_local_idx = StringMap.find var env.local_idx in
                (* side effect: update env.local_idx *)
                env.local_idx <- (StringMap.add var exis_local_idx env.local_idx);
                [Sfp exis_local_idx] 
            else 
              if (StringMap.mem var env.global_idx) 
                then 
                let exis_global_idx = StringMap.find var env.global_idx in
                  (* side effect: update env.global_idx *)
                  env.global_idx <- (StringMap.add var exis_global_idx env.global_idx);
                  [Str exis_global_idx]
              else 
                (* note the +1 for the next available local idx *)
                let new_local_idx = (List.length (StringMap.bindings env.local_idx)) + 1
                in 
                  (* side effect: modify env.local_idx *)
                  env.local_idx <- (StringMap.add var new_local_idx env.local_idx);
                  [Sfp new_local_idx] 
          else 
            [Str
               (if (StringMap.mem var env.global_idx) 
                then 
                  let exis_global_idx = StringMap.find var env.global_idx in
                    env.global_idx <- (StringMap.add var exis_global_idx env.global_idx);
                    exis_global_idx 
                    else 
                      let new_global_idx = (List.length (StringMap.bindings env.global_idx))
                      in 
                        (* side effect: modify env.global_idx *)
                        env.global_idx <- (StringMap.add var new_global_idx env.global_idx);
                        new_global_idx)] 


    | Ast.OutputC(var, rend) ->
        let var_val = (expr env var) in
        let rend_val = (expr env rend) in 
        rend_val @ var_val @ [Jsr (-1)]

    | Ast.OutputF(var, fn, rend) ->
        let var_val = (expr env var) in 
        let fn_val = (expr env fn) in 
        let rend_val = (expr env rend) in
        rend_val @ fn_val @ var_val @ [Jsr (-2)]

    | Ast.If(cond, stmt_lst) ->
        let t_stmts = (List.concat (List.map (stmt env scope) stmt_lst))
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
    | Ast.Return(e) ->
        (expr env e) @ [Rts env.num_formals] 
    | Ast.CanSet(can, select_exp, inten)->
        let int_exp = (expr env inten) 
        and sel_exp = (expr env (Ast.Select(can, select_exp)))  in 
        int_exp @ sel_exp @ [Jsr (-7)]
    | Ast.Include(str) -> 
        []
    
    
  (* 
   * Translates a function 
   *)  
  in let translate env fdecl = 
    (* Bookkeeping: FP offsets for locals and args *)
    let num_formals = List.length fdecl.params

    (* we don't currently have locals...*)
    and num_locals = 0 (* List.length *)

    and formal_offsets = (enum (-1) (-2) fdecl.params) 
    in
    let formal_offsets' = (List.map (fun (i, s) -> (i, s)) formal_offsets)
    in
    let env = { env with local_idx = string_map_pairs StringMap.empty formal_offsets';
                         num_formals = num_formals } 

    in 
      [Ent num_locals] @                                   (* Entry: allocate space for locals *) 
       (List.concat (List.map (stmt env "*local*") fdecl.body)) @           (* Body *) 
       [Lit 0; Rts num_formals]                             (* Default - return 0 *)

  in let env = { 
    function_idx  = function_indexes;
    global_idx    = StringMap.empty; (* global_indexes; *)
    local_idx     = StringMap.empty; 
    num_formals   = 0
  } in

  (* Compile the global statement list *)
  let glob_stmts = (List.concat (List.map (stmt env "*global*") stmt_lst)) in
  let main_func_call = 
    try
      [Jsr (StringMap.find "main" function_indexes)]
    with Not_found -> []
  in
  (* Compile the functions, and prepend compiled global statements and Hlt *)
  let func_bodies = (glob_stmts @ main_func_call) :: [Hlt] :: List.map (translate env) func_decls in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
                               (fun (l, i) f -> (i :: l, (i + List.length f))) 
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
                  (List.concat func_bodies));
      glob_hash = glob_ht;
      glob_hash_counter = hash_counter;
    }

