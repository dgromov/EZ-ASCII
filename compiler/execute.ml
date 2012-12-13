(* FILENAME :  execute.ml
 * AUTHOR(S):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720) 
 * PURPOSE  :  Execute bytecode returned from Compile.translate
 *)

open Ast
open Bytecode
open Hashtypes

(* stack type can be either Int (value) or Address (pointer) *)
type stack_t =
    IntValue of int
  | Address of int

(* returns int array array *)
let load_canvas fname = 
  let ic = open_in fname in 
    let n = in_channel_length ic in
      let s = String.create n in 
        really_input ic s 0 n; 
        close_in ic;
    let lines = Str.split (Str.regexp("\n")) s  in
    let canvas = Array.make_matrix (List.length lines) (String.length (List.hd lines)) 0 in
      let x = ref 0 in  
      List.iter (
                  fun line -> 
                    let row = Str.split (Str.regexp(" ")) line in 
                      canvas.(!x) <- (Array.of_list (List.map int_of_string row)); 
                      x := !x+1
                ) lines; 
    (canvas);;

let execute_prog prog debug_flag =
  let stack = Array.make 1024 (IntValue 0)
  and globals = Array.make prog.num_globals (IntValue 0) 
  (*and canv_env = Array.make 100 [] *)
  and debug s =
    if debug_flag then print_endline s
  in 
    debug ("DEBUG: num_globals is " ^ string_of_int prog.num_globals);
    try
      let rec exec fp sp pc = match prog.text.(pc) with 
          Lit i -> 
            stack.(sp) <- IntValue i; 
            debug ("DEBUG: Lit " ^ string_of_int i);
            exec fp (sp+1) (pc+1)
        | Lct i ->
            stack.(sp) <- Address i;
            debug ("DEBUG: Lct " ^ string_of_int i);
            exec fp (sp+1) (pc+1)
        | Drp -> 
            debug ("DEBUG: Drp ");
            exec fp (sp-1) (pc+1)
        | Bin op ->
            debug ("DEBUG: Bin ");
            let op1 = 
              (match stack.(sp-2) with
                   IntValue(i)     -> Hashtypes.Int(i)
                 | Address(i) -> (Hashtbl.find prog.glob_hash i) (* add error handling *)
              )
            and op2 = 
              (match stack.(sp-1) with
                   IntValue(i)     -> Hashtypes.Int(i)
                 | Address(i) -> (Hashtbl.find prog.glob_hash i) (* add error handling *)
              )
            in 
              (stack.(sp-2) <- 
               (let boolean b = if b then 1 else 0 
                and bool_of_int i = if i > 0 then true else false
                in 
                  (match (op1, op2) with
                       (Hashtypes.Int(i), Hashtypes.Int(j)) ->
                         (match op with
                              Plus ->  
                                debug("Bin +: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (i + j)
                            | Minus ->
                                debug("Bin -: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (i - j)
                            | Times ->
                                debug("Bin *: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (i * j)
                            | Divide ->
                                debug("Bin /: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (i / j)
                            | Mod ->
                                debug("Bin mod: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (i mod j)
                            | Eq ->
                                debug("Bin eq: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean (i = j))
                            | Neq        -> 
                                debug("Bin neq: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean (i != j))
                            | Lt         -> 
                                debug("Bin <: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean (i < j))
                            | Gt         -> 
                                debug("Bin >: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean (i > j))
                            | Leq        -> 
                                debug("Bin <=: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean (i <= j))
                            | Geq        -> 
                                debug("Bin >=: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean (i >= j))
                            | Or         -> 
                                debug("Bin ||: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean ((bool_of_int i) || (bool_of_int j)))
                            | And        -> 
                                debug("Bin &&: i=" ^ string_of_int i ^ " j=" ^ string_of_int j);
                                IntValue (boolean ((bool_of_int i) && (bool_of_int j)))
                            | Mask       -> IntValue 1 (* need to do *)  
                         )
                     | (Hashtypes.Bool(b1), Hashtypes.Bool(b2)) ->
                         (match op with
                              Eq ->
                                debug("Bin eq: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2);
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 = b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | Neq -> 
                                debug("Bin neq: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2);
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 != b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | Or -> 
                                debug("Bin ||: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2);
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 || b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | And -> 
                                debug("Bin &&: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2);
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 && b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | _ ->
                                raise (Failure ("Binop not supported for boolean types."))
                         )
                     | (Hashtypes.String(s1), Hashtypes.String(s2)) ->
                         (match op with
                              Plus ->
                                (* + operator for string operands is a
                                 * concatenation *)
                                debug("Bin +: string1=" ^ s1 ^ " string2=" ^ s2);
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.String (s1 ^ s2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter) + 1;
                                  ret_val
                            | _ ->
                                raise (Failure ("Binop not supported for string types."))
                         )
                     | (_, _) ->
                         raise (Failure ("Binop not supported with input operand types."))
                  ))); 
                  exec fp (sp-1) (pc+1)
        | Lod i -> 
            stack.(sp) <- globals.(i); 
            debug ("DEBUG: Lod " ^ string_of_int i ^ " Global=" ^ 
                   (match globals.(i) with
                        IntValue(j) -> "Int value " ^ string_of_int j
                      | Address(j) -> "Pointer to address " ^ string_of_int j 
                   ));
            exec fp (sp+1) (pc+1) 
        | Str i ->
            (match (stack.(sp-1), globals.(i)) with
                 Address(j), Address(k) ->
                   if j != k then
                     (* if assigning a different pointer to a hash pair, no
                      * longer need the old hash pair, so remove it *)
                     Hashtbl.remove prog.glob_hash k;
                   globals.(i) <- stack.(sp-1)
               | _ ->
                   globals.(i) <- stack.(sp-1)); 
            debug ("DEBUG: Str " ^ string_of_int i); 
            exec fp sp (pc+1) 
        | Lfp i -> 
            (match (stack.(fp+i), stack.(sp)) with
                 Address(j), Address(k) ->
                   if j != k then
                     (* if assigning a different pointer to a hash pair, no
                      * longer need the old hash pair, so remove it *)
                     Hashtbl.remove prog.glob_hash k;
                   stack.(sp) <- stack.(fp+i)
               | _ ->
                   stack.(sp) <- stack.(fp+i));
            debug ("DEBUG: Lfp " ^ string_of_int i);
            exec fp (sp+1) (pc+1) 
        | Sfp i -> 
            stack.(fp+i) <- stack.(sp-1); 
            debug ("DEBUG: Sfp " ^ string_of_int i);
            exec fp sp (pc+1) 
        (* here Jsr -1, refers to OutputC functionality *)
        | Jsr(-1) ->
            debug ("Jsr -1");
            let lookup =
              (match stack.(sp-1) with
                   IntValue(i)     -> Hashtypes.Int(i)
                 | Address(i) -> (Hashtbl.find prog.glob_hash i) (* add error handling *)
              ) in 
            print_endline (Hashtypes.string_of_ct lookup);
            exec fp sp (pc+1)
        | Jsr(-2) ->
            (* CANVAS LOADING *)
            debug ("Jsr -2");
            let gran = stack.(sp-1)
            and path = 
              (match stack.(sp-2) with
                   Address(j) ->
                     match (Hashtbl.find prog.glob_hash j) with
                        Hashtypes.String(s) -> s
                 | _ ->
                     raise (Failure("Jsr -2 expected address for file path string but got IntValue."))
              )
            in
(*
            let a = (load_canvas path) in
              print_endline(Hashtypes.string_of_canvas a (Hashtypes.make_map ['.'; '-'; '+'; 'X'; '@']) true); 
 *)
              Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Canvas (load_canvas path));
              let ret_val = Address !(prog.glob_hash_counter) in
                prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                stack.(sp-1) <- ret_val;
                exec fp sp (pc+1)
        | Jsr i -> 
            stack.(sp) <- IntValue (pc + 1); 
            debug ("DEBUG: Jsr " ^ string_of_int i);
            exec fp (sp+1) i
        | Ent i -> 
            stack.(sp) <- IntValue (fp); 
            debug ("DEBUG: Ent " ^ string_of_int i);
            exec sp (sp+i+1) (pc+1) 
        | Rts i ->
            let new_fp = match stack.(fp) with
                IntValue(k) -> k
              | Address(k) -> raise (Failure("Rts expected an integer for new frame pointer but got an address."))
            and new_pc = match stack.(fp-1) with
                IntValue(k) -> k
              | Address(k) -> raise (Failure("Rts expected an integer for new program counter but got an address.")) 
            in
              stack.(fp-i-1) <- stack.(sp-1);
              debug ("DEBUG: Rts " ^ string_of_int i);
              exec new_fp (fp-i) new_pc 
        | Beq i -> 
            debug ("DEBUG: Beq " ^ string_of_int i);
            exec fp (sp-1)
              (pc + 
               if (match stack.(sp-1) with
                       IntValue(k) -> (k = 0)
                     | Address(k) -> 
                         (match (Hashtbl.find prog.glob_hash k) with
                              Hashtypes.Bool(b) -> not b
                            | _ -> raise(Failure("Beq operation: Address lookup resulted in a non-boolean type."))
                         )) 
               then i 
               else 1) 
        | Bne i -> 
            debug ("DEBUG: Bne " ^ string_of_int i);
            exec fp (sp-1)
              (pc + 
               if (match stack.(sp-1) with
                       IntValue(k) -> (k != 0)
                     | Address(k) -> 
                         (match (Hashtbl.find prog.glob_hash k) with
                              Hashtypes.Bool(b) -> b
                            | _ -> raise(Failure("Bne operation: Address lookup resulted in a non-boolean type."))
                         )) 
               then i 
               else 1) 
        | Bra i -> 
            debug ("DEBUG: Bra " ^ string_of_int i);
            exec fp sp (pc+i)
        | Hlt -> ()
      in exec 0 0 0 
    with e -> (* catch all exceptions *)
      Printf.eprintf "Unexpected exception: %s\n" (Printexc.to_string e);

