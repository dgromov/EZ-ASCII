(* FILENAME :  execute.ml
 * AUTHOR(S):  Joe Lee (jyl2157), Dmitriy Gromov (dg2720) 
 * PURPOSE  :  Execute bytecode returned from Compile.translate
 *)

open Ast
open Bytecode
open Hashtypes
open Canvas

(* stack type can be either Int (value) or Address (pointer) *)
type stack_t =
    IntValue of int
  | Address of int


let execute_prog prog debug_flag =
  (* wrapper functions around extracting types from the stack *)
  let pop_int = function
      IntValue(i) -> i 
    | Address(i)  -> raise (Failure ("Expected an int but popped an address."))
  and pop_address_val = function
      IntValue(i) -> raise (Failure ("Expected an address but popped an int."))    
    | Address(i) -> (Hashtbl.find prog.glob_hash i) 
  and stack = Array.make 1024 (IntValue 0)
  and globals = Array.make prog.num_globals (IntValue 0) 
  (*and canv_env = Array.make 100 [] *)
  and debug s =
    if debug_flag then print_endline s
  in 
    debug ("DEBUG: num_globals is " ^ string_of_int prog.num_globals);
    try
      let rec exec fp sp pc = 
        debug ("DEBUG: fp=" ^ (string_of_int fp) ^ ", sp=" ^ (string_of_int sp) ^ ", pc=" ^ (string_of_int pc) ^ ":  ");
        match prog.text.(pc) with 
          Lit i -> 
            stack.(sp) <- IntValue i; 
            debug ("Lit " ^ string_of_int i);
            exec fp (sp+1) (pc+1)
        | Lct i ->
            stack.(sp) <- Address i;
            debug ("Lct " ^ string_of_int i);
            exec fp (sp+1) (pc+1)
        | Drp -> 
            debug ("Drp ");
            exec fp (sp-1) (pc+1)
        | Bin op ->
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
                      | (Hashtypes.Canvas(c1), Hashtypes.Canvas(c2)) ->
                        ( match op with 
                          Mask -> 
                            debug("Canvas 1\n: " ^(Hashtypes.string_of_ct true (Hashtypes.Canvas(c1))   ));
                            debug("Canvas 1\n: " ^(Hashtypes.string_of_ct true (Hashtypes.Canvas(c2)) ));
                            Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Canvas (Canvas.mask c1 c2));
                            let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                         | _ ->
                              raise (Failure ("Binop not supported for canvas types."))
                        )

                (* ?     | (_, _) -> *)
                         (* raise (Failure ("Binop not supported with input operand types.")) *)
                  ))); 
                  exec fp (sp-1) (pc+1)
        | Lod i -> 
            stack.(sp) <- globals.(i); 
            debug ("Lod " ^ string_of_int i ^ " Global=" ^ 
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
            debug ("Str " ^ string_of_int i); 
            exec fp sp (pc+1) 
        | Lfp i -> 
            (match (stack.(fp+i), stack.(sp)) with
                 Address(j), Address(k) ->
                   if j != k then
                     (* if over-writing a local, remove hash for 
                      * the local being overwritten *)
                     (* if assigning a different pointer to a hash pair, no
                      * longer need the old hash pair, so remove it *)
                     Hashtbl.remove prog.glob_hash k;
                   stack.(sp) <- stack.(fp+i)
               | _ ->
                   stack.(sp) <- stack.(fp+i));
            debug ("Lfp " ^ string_of_int i);
            exec fp (sp+1) (pc+1) 
        | Sfp i -> 
            stack.(fp+i) <- stack.(sp-1); 
            debug ("Sfp " ^ string_of_int i);
            exec fp (sp+1) (pc+1) 
        (* here Jsr -1, refers to OutputC functionality *)
        | CAtr atr -> 
            debug ("CAtr "); 
            let canv_id = stack.(sp-1) in 
              let canv = (match pop_address_val canv_id with 
                Hashtypes.Canvas(c) -> c 
              | _ -> raise (Failure ("Catr needs to be given a canvas"))) in

            let result = 
              (
                match atr with
                  Ast.W -> Canvas.width canv 
                | Ast.H -> Canvas.height canv 
                | Ast.G -> Canvas.granularity canv 
              ) in 
            stack.(sp-1) <- IntValue result; 
            exec fp sp (pc+1)

        | Jsr(-1) ->
            debug ("Jsr -1");
            let lookup =
              (match stack.(sp-1) with
                   IntValue(i) -> Hashtypes.Int(i)
                 | Address(i) -> (Hashtbl.find prog.glob_hash i) (* add error handling *)
              ) in 
            let render = 
              (match stack.(sp-2) with 
                  IntValue(i) -> raise (Failure ("Render should be a boolean"))
                | Address(i) -> match (Hashtbl.find prog.glob_hash i) with 
                                Hashtypes.Bool(b) -> b (* add error handling *)
              ) in
            print_endline (Hashtypes.string_of_ct render lookup);
            exec fp sp (pc+1)
        | Jsr(-2) ->
            debug ("Jsr -2");
            let lookup =
              (match stack.(sp-1) with
                   IntValue(i) -> Hashtypes.Int(i)
                 | Address(i) -> (Hashtbl.find prog.glob_hash i) (* add error handling *)
              ) in 
            let filename = 
              ( match (pop_address_val stack.(sp-2)) with
                  Hashtypes.String(s) -> s
                | _ ->
                    raise (Failure("Jsr -2 expected a string filepath but got a different type.")) 
              ) in 
            let render = 
              (match stack.(sp-3) with 
                  IntValue(i) -> raise (Failure ("Render should be a boolean"))
                | Address(i) -> match (Hashtbl.find prog.glob_hash i) with 
                                Hashtypes.Bool(b) -> b (* add error handling *)
              ) in
           
            let oc = open_out filename in 
              output_string oc ( (Hashtypes.string_of_ct render lookup) ^ "\n" );
            exec fp sp (pc+1) 
        | Jsr(-3) ->
            (* CANVAS LOADING *)
            debug ("Jsr -2");
            let gran_val = pop_int(stack.(sp-1))
            and path = 
              match (pop_address_val stack.(sp-2)) with
                  Hashtypes.String(s) -> s
                | _ ->
                    raise (Failure("Jsr -2 expected a string filepath but got a different type."))
            in
            let granularity = string_of_int gran_val 
            in
              let filename_parts = Str.split (Str.regexp "/") path in 
                let filename = 
                  match Str.string_match (Str.regexp ".+.i")  (List.hd (List.rev filename_parts)) 0 with 
                    false -> 
                      debug ("Trying to open: " ^ "../tmp/" ^ List.hd (List.rev filename_parts) ^ ".i" ); 
                      let comm = "python util/load_img.py " ^ path ^ " " ^ granularity in 
                      Sys.command (comm);
                      "../tmp/" ^ List.hd (List.rev filename_parts) ^ ".i" 
                  | true -> 
                      debug ("Trying to open raw: " ^ path ); 
                      path
              in 
              Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) 
                (Hashtypes.Canvas (Canvas.load_canvas filename  gran_val));
              let ret_val = Address !(prog.glob_hash_counter) in
                prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                stack.(sp-1) <- ret_val;
                exec fp sp (pc+1)
        | Jsr(-4) ->
            (* BLANK *)
            debug ("Jsr -4"); 
            let h_val = (pop_int stack.(sp-3))
            and w_val = (pop_int stack.(sp-2))
            and g_val = (pop_int stack.(sp-1))
            in 
              Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) 
                (Hashtypes.Canvas (Canvas.blank h_val w_val g_val 0));
              let ret_val = Address !(prog.glob_hash_counter) in
                prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                stack.(sp-1) <- ret_val;
                exec fp sp (pc+1)
       | Jsr (-5) -> 
            (* SHIFT *)
            debug ("Jsr -5");
            exec fp sp (pc+1)
       | Jsr (-6) -> 
            (* SELECT *)
            debug ("Jsr -6: - Select Piece of Canvas");
            let existing = match (pop_address_val stack.(sp-1)) with
                Hashtypes.Canvas(c) -> c
              | _ -> raise(Failure("Jsr -6: Expected canvas type."))
            in 
            let sel_type = (pop_int stack.(sp-2)) in
            let selected = 
              (* This match should be on some sort of enum *)
              match sel_type with 
                  1 -> 
                    let x = pop_int stack.(sp-4) 
                    and y = pop_int stack.(sp-3) in
                    Canvas.select_point x y existing        
                | 2 -> 
                    let x1 = pop_int stack.(sp-6) 
                    and x2 = pop_int stack.(sp-5) 
                    and y1 = pop_int stack.(sp-4)
                    and y2 = pop_int stack.(sp-3) in
                    Canvas.select_rect x1 x2 y1 y2 existing 
                | 3 -> 
                    let x = pop_int stack.(sp-5)
                    and y1 = pop_int stack.(sp-4)
                    and y2 = pop_int stack.(sp-3) in
                    Canvas.select_hslice x y1 y2 existing 
                | 4 -> 
                    let x1 = pop_int stack.(sp-5)
                    and x2 = pop_int stack.(sp-4)
                    and y  = pop_int stack.(sp-3) in
                    Canvas.select_vslice x1 x2 y existing 
                | 5 -> 
                    let x = pop_int stack.(sp-3) in
                    Canvas.select_hslice_all x existing 
                | 6 -> 
                    let y = pop_int stack.(sp-3) in
                    Canvas.select_vslice_all y existing 
                | 7 -> 
                    Canvas.select_all existing 
            in 

            Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Canvas(selected));
            let ret_val = Address !(prog.glob_hash_counter) in
              prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
              stack.(sp-1) <- ret_val;
              exec fp sp (pc+1)

        | Jsr (-7) ->
            (* SET POINT *)
            debug ("Jsr -7: - Set point");
            exec fp sp (pc + 1)
        | Jsr i -> 
            stack.(sp) <- IntValue (pc + 1); 
            debug ("Jsr " ^ string_of_int i);
            exec fp (sp+1) i
        | Ent i -> 
            stack.(sp) <- IntValue (fp); 
            debug ("Ent " ^ string_of_int i);
            exec sp (sp+i+1) (pc+1) 
        | Rts i ->
            let new_fp = pop_int stack.(fp) 
            and new_pc = pop_int stack.(fp-1) 
            in
              stack.(fp-i-1) <- stack.(sp-1);
              debug ("Rts " ^ string_of_int i);
              exec new_fp (fp-i) new_pc 
        | Beq i -> 
            debug ("Beq " ^ string_of_int i);
            exec fp (sp-1)
              (pc + 
               if (match stack.(sp-1) with
                       IntValue(k) -> (k = 0)
                     | Address(k) -> match (Hashtbl.find prog.glob_hash k) with
                           Hashtypes.Bool(b) -> not b
                         | _ -> raise(Failure("Beq operation: Address lookup resulted in a non-boolean type.")))
               then i 
               else 1) 
        | Bne i -> 
            debug ("Bne " ^ string_of_int i);
            exec fp (sp-1)
              (pc + 
               if (match stack.(sp-1) with
                       IntValue(k) -> (k != 0)
                     | Address(k) -> match (Hashtbl.find prog.glob_hash k) with
                           Hashtypes.Bool(b) -> not b
                         | _ -> raise(Failure("Bne operation: Address lookup resulted in a non-boolean type.")))
               then i 
               else 1) 
        | Bra i -> 
            debug ("Bra " ^ string_of_int i);
            exec fp sp (pc+i)
        | Hlt -> ()
      in exec 0 0 0 
    with e -> (* catch all exceptions *)
      Printf.eprintf "Unexpected exception: %s\n" (Printexc.to_string e);

