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
  in 
  let stack = Array.make 1024 (IntValue 0)
  and globals = Array.make prog.num_globals (IntValue 0) 
  in 
  let get_pnts sel_type soff canv =  
      (* This match should be on some sort of enum *)
      let h = (Canvas.height canv -1)
      and w = (Canvas.width canv -1) in 
      ( match sel_type with 
          1 -> 
            let x = pop_int stack.(soff - 1) 
            and y = pop_int stack.(soff) in
            Canvas.select_point x y        
        | 2 -> 
            let x1 = pop_int stack.(soff - 3) 
            and x2 = pop_int stack.(soff - 2) 
            and y1 = pop_int stack.(soff - 1)
            and y2 = pop_int stack.(soff) in
            Canvas.select_rect x1 x2 y1 y2  
        | 3 -> 
            let x = pop_int stack.(soff - 2)
            and y1 = pop_int stack.(soff - 1)
            and y2 = pop_int stack.(soff) in
            Canvas.select_hslice x y1 y2  
        | 4 -> 
            let x1 = pop_int stack.(soff - 2)
            and x2 = pop_int stack.(soff - 1)
            and y  = pop_int stack.(soff) in
            Canvas.select_vslice x1 x2 y  
        | 5 -> 
            let x = pop_int stack.(soff) in
            Canvas.select_hslice_all x w
        | 6 -> 
            let y = pop_int stack.(soff) in
            Canvas.select_vslice_all y h 
        | 7 -> 
            Canvas.select_all h w
        | 8 -> 
            let op_id = pop_int stack.(soff)
            and limit = pop_int stack.(soff - 1) in 
            (match op_id with 
              0 -> 
                 let eq x y =
                    Canvas.get x y canv == limit in 
                  Canvas.fetch_match 0 h w eq [];
             | 1 -> 
                  let neq x y =
                    Canvas.get x y canv != limit in 
                  Canvas.fetch_match 0 h w neq [];
             | 2 -> 
                  let less x y =
                    Canvas.get x y canv < limit in 
                  Canvas.fetch_match 0 h w less [];
             | 3 -> 
                  let leq x y =
                    Canvas.get x y canv <= limit in 
                  Canvas.fetch_match 0 h w leq [];
             | 4 -> 
                  let gt x y =
                    Canvas.get x y canv > limit in 
                  Canvas.fetch_match 0 h w gt [];
             | 5 -> 
                  let gte x y =
                    Canvas.get x y canv >= limit in 
                  Canvas.fetch_match 0 h w gte [];

            | _ -> raise (Failure("Invalid Select: 1 SS should catch this"))) 
        | _ -> 
          raise (Failure("Invalid Select: 2 SS should catch this")) )
  and debug s =
    if debug_flag then print_string s
  in 
    debug ("DEBUG: num_globals is " ^ string_of_int prog.num_globals ^ "\n");
    try
      let rec exec fp sp pc = 
        debug ("DEBUG: fp=" ^ (string_of_int fp) ^ ", sp=" ^ (string_of_int sp) ^ ", pc=" ^ (string_of_int pc) ^ ":  ");
        match prog.text.(pc) with 
          Lit i -> 
            stack.(sp) <- IntValue i; 
            debug ("Lit " ^ string_of_int i ^ "\n");
            exec fp (sp+1) (pc+1)
        | Lct i ->
            stack.(sp) <- Address i;
            debug ("Lct " ^ string_of_int i ^ "\n");
            exec fp (sp+1) (pc+1)
        | Drp -> 
            debug ("Drp " ^ "\n");
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
                                debug("Bin +: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (i + j)
                            | Minus ->
                                debug("Bin -: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (i - j)
                            | Times ->
                                debug("Bin *: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (i * j)
                            | Divide ->
                                debug("Bin /: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (i / j)
                            | Mod ->
                                debug("Bin mod: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (i mod j)
                            | Eq ->
                                debug("Bin eq: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean (i = j))
                            | Neq        -> 
                                debug("Bin neq: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean (i != j))
                            | Lt         -> 
                                debug("Bin <: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean (i < j))
                            | Gt         -> 
                                debug("Bin >: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean (i > j))
                            | Leq        -> 
                                debug("Bin <=: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean (i <= j))
                            | Geq        -> 
                                debug("Bin >=: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean (i >= j))
                            | Or         -> 
                                debug("Bin ||: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean ((bool_of_int i) || (bool_of_int j)))
                            | And        -> 
                                debug("Bin &&: i=" ^ string_of_int i ^ " j=" ^ string_of_int j ^ "\n");
                                IntValue (boolean ((bool_of_int i) && (bool_of_int j)))
                            | Mask -> 
                                raise( Failure("Mask is not valid for bools. SS should catch this"))
                         )
                     | (Hashtypes.Bool(b1), Hashtypes.Bool(b2)) ->
                         (match op with
                              Eq ->
                                debug("Bin eq: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2 ^ "\n");
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 = b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | Neq -> 
                                debug("Bin neq: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2 ^ "\n");
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 != b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | Or -> 
                                debug("Bin ||: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2 ^ "\n");
                                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Bool (b1 || b2));
                                let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                            | And -> 
                                debug("Bin &&: b1=" ^ string_of_bool b1 ^ " b2=" ^ string_of_bool b2 ^ "\n");
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
                                debug("Bin +: string1=" ^ s1 ^ " string2=" ^ s2 ^ "\n");
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
                            debug("Canvas 1\n: " ^(Hashtypes.string_of_ct true (Hashtypes.Canvas(c1))   ) ^ "\n");
                            debug("Canvas 1\n: " ^(Hashtypes.string_of_ct true (Hashtypes.Canvas(c2)) ) ^ "\n");

(*                            ( if not ( Canvas.height c1 != Canvas.height c2 && Canvas.width c1 == Canvas.width c2 ) then 
                              raise (Failure ("Canvas Mapping must be on canvas of same dimension ")))
                            ;
 *)
                             Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Canvas (Canvas.mask c1 c2));
                            let ret_val = Address !(prog.glob_hash_counter) in
                                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                                  ret_val
                         | _ ->
                              raise (Failure ("Binop not supported for canvas types."))
                        )

                     | (_, _) ->
                          (* This shouldn't happened if the SS gets to it first *)
                          raise (Failure ("Binop not supported on those operand types."))
                  ))); 
                  exec fp (sp-1) (pc+1)
        | Lod i -> 
            stack.(sp) <- globals.(i); 
            debug ("Lod " ^ string_of_int i ^ " Global=" ^ 
                   (match globals.(i) with
                        IntValue(j) -> "Int value " ^ string_of_int j
                      | Address(j) -> "Pointer to address " ^ string_of_int j 
                   ) ^ "\n");
            exec fp (sp+1) (pc+1) 
        | Str i ->
(*
            (match (stack.(sp-1), globals.(i)) with
                 Address(j), Address(k) ->
                   if j != k then
                     (* if assigning a different pointer to a hash pair, no
                      * longer need the old hash pair, so remove it *)
                     (); (*Hashtbl.remove prog.glob_hash k; *)
                   globals.(i) <- stack.(sp-1)
               | _ ->
                   globals.(i) <- stack.(sp-1)); 
 *)
            globals.(i) <- stack.(sp-1);
            debug ("Str " ^ string_of_int i ^ "\n"); 
            exec fp sp (pc+1) 
        | Lfp i -> 
(*
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
 *)
            stack.(sp) <- stack.(fp + i);
            debug ("Lfp " ^ string_of_int i ^ "\n");
            exec fp (sp+1) (pc+1) 
        | Sfp i -> 
            stack.(fp+i) <- stack.(sp-1); 
            debug ("Sfp " ^ string_of_int i ^ "\n");
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
            debug ("Jsr -1" ^ "\n");
            let lookup =
              (match stack.(sp-1) with
                   IntValue(i) -> Hashtypes.Int(i)
                 | Address(i) -> 
                     try (Hashtbl.find prog.glob_hash i)
                     with Not_found ->
                       (* add error handling *)
                       raise(Failure("Jsr -1: No value found at address " ^ string_of_int i))
              ) in 
            let render = 
              (match stack.(sp-2) with 
                  IntValue(i) -> raise (Failure ("Jsr -1: Render should be a boolean."))
                | Address(i) -> 
                    match (Hashtbl.find prog.glob_hash i) with 
                        Hashtypes.Bool(b) -> b
                      | _ -> raise(Failure ("Jsr -1: Render should be a boolean.")) 
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
            let render = 
              (match stack.(sp-3) with 
                  IntValue(i) -> raise (Failure ("Render should be a boolean"))
                | Address(i) -> match (Hashtbl.find prog.glob_hash i) 
                  with 
                    Hashtypes.Bool(b) -> b (* add error handling *)
                  | _ -> raise (Failure("Jsr -2 expected a boolean render but got a different type.")) 
              ) in
             let filename = 
              ( match (pop_address_val stack.(sp-2)) with
                  Hashtypes.String(s) -> 
                                        (match lookup with 
                                          Hashtypes.Canvas(c) -> Canvas.make_name s render
                                        | _ -> s )

                | _ ->
                    raise (Failure("Jsr -2 expected a string filepath but got a different type.")) 
              ) in 
              
            let oc = open_out filename in 
              output_string oc ( (Hashtypes.string_of_ct render lookup) ^ "\n" );
            exec fp sp (pc+1) 
        | Jsr(-3) ->
            (* CANVAS LOADING *)
            debug ("Jsr -2" ^ "\n");
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
                      debug ("Trying to open: " ^ "../tmp/" ^ List.hd (List.rev filename_parts) ^ ".i" ^ "\n"); 
                      let comm = "python util/load_img.py " ^ path ^ " " ^ granularity in 
                      let _ = Sys.command (comm)
                      in ();
                      "../tmp/" ^ List.hd (List.rev filename_parts) ^ ".i" 
                  | true -> 
                      debug ("Trying to open raw: " ^ path ^ "\n"); 
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
            debug ("Jsr -4" ^ "\n"); 
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
            debug ("Jsr -5" ^ "\n");
             let existing = match (pop_address_val stack.(sp-1)) with
                Hashtypes.Canvas(c) -> c
              | _ -> raise(Failure("Jsr -6: Expected canvas type."))
             and dir = pop_int stack.(sp-2)
             and dist = pop_int stack.(sp-3)
           in 
            let shifted = (Canvas.shift existing dir dist) in 
            Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) 
                (Hashtypes.Canvas (shifted));
              let ret_val = Address !(prog.glob_hash_counter) in
                prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                stack.(sp-1) <- ret_val;
                exec fp sp (pc+1)
       | Jsr (-6) -> 
            (* SELECT *)
            debug ("Jsr -6: - Select Piece of Canvas" ^ "\n");
            let existing = match (pop_address_val stack.(sp-1)) with
                Hashtypes.Canvas(c) -> c
              | _ -> raise(Failure("Jsr -6: Expected canvas type.")) in 
            let sel_type = (pop_int stack.(sp-2)) in
            let stack_offset = sp-3 in 
            let pnts = get_pnts sel_type stack_offset existing in 
            let selected = (Canvas.select_rect_from_list pnts existing) in 
          ( 
             match sel_type > 1 with 

              true -> 
                Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Canvas(selected));
                let ret_val = Address !(prog.glob_hash_counter) in
                  prog.glob_hash_counter := !(prog.glob_hash_counter)+1;
                  stack.(sp-1) <- ret_val; 

            | false -> 
                match List.hd pnts with 
                  (x, y) -> 
                      stack.(sp-1) <- IntValue (Canvas.get x y existing);
 
          );
            exec fp sp (pc+1)
        | Jsr (-7) ->
            (* SET POINT *)
            debug ("Jsr -7: - Set point" ^ "\n");
            let existing = match (pop_address_val stack.(sp-1)) with
                Hashtypes.Canvas(c) -> c
              | _ -> raise(Failure("Jsr -6: Expected canvas type."))
            and set_val = (pop_int stack.(sp-2))
            and sel_type = (pop_int stack.(sp-3))
            and stack_offset = sp-4 in

            let pnts = get_pnts sel_type stack_offset existing in 
            Canvas.set_from_list existing set_val pnts;

            (* 
               Don't actually need to set the canvas back to what it was because it's being modified directly.    
                let modified_can = (Canvas.set_from_list existing set_val pnts) in 
               Hashtbl.add prog.glob_hash !(prog.glob_hash_counter) (Hashtypes.Canvas(modified_can)); 
            *) 

            exec fp sp (pc + 1)
        | Jsr i -> 
            stack.(sp) <- IntValue (pc + 1); 
            debug ("Jsr " ^ string_of_int i ^ "\n");
            exec fp (sp+1) i
        | Ent i -> 
            stack.(sp) <- IntValue (fp); 
            debug ("Ent " ^ string_of_int i ^ "\n");
            exec sp (sp+i+1) (pc+1) 
        | Rts i ->
            let new_fp = pop_int stack.(fp) 
            and new_pc = pop_int stack.(fp-1) 
            in
              stack.(fp-i-1) <- stack.(sp-1);
              debug ("Rts " ^ string_of_int i ^ "\n");
              exec new_fp (fp-i) new_pc 
        | Beq i -> 
            debug ("Beq " ^ string_of_int i ^ "\n");
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
            debug ("Bne " ^ string_of_int i ^ "\n");
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
            debug ("Bra " ^ string_of_int i ^ "\n");
            exec fp sp (pc+i)
        | Hlt -> ()
      in exec 0 0 0 
    with e -> (* catch all exceptions *)
      Printf.eprintf "Runtime error: %s\n" (Printexc.to_string e);

