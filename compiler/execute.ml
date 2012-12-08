open Ast
open Bytecode


let implode lst =
  let res = String.create (List.length lst) in
  let rec imp i = function
    | [] -> res
    | c :: lst -> res.[i] <- c; imp (i + 1) lst in
    imp 0 lst

let execute_prog prog debug_flag =
  let stack = Array.make 1024 0
  and canv_env = Array.make 100 []
  and globals = Array.make prog.num_globals 0 
  and debug s =
   if debug_flag then print_endline s
  in 
    debug ("DEBUG: num_globals is " ^ string_of_int prog.num_globals);
    try
      let rec exec fp sp pc = match prog.text.(pc) with 
          Lit i -> 
            stack.(sp) <- i; 
            debug ("DEBUG: Lit " ^ string_of_int i);
            exec fp (sp+1) (pc+1)
        | Drp -> 
            debug ("DEBUG: Drp ");
            exec fp (sp-1) (pc+1)
        | Bin op ->
            debug ("DEBUG: Bin ");
            let op1 = stack.(sp-2) 
            and op2 = stack.(sp-1) in
              stack.(sp-2) <- 
              (let boolean b = if b then 1 else 0 
               and bool_of_int i = if i > 0 then true else false
               in 
                 match op with
                     Plus       -> 
                       debug("Bin +: op1=" ^ string_of_int op1 ^ " op2=" ^ string_of_int op2);
                       op1 + op2
                   | Minus      -> op1 - op2
                   | Times      -> op1 * op2
                   | Divide     -> op1 / op2
                   | Mod        -> op1 mod op2
                   | Eq         -> boolean (op1 = op2)
                   | Neq        -> boolean (op1 != op2)
                   | Lt         -> boolean (op1 < op2)
                   | Gt         -> boolean (op1 > op2)
                   | Leq        -> boolean (op1 <= op2)
                   | Geq        -> boolean (op1 >= op2)
                   | Or         -> boolean ((bool_of_int op1) || (bool_of_int op2))
                   | And        -> boolean ((bool_of_int op1) && (bool_of_int op2))
                   | Mask       -> 1 (* need to do *)
              );
              exec fp (sp-1) (pc+1)
        | Lod i -> 
            stack.(sp) <- globals.(i); 
            debug ("DEBUG: Lod " ^ string_of_int i ^ " Global=" ^ string_of_int globals.(i));
            exec fp (sp+1) (pc+1) 
        | Str i -> 
            globals.(i) <- stack.(sp-1); 
            debug ("DEBUG: Str " ^ string_of_int i);
            exec fp sp (pc+1) 
        | Lfp i -> 
            stack.(sp) <- stack.(fp+i);
            debug ("DEBUG: Lfp " ^ string_of_int i);
            exec fp (sp+1) (pc+1) 
        | Sfp i -> 
            stack.(fp+i) <- stack.(sp-1); 
            debug ("DEBUG: Sfp " ^ string_of_int i);
            exec fp sp (pc+1) 
        (* here Jsr -1, -2, -3 refer to the OutputC functionality 
         * for ints/strings/bools, respectively *)
        | Jsr(-1) ->
            debug ("Jsr -1");
            print_endline (string_of_int stack.(sp-1));
            exec fp sp (pc+1)
        | Jsr(-2) -> 
            debug ("Jsr -2");
            let size = stack.(sp-1) in
            let rec load_ints_list accum counter = 
              if counter > size
              then accum
              else load_ints_list (stack.(sp-2-counter) :: accum) (counter+1)
            in 
            let res = 
              (* check for empty string *)
              if size = 0
              then []
              else load_ints_list [] 1 in
              (if (List.length res) = 0 
               then print_endline "" 
               else print_endline (implode (List.map Char.chr res)));
              exec fp sp (pc+1)
        | Jsr(-3) ->
            debug ("Jsr -3");
            let i = stack.(sp-1) in
              (if i > 0 
               then print_endline "true"
               else print_endline "false");
              exec fp sp (pc+1)

           
        | Jsr i -> 
            stack.(sp) <- pc + 1; 
            debug ("DEBUG: Jsr " ^ string_of_int i);
            exec fp (sp+1) i
        | Ent i -> 
            stack.(sp) <- fp; 
            debug ("DEBUG: Ent " ^ string_of_int i);
            exec sp (sp+i+1) (pc+1) 
        | Rts i -> 
            let new_fp = stack.(fp) 
            and new_pc = stack.(fp-1) in
              stack.(fp-i-1) <- stack.(sp-1);
              debug ("DEBUG: Rts " ^ string_of_int i);
              exec new_fp (fp-i) new_pc 
        | Beq i -> 
            debug ("DEBUG: Beq " ^ string_of_int i);
            exec fp (sp-1)
              (pc + if stack.(sp-1) = 0 then i else 1) 
        | Bne i -> 
            debug ("DEBUG: Bne " ^ string_of_int i);
            exec fp (sp-1)
              (pc + if stack.(sp-1) != 0 then i else 1) 
        | Bra i -> 
            debug ("DEBUG: Bra " ^ string_of_int i);
            exec fp sp (pc+i)
        | Lcv i -> ()
        | Hlt -> ()
      in exec 0 0 0 
    with e -> (* catch all exceptions *)
      Printf.eprintf "Unexpected exception: %s" (Printexc.to_string e);

