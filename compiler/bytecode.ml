open Ast

type bstmt =
    Lit of int          (* Push a literal *)
  | Stl of string       (* Push a string literal *)
  | Boo of bool         (* Push a bool literal *)
  | Drp                 (* Discard a value *)
  | Bin of Ast.op       (* Perform arithmetic on top of stack *)
  | Lod of int          (* Fetch global variable *)
  | Str of int          (* Store global variable *)
  | Lfp of int          (* Load frame pointer relative *)
  | Sfp of int          (* Store frame pointer relative *)
  | Jsr of int          (* Call function by absolute address *)
  | Ent of int          (* PushFP, FP->SP, SP+=i *)
  | Rts of int          (* Restore FP, SP, consume formals, push result *)
  | Beq of int          (* Branch relative if top-of-stack is zero *)
  | Bne of int          (* Branch relative if top-of-stack is non-zero *)
  | Bra of int          (* Branch relative *)
  | Lcv of int          (* Load canvas by absolute address *)
  | Hlt                 (* Terminate *)

type prog = {
  num_globals : int;    (* Number of global variables *) 
  text : bstmt array;   (* Code for all the functions *)
}

let string_of_prog prog =
  let rec string_of_prog_helper s = function
      []          -> s 
    | hd :: tail  -> 
        let s = 
          match hd with
              Lit(i)      -> s ^ "Lit " ^ string_of_int i ^ "\n"
            | Stl(st)     -> s ^ "Stl " ^ st ^ "\n"
            | Boo(b)      -> s ^ "Boo " ^ string_of_bool b ^ "\n"
            | Drp         -> s ^ "Drp\n"
            | Bin(op)     -> s ^ "Bin\n"
            | Lod(i)      -> s ^ "Lod " ^ string_of_int i ^ "\n"
            | Str(i)      -> s ^ "Str " ^ string_of_int i ^ "\n"
            | Lfp(i)      -> s ^ "Lfp " ^ string_of_int i ^ "\n"
            | Sfp(i)      -> s ^ "Sfp " ^ string_of_int i ^ "\n"
            | Jsr(i)      -> s ^ "Jsr " ^ string_of_int i ^ "\n"
            | Ent(i)      -> s ^ "Ent " ^ string_of_int i ^ "\n"
            | Rts(i)      -> s ^ "Rts " ^ string_of_int i ^ "\n"
            | Beq(i)      -> s ^ "Beq " ^ string_of_int i ^ "\n"
            | Bne(i)      -> s ^ "Bne " ^ string_of_int i ^ "\n"
            | Bra(i)      -> s ^ "Bra " ^ string_of_int i ^ "\n"
            | Lcv(i)      -> s ^ "Lcv " ^ string_of_int i ^ "\n"    
            | Hlt         -> s ^ "Hlt\n"
        in string_of_prog_helper s tail
  in string_of_prog_helper "" (Array.to_list prog.text) 
