(* FILENAME :  hashtypes.ml
 * AUTHOR(s):  Joe Lee (jyl2157)
 * PURPOSE  :  Define custom types for hash map values to support EZ-ASCII's
 * types.
 *)

module IntMap = 
  Map.Make(
    struct type t = int
    let compare = compare end
  )

(*
(* Blank Function  *)
let blank height width = 
  Array.make_matrix height width 0;;

(* Make File Name *)
let make_name name render = 
  if render then 
    name 
  else 
    String.concat "" [name; ".i"]

(* Print Row *)
let print_row row render the_map oc =
  Array.iter 
  (
    fun x -> (
              match render with
                  false -> output_string oc (string_of_int x);
                           output_string oc " "; 

                | true ->  output_char oc (IntMap.find x the_map ) 
             )
  ) row;;

(* Print Canvas *)
let print_canvas can render the_map oc =
 
    Array.iter 
    (
      fun x ->
           ( 
              print_row x render the_map oc;
              output_string oc "\n";

           )
    ) can
  
let print_canvas_out can render the_map = 
  print_canvas can render the_map stdout;; 

let print_canvas_file can render the_map file_name = 
  let oc = open_out (make_name file_name render) in
    print_canvas can render the_map oc;
    close_out oc;;
 *)


let string_of_canvas can map render =
  Array.fold_left (fun accum row ->
                     accum ^ (String.concat "" (List.map string_of_int (Array.to_list row)))) "" can

let make_map vals =
  let rec add_val the_map = function
     v :: vs -> (add_val (IntMap.add (IntMap.cardinal the_map) v the_map) vs) 
    | [] -> the_map
  in add_val IntMap.empty vals
  
type ct =
    (* Note: The compiler will not add any int types to the hash map 
     * but the bytecode executor might during binop operations *)
    Int of int
  | String of string
  | Bool of bool
  | Canvas of int array array

let string_of_ct = function
    Int(i) -> string_of_int i
  | String(s) -> (Scanf.unescaped s)
  | Bool(b) -> string_of_bool b
  | Canvas(c) -> string_of_canvas c (make_map ['.'; '-'; '+'; 'X'; '@']) true
     
