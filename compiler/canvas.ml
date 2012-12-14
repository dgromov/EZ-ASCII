(* FILENAME :  canvas.ml
 * AUTHOR(S):  Dmitriy Gromov (dg2720), Joe Lee (jyl2157) 
 * PURPOSE  :  Canvas functions (loading, preprocessing, blank,
 *             string_of_canvas, etc...).
 *)

module IntMap = 
  Map.Make(
    struct type t = int
    let compare = compare end
  )

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
let string_of_row row render the_map =
  match render with
      false -> String.concat " " (Array.to_list (Array.map string_of_int row ))
    | true ->  String.concat "" (Array.to_list (Array.map (fun x -> Char.escaped (IntMap.find x the_map)) row))

let make_map vals =
  let rec add_val the_map = function
     v :: vs -> (add_val (IntMap.add (IntMap.cardinal the_map) v the_map) vs) 
    | [] -> the_map
  in add_val IntMap.empty vals

let string_of_canvas can map render =
  String.concat "\n" (Array.to_list(Array.map (fun r -> string_of_row r render map ) can))

(* Loads an image from filepath fname, and returns
 *  canvas type int array array *)
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


