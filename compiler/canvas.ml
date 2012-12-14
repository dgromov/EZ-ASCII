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

type canvas =  
{
  data: int array array; 
  gran: int; 
};; 

(* Blank Function  *)
let blank height width granularity = 
  { 
    data = Array.make_matrix height width 0; 
    gran = granularity
  }

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
  String.concat "\n" (Array.to_list(Array.map (fun r -> string_of_row r render map ) can.data))


(* CANVAS ATTRIBUTES *)
let height can = 
  Array.length can.data 

let width can = 
  Array.length can.data.(0)

let granularity can = 
  can.gran

(* END CANVAS ATTRIBUTES *)

let create_blank_from_existing existing = 
   blank (width existing) (height existing) (granularity existing)


(* SELECT *)
let get x y can = 
  can.data.(x).(y)

(* MASK *)
let set x y intensity can = 
  can.data.(x).(y) <- intensity

let select_point x y can = 
  let blank_slate = create_blank_from_existing can in 
    let selected = get x y can in 
      set x y selected blank_slate;
  blank_slate

let select_rect x1 x2 y1 y2 can = 
   let blank_slate = create_blank_from_existing can in 
   print_endline (string_of_int x2) ;
          
      for i = x1 to x2 do 
        for j = y1 to y2 do 
          let selected = get i j can in 
            set i j selected blank_slate;
        done; 
      done;

    (blank_slate)


(* END SELECT *)








(* Loads an image from filepath fname, and returns
 *  canvas type int array array *)
let load_canvas fname granularity  = 
  let ic = open_in fname in 
  let n = in_channel_length ic in
  let s = String.create n in 
    really_input ic s 0 n; 
    close_in ic;
    let lines = Str.split (Str.regexp("\n")) s  in
    let can = {
                data = (Array.make_matrix (List.length lines) (String.length (List.hd lines))) 0; 
                gran = granularity;
              }
    in 
    let x = ref 0 in  
      List.iter (
        fun line -> 
          let row = Str.split (Str.regexp(" ")) line in 
            can.data.(!x) <- (Array.of_list (List.map int_of_string row)); 
            x := !x+1
      ) lines; 
      (can);;


