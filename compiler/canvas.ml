 (* AUTHOR(S):  Dmitriy Gromov (dg2720), Joe Lee (jyl2157) 
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

type stypes = 
  POINT
| RECT  
| VSLICE 
| HSLICE  
| VSLICE_ALL 
| HSLICE_ALL  
| ALL

let select_type = function 
  POINT -> 1
| RECT -> 2
| VSLICE -> 3
| HSLICE -> 4 
| VSLICE_ALL -> 5  
| HSLICE_ALL -> 6 
| ALL -> 7 


type dir =
   UP
 | DOWN 
 | LEFT 
 | RIGHT

let get_dir = function 
  0 -> (UP)
 | 1 -> (DOWN)
 | 2 -> (LEFT)
 | 3 -> (RIGHT)
;;


(* Blank Function  *)
let blank height width granularity default= 
  { 
    data = Array.make_matrix height width default; 
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
    | true ->  String.concat "" (Array.to_list (Array.map (fun x ->   match x with 
                                                                      (-1) -> " "
                                                                      | _ ->  Char.escaped (IntMap.find x the_map)) row))

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

let create_blank_from_existing existing default = 
   blank (width existing) (height existing) (granularity existing) default 


(* SELECT *)
let get x y can = 
  can.data.(x).(y)

(* MASK *)
let set x y intensity can = 
  can.data.(x).(y) <- intensity

let set_rect_int x1 x2 y1 y2 can intensity = 
  for i = x1 to x2 do 
      for j = y1 to y2 do 
          set i j intensity can;
      done; 
    done;
;;

let set_rect_can x1 x2 y1 y2 old_can new_can = 
  for i = x1 to x2 do 
      for j = y1 to y2 do 
        let selected = get i j old_can in 
          match selected >= 0 with 
            true -> set i j selected new_can;
            | false -> ();
      done; 
    done;

    (new_can)
;;

let select_rect x1 x2 y1 y2 can = 
   let blank_slate = create_blank_from_existing can (-1) in 
    set_rect_can x1 x2 y1 y2 can blank_slate;
  (blank_slate)

let select_point x y can = 
  select_rect x x y y can 

let select_hslice x y1 y2 can = 
  select_rect x x y1 y2 can 

let select_vslice x1 x2 y can = 
  select_rect x1 x2 y y can 

let select_hslice_all x can = 
  select_rect x x 0 ((width can)-1) can 

let select_vslice_all y can = 
  select_rect 0 ((height can)-1) y y can 

let select_all can = 
  select_rect 0 ((height can)-1) 0 ((width can)-1) can 

(* END SELECT *)

let mask can1 can2 =
  let blank_slate = create_blank_from_existing can1 (-1) in 
  let cp_can1 = set_rect_can 0 ((height can2)-1) 0 ((width can2)-1) blank_slate can1 in 
    set_rect_can 0 ((height can2)-1) 0 ((width can2)-1) blank_slate can2 

let shift can1 dir steps = 
  let blank_slate = create_blank_from_existing can1 (-1) in 
  match dir with 
    UP -> 
       set_rect_can 0 ((height can1)-1) 0 ((width can1) - (1+steps) ) blank_slate can1
  | DOWN ->
       set_rect_can 0 ((height can1)-1) steps ((width can1)-1) blank_slate can1
  | LEFT -> 
       set_rect_can 0 ((height can1)- (1+steps)) 0 ((width can1)-1) blank_slate can1
  | RIGHT ->
       set_rect_can steps ((height can1)-1) 0 ((width can1)-1) blank_slate can1

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


