 (* AUTHOR(S):  Dmitriy Gromov (dg2720), Joe Lee (jyl2157) 
 * PURPOSE  :  Canvas functions (loading, preprocessing, blank,
 *             string_of_canvas, etc...).
 *)

module IntMap = 
  Map.Make(
    struct type t = int
    let compare = compare end
  )

let make_map vals =
  let rec add_val the_map = function
     v :: vs -> (add_val (IntMap.add (IntMap.cardinal the_map) v the_map) vs) 
    | [] -> the_map
  in add_val IntMap.empty vals

(* Default mapping taken from http://incredibleart.org/links/ascii/new_faq.html *)
let default_map = 
   make_map (['.';'`';'^';':';'"';';';'~';  
            '-';'_';'+';'<';'>';'i';'!'
            ;'l';'I';'l';'?';'|';
            '(';')';'1';'{';'}';'[';']';    
            'r';'c';'v';'u';'n';'x';'z';'j'
            ;'f';'t';'L';'C';'J';'U';'Y';'X'
            ;'Z';'O';'0';'Q';'o';'a';'h';'k'
            ;'b';'d';'p';'q';'w';'m';'*';'W';
            'M';'B';'8';'&';'%';'$';'#';'@']) 

(* If 0 or Max Granularity Then min or max, otherwise even dist *)
let get_char_for_intensity intensity granularity map = 
  let max_int = (granularity - 1) in 
  let card = ((IntMap.cardinal map) - 1) in 
  let multip =  card /  max_int in 
  let idx = 
    match intensity == 0 with 
      true -> 0 
    | false -> match intensity = max_int with 
        true -> (card)
      | false  -> multip * intensity
  in 
  try 
    Char.escaped (IntMap.find idx map);
  with e ->
    raise (Failure ("Intensity " ^ string_of_int idx ^ " is out of bounds "))

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
| BOOL 

let select_type = function 
  POINT -> 1
| RECT -> 2
| VSLICE -> 3
| HSLICE -> 4 
| VSLICE_ALL -> 5  
| HSLICE_ALL -> 6 
| ALL -> 7 
| BOOL -> 8 


type dir =
   UP
 | LEFT 
 | DOWN 
 | RIGHT

let get_dir = function 
   0 -> (UP)
 | 1 -> (LEFT)
 | 2 -> (DOWN)
 | 3 -> (RIGHT)
 | _ -> raise(Failure ("Not a valid Direction"))


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
let string_of_row row render the_map gran =
  match render with
      false -> String.concat " " (Array.to_list (Array.map string_of_int row ))
    | true ->  String.concat "" (Array.to_list (Array.map (fun x ->   match x with 
                                                                      (-1) -> " "
                                                                      | _ ->  (get_char_for_intensity 
                                                                                x gran the_map)) row))



let string_of_canvas can map render =
  String.concat "\n" (Array.to_list(Array.map (fun r -> string_of_row r render map can.gran ) can.data))


(* CANVAS ATTRIBUTES *)
let height can = 
  Array.length can.data 

let width can = 
  Array.length can.data.(0)

let granularity can = 
  can.gran

(* END CANVAS ATTRIBUTES *)

let create_blank_from_existing existing default = 
   blank (height existing) (width existing)  (granularity existing) default 


(* SELECT *)
let get x y can = 
  if x < (height can) && x >= 0
     && y < (width can) && y >= 0
  then
    can.data.(x).(y)
  else 
    raise (Failure("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ") is out of bounds of canvas")) 
(* MASK *)
let set x y intensity can = 
  if x < (height can) && x >= 0
     && y < (width can) && y >= 0
  then
    can.data.(x).(y) <- intensity

let accept_all x y = 
  true 


let rec fetch_row x1 y1 y2 acc cond=
    match y1 <= y2 with 
      true -> 
           if (cond x1 y1) then 
                (x1, y1) :: fetch_row x1 (y1+1) y2 acc cond 
           else fetch_row x1 (y1+1) y2 acc cond
          
    | false -> [] 

let rec fetch_box x1 x2 y1 y2 acc = 
    match x1 <= x2 with
      true -> (fetch_row x1 y1 y2 acc accept_all) @ fetch_box (x1+1) x2 y1 y2 acc 
      | false -> []

let rec fetch_match x h w cond acc = 
    match x <= h with
      true -> (fetch_row x 0 w acc cond) @ fetch_match (x+1) h w cond acc
      | false -> [] 

let string_of_point = function 
  (x, y) -> string_of_int x ^ " " ^ string_of_int y ;;

let rec print_l = function
    x :: xs -> print_endline (string_of_point x); 
               print_l xs 
  | [] -> ""

let rec set_point can intensity = function
    x :: xs -> (match (x) with 
                (i,j) -> 
                  (match intensity >= 0 with 
                    true -> set i j intensity can
                    | false -> ());
                  set_point can intensity xs;
                )
  | [] -> ()
;; 

(* let set_points_int points can intensity = 
  let l = (fetch_match can []) in 
    set_point can intensity l 
 *)
let set_rect_can l old_can new_can= 
  let rec set_point = function 
    x :: xs -> (match x with 
                  (i,j) -> 
                    let selected = get i j old_can in 
                      match selected >= 0 with 
                        true -> set i j selected new_can
                      | false -> ()
                );
                set_point xs;
  | [] -> () 
  in 
  set_point (l);
  (new_can)

let set_from_list can intensity pnts = 
  set_point can intensity pnts

let select_rect_from_list l can = 
   let blank_slate = create_blank_from_existing can (-1) in 
    set_rect_can  l can blank_slate 

(* 
   The following functions simply return points representing various boxes
   They are labels for the various select operations.
*)
 
let select_rect x1 x2 y1 y2 = 
    fetch_box x1 x2 y1 y2 []

let select_point x y = 
  fetch_box x x y y [] 

let select_hslice x y1 y2  = 
  fetch_box x x y1 y2 [] 

let select_vslice x1 x2 y  = 
  fetch_box x1 x2 y y []

let select_hslice_all x w= 
  fetch_box x x 0 (w) []

let select_vslice_all y h = 
  fetch_box 0 (h) y y []

let select_all h w= 
  fetch_box 0 (h) 0 (w) []

(* END SELECT *)

let mask can1 can2 =
  let blank_slate = create_blank_from_existing can1 (-1) in 
    let pl = select_all ((height can2)-1) ((width can2)-1) in
     let cp_can1 = set_rect_can pl can1 blank_slate in
     (set_rect_can  pl can2 cp_can1 ) 
  
let shift can dir steps = 
  let shifted = create_blank_from_existing can (-1) in 
    let rec set_point = function
      x :: xs -> (match (x) with 
                  (i,j) -> 
                    ( let intensity = get i j can 
                      and new_point = 
                        (match ( get_dir (dir) ) with 
                          UP ->
                            (i - steps, j) 
                        | DOWN -> 
                            (i + steps, j) 
                        | LEFT ->
                            (i, j - steps)
                        | RIGHT -> 
                            (i, j + steps) 
                        ) in 

                      (
                        match new_point with 
                          (a, b) ->  set a b intensity shifted 
                      )
                     )
                  );
                  set_point xs;
    | [] -> ()
  in 
  let l = 
    (match ( get_dir (dir) ) with 
                          UP ->
                          (fetch_box steps (((height can)-1)) 0 ((width can)-1) [])
                        | DOWN -> 
                          (fetch_box 0 (((height can)-1) - steps) 0 ((width can)-1) [])
                        | LEFT ->
                          (fetch_box 0 ((height can)-1) steps ((width can)-1) [])
                        | RIGHT -> 
                          (fetch_box 0 (((height can)-1)) 0 (((width can)-1) - steps) [])
                      ) in 
    set_point (l); 
  
  shifted

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


