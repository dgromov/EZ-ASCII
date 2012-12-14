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

let make_map vals =
  let rec add_val the_map = function
     v :: vs -> (add_val (IntMap.add (IntMap.cardinal the_map) v the_map) vs) 
    | [] -> the_map
  in add_val IntMap.empty vals
  
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
    (canvas)
   

let _ = 
  let can = load_canvas "../tmp/edwards.jpeg.i" in 
  let the_map = make_map ['.'; '-'; '+' ;'X'; '@'] in 
    print_canvas_out can true  the_map; 
    print_string "\n"
 