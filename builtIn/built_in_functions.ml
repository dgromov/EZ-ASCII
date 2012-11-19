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
    )
    can;
  close_out oc;;

let print_canvas_out can render the_map = 
  print_canvas can render the_map stdout;; 

let print_canvas_file can render the_map file_name = 
  let oc = open_out (make_name file_name render) in
    print_canvas can render the_map oc;;  

let make_map vals =
  let rec add_val the_map = function
     v :: vs -> (add_val (IntMap.add (IntMap.cardinal the_map) v the_map) vs) 
    | [] -> the_map
  in add_val IntMap.empty vals

let _ = 
(*   let img = Images.load "testimg.jpg" []
  img; 
 *)



  let the_map = make_map ['q';'z'] in 
  print_canvas_file (blank 10 10) true the_map "test";
  print_string "\n"

(* let oc = open_out "output.txt" in
  try
    while true do
      let s = input_line ic in
      output_string oc s;
      output_char oc '\n';
    done
  with End_of_file ->
    close_in ic;
    close_out oc; *)