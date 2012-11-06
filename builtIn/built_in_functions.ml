module IntMap = 
  Map.Make(
    struct type t = int
    let compare = compare end
  )

(* Blank Function  *)
let blank height width = 
  Array.make_matrix height width 0;;

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
  can;; 

let make_map vals =
  let rec add_val the_map = function
     v :: vs -> (add_val (IntMap.add (IntMap.cardinal the_map) v the_map) vs) 
    | [] -> the_map
  in add_val IntMap.empty vals

let _ = 
  let the_map = make_map ['q';'z'] in 
  
  (* let oc = stdout in  *)
  let oc = open_out "output.txt" in
    print_canvas (blank 10 10) false the_map oc;
    close_out oc; 
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