(* FILENAME :  preprocess.ml
 * AUTHOR(S):  Dmitriy Gromov (dg2720)
 * PURPOSE  : 
 *) 

open Str

let read_file fname = 
	let ic = open_in fname in 
		let n = in_channel_length ic in
			let s = String.create n in 
				really_input ic s 0 n; 
				close_in ic; 
			(s) ;; 

let run fname = 
	let prog_text = read_file fname in 
		let white_sp = "[\r\n\t ]" in

		let inc_regex = Str.regexp (
																"include" (* include *)
																^ white_sp ^ "+"  (* atleast 1 space btwn include and filename *)
																^ "\"\\(.+\\)\""  (* "sometext" - Only the part between the quotes goes in *)
																^ white_sp ^ "*" ^ ";" 
															 ) in (* Possible space between end of string and semi-colon *)
		
		let rec  replace_include text =  
			try 
			  ignore (Str.search_forward inc_regex text 0);
				let q = Str.global_substitute inc_regex (
																									fun m -> 
																									let inc_name = Str.matched_group 1 m in 
																									read_file inc_name
																								) text in 

					replace_include ( q )
			with Not_found -> 
						text 
			in replace_include prog_text
