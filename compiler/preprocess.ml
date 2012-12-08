let run oc =
  let n = in_channel_length oc in 
  let s = String.create n in 
  really_input oc s 0 n;
  close_in oc; 
  s; 
