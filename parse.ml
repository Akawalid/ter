let parse file = 
  let channel = open_in file in
  let v = int_of_string (input_line channel) in 
  let s = int_of_string (input_line channel) in
  let g = Graph.create v in
  (try Graph.set_start g s
  with Failure _ -> failwith "Parsing error, expected integer at 2nd line.");

  let rec loop () =
    try
      let line = input_line channel in 
      parse_line line;
      loop ()
    with End_of_file -> close_in channel 

  and parse_line line = Scanf.sscanf line "%d %d" (fun s1 s2 -> Graph.add_e g s1 s2)
  in
  loop ();
  close_in channel;
  g