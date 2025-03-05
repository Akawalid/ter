let parse file = 
  let channel = open_in file in
  let v = int_of_string (input_line channel) in 
  (*let s = int_of_string (input_line channel) in*)
  let g = Graph.create v in

  let rec loop () =
    try
      let line = input_line channel in 
      Scanf.sscanf line "%d %d" (fun v1 v2 -> 
        try Graph.add_e g v1 v2 
        with Failure _ -> print_string "Error: Index out of bound, verify vertices are within the bound 'v'\n"
      );
      loop ()
    with End_of_file -> close_in channel 
  in
  
  loop ();
  close_in channel;
  g