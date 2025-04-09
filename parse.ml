let parse file = 
  let channel = open_in file in
  let v = Z.of_int(int_of_string (input_line channel)) in 
  (*let s = int_of_string (input_line channel) in*)
  let g = Graph2.create v in

  let rec loop () =
    (* match with exception to keep the terminal behaviour *)
    try
      let line = input_line channel in 
      Scanf.sscanf line "%d %d" (fun v1 v2 -> 
        try Graph2.add_e g (Z.of_int v1) (Z.of_int v2)
        with Failure _ -> print_string "Error: Index out of bound, verify vertices are within the bound 'v'\n"
      );
      loop ()
    with End_of_file -> close_in channel 
  in
  
  loop ();
  close_in channel;
  g