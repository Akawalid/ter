module Graph = struct 
  type t = {v: int; e: int; mutable start: int; graph: int list array}
  (*idea: store then number of V and E*)
  let create n_vertecies = {
    v = n_vertecies;
    e = 0;
    start = -1;
    graph = Array.make n_vertecies [] 
  }
  let add_e g s1 s2 =
    (*Add edge e1->e2 to the graph*)
    (*Is it better to make the structur imutable*)
    assert (s1 >= 0 &&
            s2 >= 0 &&
            s1 < Array.length g.graph && 
            s2 < Array.length g.graph 
           );
    g.graph.(s1) <- s2 :: g.graph.(s1)

  let set_start g start = g.start <- start 
end;;

module Parsing = struct
  let next =
    let i = ref 0 in
    fun () ->
      i := !i + 1;
      !i
      
  let parse file = 
    let channel = open_in file in
    let g = ref (Graph.create 0) in
    let rec loop () = 
      try
        let line = input_line channel in 
        parse_line line;
        loop ()
      with End_of_file -> close_in channel 
      
    and parse_line line = 
      let i = next () in
      if i = 0 then 
        (*Vertecies number*)
        let v_l = int_of_string line in
        ()
      else if i = 1 then ()
      else if i = 2 then 
        try Graph.set_start !g (int_of_string line)
        with Failure _ -> failwith "Parsing error, excpected integer at 3rd line." 
      else 
        (match String.split_on_char ' ' line with
        (*Is it better to make Grpah module type t abstract*)
        [s1; s2] -> Graph.add_e !g (int_of_string s1) (int_of_string s2)
        | _ -> failwith "Parsing error");
    in
    loop ();
    !g
end