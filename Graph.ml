
module Graph = struct 
  type t = {v: int; e: int; mutable start: int; graph: int list array}
  (*idea: store then number of V and E*)
  let create n_vertecies = {
    v = n_vertecies;
    e = 0;
    start=-1;
    Array.make n_vertecies [] 
  }
  let add_e g s1 s2 =
    (*Add edge e1->e2 to the graph*)
    (*Is it better to make the structur imutable*)
    assert (s1 >= 0 &&
            s2 >= 0 &&
            s1 < Array.length g && 
            s2 < Array.length g 
           );
    g.(s1) <- s2 :: g.(s1); 

  let set_start g start = g.start <- start 
end