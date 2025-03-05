type t = {v: int; graph: int list array}
(*idea: store then number of V and E*)
let create n_vertecies = {
  v = n_vertecies;
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

let succ g v = g.graph.(v)
let size g = g.v 
