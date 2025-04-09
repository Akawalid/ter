type t = {v: int; graph: int list array}
(* Idea: store the number of Vertices 'v' as well as the graph itself *)
let create n = {
  v = n;
  graph = Array.make n []
}
let add_e g s1 s2 =
(* Add edge e1->e2 to the graph *)
  assert (s1 >= 0 && s2 >= 0 &&
          s1 < Array.length g.graph && 
          s2 < Array.length g.graph 
  );
  g.graph.(s1) <- s2 :: g.graph.(s1)

let succ g v = g.graph.(v)
let size g = g.v 
