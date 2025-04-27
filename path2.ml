open Graph2
type path = (Z.t) list

let dfs (g: ((Z.t) list) array) (v': Z.t) : (Z.t) list =
  let visited = Array.make (Z.to_int (size g)) false in
  let accessible = ref ([] ) in
  let rec branch (v: Z.t) : unit =
    if not visited.(Z.to_int v)
    then begin
           visited.(Z.to_int v) <- true;
           accessible := v :: !accessible;
           let succs = succ g v in loop succs
         end
  and loop (l: (Z.t) list) : unit =
    match l with
    | [] -> ()
    | neighbour :: s -> branch neighbour; loop s in
  branch v'; !accessible