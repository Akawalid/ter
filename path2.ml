type path = (Z.t) list

let dfs (g: ((Z.t) list) array) (v: Z.t) : (Z.t) list =
  let visited = Array.make (Z.to_int (size g)) false in
  let accessible = ref ([] ) in
  let rec branch (v1: Z.t) : unit =
    if not visited.(Z.to_int v1)
    then begin
           visited.(Z.to_int v1) <- true;
           accessible := v1 :: !accessible;
           loop (succ g v1)
         end
  and loop (l: (Z.t) list) : unit =
    match l with
    | [] -> ()
    | neighbour :: s -> branch neighbour; loop s in
  branch v; !accessible

