let dfs g s =
  let visited = Array.make (Graph.size g) false in
  let rec loop s =
    if not (visited.(s)) then
      visited.(s) <- true;
      let neighbours = Graph.succ g s in
      List.iter (fun succ -> loop succ) neighbours
      (*    ^ create in why3 because it doesn't exist *)
  in
  loop s;
  let accessible = ref [] in
  for i = 0 to Array.length visited - 1 do
    if visited.(i) then
      accessible := i :: !accessible
  done;
  List.rev !accessible