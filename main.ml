(* Programme temporaire de test *)
(* see Format, pp_print_list *)
let print_zarith_list l =
  print_string "[";
  let rec loop l =
    match l with
    | [] -> print_string "]"
    | [x] -> print_int (Z.to_int x); print_string "]"
    | x :: q -> print_int (Z.to_int x); print_string ", "; loop q
  in loop l; print_newline ()

let () =
  let g1 = Parse.parse "g1.txt" in
  let l1 = Path2.dfs g1 (Z.of_int 0) in
  print_zarith_list l1;

  let l2 = Path2.dfs g1 (Z.of_int 3) in
  print_zarith_list l2;

  let l3 = Path2.dfs g1 (Z.of_int 4) in
  print_zarith_list l3

(* Sys.argv pour des entrées dynamiques: module Arg *)
(* Graphviz *)