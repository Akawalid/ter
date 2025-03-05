(* Programme temporaire de test *)
let print_int_list l =
  print_string "[";
  let rec loop l =
    match l with
    | [] -> print_string "]"
    | [x] -> print_int x; print_string "]"
    | x :: q -> print_int x; print_string ", "; loop q
  in loop l; print_newline ()

let () =
  let g1 = Parse.parse "g1.txt" in
  let l1 = Path.dfs g1 0 in
  print_int_list l1;

  let l2 = Path.dfs g1 3 in
  print_int_list l2;

  let l3 = Path.dfs g1 4 in
  print_int_list l3