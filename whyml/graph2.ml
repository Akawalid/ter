type t = ((Z.t) list) array

let size (g: ((Z.t) list) array) : Z.t = Z.of_int (Array.length g)

let e_size : type xi. ((xi list) array) ->  (Z.t) =
  fun g -> let rec aux (i: Z.t) (n: Z.t) : Z.t =
             if Z.geq i (Z.of_int (Array.length g))
             then n
             else
               aux (Z.add i Z.one)
               (Z.add n (Z.of_int (List.length g.(Z.to_int i)))) in
           aux Z.zero Z.zero

let create (n: Z.t) : ((Z.t) list) array = Array.make (Z.to_int n) ([] )

let add_e (g: ((Z.t) list) array) (s1: Z.t) (s2: Z.t) : unit =
  g.(Z.to_int s1) <- s2 :: g.(Z.to_int s1)

let succ (g: ((Z.t) list) array) (v: Z.t) : (Z.t) list = g.(Z.to_int v)

