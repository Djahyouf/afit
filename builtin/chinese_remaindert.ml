(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec _crt num list =
    match list with
    |[] -> []
    |e::l -> (modulo num e) :: (_crt num l)
  in _crt x l
;;

(** Inverse image of Chinese Remainder map
    @para m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
let crt_solver m l y =
  match (l, y) with
  | (x::y::_, a::b::_ ) ->
     let (u, v, gcd) = bezout x y in
     modulo (a*y*v + b*x*u) m
  | _ -> failwith "give me good arrays !"
;;


