(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b =
 match b with
 | 0 -> a
 | bb when 0 > bb -> gcd (-b) (-(modulo a b))
 | _ -> gcd b (modulo a b)
;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
 let rec _bezout (d,u,v, d1,u1,v1) =
  if d1 = 0
   then
    (u,v,d)
   else
    _bezout ( d1,u1,v1,
              (d-((quot d d1)*d1)),
              (u-((quot d d1)*u1)),
              (v-((quot d d1)*v1))  )
 in _bezout (a, 1, 0,  b, 0, 1)
;;
