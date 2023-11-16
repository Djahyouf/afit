(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
 let rec _pow a n_ =
  match n_ with
  | 0 -> 1
  | _ -> a * _pow a (n_ -1)
 in _pow x n
;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let rec power x n =
 match n with
 | 0 -> 1
 | 1 -> x
 | n when n mod 2 = 0 -> power (x*x) (quot n 2)
 | n -> x * power (x*x) (quot (n-1) 2)
;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
 let rec _modpow _x _n =
  if n <= _n
   then
    _x
   else
    _modpow (modulo (x*_x) m) (_n+1)
 in _modpow 1 0
;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
 match x with
 | 0 -> 0
 | _ -> mod_power x (modulo n (p-1)) p
;;
