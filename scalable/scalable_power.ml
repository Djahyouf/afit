(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  if x = [] && n = []
  then
    [0;1]
  else
    let rec _pow exponant result =
      match exponant with
      | [] -> result
      | n -> _pow (diff_b n [0;1]) (mult_b x result)
    in _pow n [0;1]
;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)

let power x n =
  let rec _power base exponant =
    match exponant with
    | [] -> [0;1]
    | [0;1] -> base
    | n when mod_b n [0;0;1] = [] ->
       _power (pow base [0;0;1]) (quot_b exponant [0;0;1])
    | n -> mult_b base (_power  (pow base [0;0;1]) (quot_b (diff_b exponant [0;1]) [0;0;1]))
  in _power x n
;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)

let mod_power x n m =
  if x = []
  then
    []
  else
    let rec _modpow p x n =
      match (p,x,n) with
      | (p,_,[]) -> p
      | (p,x,e::l) when e = 1 ->
         _modpow (mod_b (mult_b p x) m) (mod_b (mult_b x x) m) l
      | (p,x,e::l) -> _modpow p (mod_b (mult_b x x) m) l
    in
    match n with
    | [] -> [0;1]
    | e::l -> _modpow [0;1] x l
;;



(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if x = []
  then
    []
  else
    mod_power x (mod_b n (diff_b p [0;1])) p
;;

