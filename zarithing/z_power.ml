(** Power function implementations for big integers *)

open Z

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec _modpow _x _n =
    if leq n _n
    then
      _x
    else
      _modpow (erem (mul x _x) m) (succ _n)
  in _modpow one zero
;;


(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  match x with
  | x when x = zero -> zero
  | _ -> mod_power x (erem n (pred p)) p
;;

