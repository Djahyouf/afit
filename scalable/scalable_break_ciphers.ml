(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,p) = key in
  let rec breaking e =
    if e >> n
    then
      ([],[])
    else
      if mod_b n e = []
      then
        (e, quot_b n e)
      else
        breaking (add_b e [0;1])
  in breaking [0;0;1]
;;
