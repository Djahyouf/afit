(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,p)  = key in
  let rec breaking e =
    if gt e n
    then
      (zero,zero)
    else
      if equal (erem n e) zero
      then
        (e,div n e)
      else
        breaking (succ e)
    in breaking (succ one)
;;
