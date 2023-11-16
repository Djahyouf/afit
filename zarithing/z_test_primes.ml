(** Testing for primality *)

open Z
open Z_power
;;


(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
  let rec _prime i =
    match i with
    | i when equal i n -> true
    | i when erem n i <> zero  -> _prime (succ i)
    | _ -> false
  in _prime (succ one)
;;
