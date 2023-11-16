(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
  let rec _prime i =
    match i with
    | i when i = n -> true
    | i when mod_b n i <> [] -> _prime (add_b i [0;1])
    | _ -> false
  in _prime [0;0;1]
;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec _psdprime sequence =
    match sequence with
    | [] -> true
    | e::l -> if mod_power e (diff_b p [0;1]) p = [0;1] || (<<=) p [0;0;1]
              then
                _psdprime l
              else
                false
  in _psdprime test_seq
;;
