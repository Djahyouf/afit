(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
 let rec _prime i =
  match i with
  | i when i = n -> true
  | i when modulo n i <> 0 -> _prime (i+1)
  | _ -> false
 in _prime 2
;;

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
 let rec _psdprime sequence =
  match sequence with
  | [] -> true
  | e::l -> if mod_power e (p-1) p = 1 ||
               p <= 2
             then
              _psdprime l
             else
              false
 in _psdprime test_seq
;;
