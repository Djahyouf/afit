(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  let rec _init currentBa =
    if currentBa >>= (add_b n [0;1])
    then
      []
    else
      if compare_b currentBa [0;0;1] = 0
      then
        [0;0;1]::_init (add_b currentBa [0;1])
      else
        if compare_b (mod_b currentBa [0;0;1])[] <> 0
        then
          currentBa :: _init (add_b currentBa [0;1])
        else
          _init (add_b currentBa [0;1])
    in _init [0;0;1]
;;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
 *)
(*
a function to remove a Bitarray from the init_eratosthenes list of bitarrays
 *)


let rec remove n list =
  match list with
  | [] -> []
  | e::l when compare_b (mod_b e n) [] = 0 && compare_b e n <> 0 ->
     remove n l
  | e::l -> e::remove n l
;;

let eratosthenes n =
  if sign_b n = (-1)
  then
    invalid_arg "give me a positive BitArray"
  else
    let rec _eras i list =
      if (mult_b i i) >> n
      then
          list
      else
        _eras (add_b i [0;1]) (remove i list)
    in
    _eras [0;1;1] (init_eratosthenes n)
;;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ();;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
 let rec _double list =
   match list with
   | [] -> []
   | e::l when isprime (add_b (mult_b e [0;0;1]) [0;1]) && isprime e ->
               (e, (add_b (mult_b e [0;0;1]) [0;1])) :: _double l
   | e::l -> _double l
 in _double (eratosthenes limit)
;;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =

 let primes = eratosthenes limit in

 let rec _twin primes =
   match primes with
   | [] -> []
   | e::l when compare_b e [0;0;1] = 0 -> _twin l
   | e::l when isprime (add_b e [0;0;1])
              -> (e, (add_b e [0;0;1])) :: _twin l
   | _::l -> _twin l
 in _twin primes
;;
