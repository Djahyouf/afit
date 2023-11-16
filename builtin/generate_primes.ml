(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
 let rec _eratosthenes x =
  match x with
  | x when x = n+1 -> []
  | 2 -> 2::_eratosthenes (x+1)
  | x when modulo x 2 <> 0 -> x:: _eratosthenes (x+1)
  | _ -> _eratosthenes (x+1)
 in _eratosthenes 2
;;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
 if n < 0
  then
   invalid_arg "eratosthenes: n < 0"
  else

   let rec removal n list =
    match list with
    | [] -> []
    | e::l when modulo e n = 0 && n <> e -> removal n l
    | e::l -> e:: removal n l
   in

   let rec _eras i era_li =
    if i*i > n
     then
      era_li
     else
      _eras (i+1) (removal i era_li)
   in _eras 3 (init_eratosthenes n)
;;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)


let write_list li file =

 let oc = open_out file in

 let rec writing list =
  match list with
  | [] -> close_out oc
  | e::l -> Printf.fprintf oc "%d\n" e ; writing l
 in writing li
;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)

let write_list_primes n file =
 write_list (eratosthenes n) file
;;


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c
;;

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)





let read_list_primes file =
 create_list (open_in file)
;;





(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
 let rec _double list =
  match list with
  | [] -> []
  | e::l when isprime e && isprime (2*e+1)
          -> (e,2*e+1) :: _double l
  | e::l -> _double l
 in _double (eratosthenes limit)
;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =

 let prime_li = eratosthenes limit in

 let rec _twin list =
  match list with
  | [] -> []
  | e::l when e = 2 -> _twin l
  | e::l when isprime (e+2)
          -> (e,(e+2)) :: _twin l
  | _::l -> _twin l
 in _twin prime_li
;;
