(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
    let len = String.length str in
    let rec _encode str len i a res =
        if len = 0
         then
          res
         else

          let scl = power 2 bits in

          _encode str (len-1) (i-1) (scl * a) ((int_of_char str.[i])*a+res)

    in _encode str len (len-1) 1 0
;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =

  let rec la_div n x i =
    if n mod x = 0
    then
      i
    else
      la_div (n-1) x (i+1)
  in

  let rec _decode bits msg =

    let chr = la_div msg (power 2 bits) 0 in

    let _msg = (msg - chr) / (power 2 bits) in

    if msg = 0
    then
      ""
    else
      (_decode bits _msg) ^ (Char.escaped (char_of_int chr))
  in _decode bits msg
;;
