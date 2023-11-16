(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let len = String.length str in
  let rec _encode str len i a res =
    if len = 0
    then
      res
    else

      let scl = power [0;0;1] (from_int bits) in

      _encode str (len-1) (i-1) (mult_b scl a)
                  (add_b res (mult_b a (from_int (int_of_char str.[i]))))

  in _encode str len (len-1) [0;1] []
;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
  
  let rec _decode bits msg =

  let chr = mod_b msg (power [0;0;1] bits) in

  let _msg = quot_b msg (power [0;0;1] bits) in

  if compare_b msg [] = 0
  then
    ""
  else
    (_decode bits _msg) ^ (Char.escaped (char_of_int (to_int chr)))

  in _decode (from_int bits) msg 
;;
