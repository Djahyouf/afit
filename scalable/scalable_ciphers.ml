(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q = 
 
  let n = mult_b p q in

  let lambda_n = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in

  let e = [0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1] in

  let (d,vegeta,sangoku) = bezout_b e lambda_n in

  ((n,e),(n,d))
;;

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n
;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n
;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)

let randomBa bitarray =
 let rec build_rdm_binary = function
   | [] -> []
   | _::l -> (Random.int 1) :: build_rdm_binary l
 in quot_b (build_rdm_binary bitarray) [0;0;1]
;;

let rec public_data_g p =
 let g = abs_b (randomBa (abs_b p)) in (g,p)
;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = add_b (abs_b (randomBa (diff_b p [0;1]))) [0;1] in
  (mod_power g x p, x)
;;
(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = add_b (abs_b (randomBa (diff_b p [0;1]))) [0;1] in
  (mod_power g k p, mod_b (mult_b msg (mod_power kA k p)) p)
;; 

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
 let k = mod_power msgA a p in
 mod_b (mult_b msgB (mod_power k (diff_b p [0;0;1]) p )) p
;;
