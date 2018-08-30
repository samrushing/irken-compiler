;; -*- Mode: Irken -*-

;; https://rosettacode.org/wiki/RSA_code

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")

(define (encrypt msg e n)
  (big (expmod msg e n)))

(define (decrypt msg d n)
  (big (expmod msg d n)))

(define (main)
  (let ((d (big (dec "5617843187844953170308463622230283376298685")))
        (n (big (dec "9516311845790656153499716760847001433441357")))
        (e (big (I 65537)))
        (text "rubber ducky")
        (text0 (b256->big text))
        (enc0 (encrypt text0 e n))
        (dec0 (decrypt enc0 d n)))
    (printf " plaintext " (string text) "\n")
    (printf "   as int  " (big-repr text0) "\n")
    (printf "   as txt  " (string (big->b256 text0)) "\n")
    (printf "encrypted  " (big-repr enc0) "\n")
    (printf "decrypted  " (big-repr dec0) "\n")
    (printf "   as txt  " (string (big->b256 dec0)) "\n")
    ))

(main)
