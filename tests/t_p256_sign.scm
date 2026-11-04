;; -*- Mode: Irken -*-

;; lib/crypto/p256.scm: scalar range checks, public keys, and DER
;;  signatures.  tests/p256/verify_sign.py checks the output.

(require "lib/basis.scm")
(require "lib/urandom.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/p256.scm")

(for-list h (list "0000000000000000000000000000000000000000000000000000000000000000" ;; 0: no
                  "0000000000000000000000000000000000000000000000000000000000000001" ;; 1: yes
                  "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632550" ;; n-1: yes
                  "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551" ;; n: no
                  "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632552" ;; n+1: no
                  "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" ;; no
                  "00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") ;; yes
  (printf "ok? " h " " (bool->string (p256-scalar-ok? (hex->string h))) "\n"))

(define RNG (urandom-make))
(define sk (p256-make-key RNG))
(printf "sk " (string->hex sk) "\n")
(printf "pub " (string->hex (p256-public-key sk)) "\n")
(for-range i 2000
  (let ((m (format "message " (int i))))
    (printf "sig " (string->hex m) " " (string->hex (p256-sign m sk RNG)) "\n")))
