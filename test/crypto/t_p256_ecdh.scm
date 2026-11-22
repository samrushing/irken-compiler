;; -*- Mode: Irken -*-

;; P-256 ECDH (include/p256, lib/crypto/p256.scm): rfc5903 section 8.1,
;;  both ways; and points that must be refused.

;; skip: b -- the VM can't link include/p256's C.

(include "lib/basis.scm")
(require "lib/codecs/hex.scm")
(require "lib/crypto/sodium.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/p256.scm")

(define i (hex->string "c88f01f510d9ac3f70a292daa2316de544e9aab8afe84049c62a9c57862d1433"))
(define r (hex->string "c6ef9c5d78ae012a011164acb397ce2088685d8f06bf9be0b283ab46476bee53"))

(define (show m)
  (match m with
    (maybe:yes x) -> (printf (string->hex x) "\n")
    (maybe:no)    -> (printf "refused\n")))

(let ((gi (p256-public-key i))
      (gr (p256-public-key r)))
  (printf (string->hex (substring gi 1 33)) "\n")
  (printf (string->hex (substring gr 1 33)) "\n")
  (show (p256-ecdh i gr))
  (show (p256-ecdh r gi))
  ;; off the curve
  (show (p256-ecdh i (string-append (substring gr 0 64) (int->string 7))))
  ;; x = p
  (show (p256-ecdh i (string-append "\x04" (hex->string "ffffffff00000001000000000000000000000000ffffffffffffffffffffffff") (substring gr 33 65))))
  ;; not uncompressed, wrong length
  (show (p256-ecdh i (string-append "\x02" (substring gr 1 33))))
  (show (p256-ecdh i "")))

;; => dad0b65394221cf9b051e1feca5787d098dfe637fc90b9ef945d0c3772581180
;; => d12dfb5289c8d4f81208b70270398c342296970a0bccb74c736fc7554494bf63
;; => d6840f6b42f6edafd13116e0e12565202fef8e9ece7dce03812464d04b9442de
;; => d6840f6b42f6edafd13116e0e12565202fef8e9ece7dce03812464d04b9442de
;; => refused
;; => refused
;; => refused
;; => refused
