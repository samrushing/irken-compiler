;; -*- Mode: Irken -*-

;; BINTEGER encodes minimally, and ber/uint keeps unsigned values positive.

(require "lib/basis.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")

(define (check what ob expected)
  (let ((got (string->hex (asn1->ber ob))))
    (printf (if (string=? got expected) "ok   " "FAIL ") what " " got
            (if (string=? got expected) "" (format " expected " expected)) "\n")))

(define (B h) (ber:BINTEGER (hex->string h)))
(define (U h) (ber/uint (hex->string h)))

(check "0"            (B "00")       "020100")
(check "000000"       (B "000000")   "020100")
(check "007f"         (B "007f")     "02017f")
(check "0080"         (B "0080")     "02020080")
(check "000080"       (B "000080")   "02020080")
(check "ff80 (-128)"  (B "ff80")     "020180")
(check "ff7f (-129)"  (B "ff7f")     "0202ff7f")
(check "ffff (-1)"    (B "ffff")     "0201ff")
(check "empty"        (B "")         "020100")
(check "uint 80"      (U "80")       "02020080")
(check "uint 0001"    (U "0001")     "020101")
(check "uint 00ff"    (U "00ff")     "020200ff")
(check "uint 7f"      (U "7f")       "02017f")
