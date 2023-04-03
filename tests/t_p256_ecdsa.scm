;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/bignum.scm")
(require "lib/codecs/hex.scm")

(require-ffi 'p256_ecdsa)

;; p256_ecdsa_sign (uint8_t out_r[32], uint8_t out_s[32],const uint8_t sk[32], const uint8_t k[32], const uint8_t h[32])
(define sk (hex->string "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"))
(define k (hex->string "a6e3c57dd01abe90086538398355dd4c3b17aa873382b0f24d6129493d8aad60"))
(define h (hex->string "af2bdbe1aa9b6ec1e2ade1d694f41fc71a831d0268e9891562113d8a62add1bf"))

(define (string->u8* s)
  (%c-cast uchar (%string->cref #f s)))

(let ((r (make-string 32))
      (s (make-string 32)))
  (p256_ecdsa/p256_ecdsa_sign
   (string->u8* r)
   (string->u8* s)
   (string->u8* sk)
   (string->u8* k)
   (string->u8* h))
  (:tuple r s)
  )
