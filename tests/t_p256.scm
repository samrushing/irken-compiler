;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/hex.scm")
(require "lib/urandom.scm")
(require "lib/crypto/sodium.scm")
(require "demo/bignum.scm")
(require-ffi 'bearssl)

;; (ecdsa-secp256r1-sha256 #x0403)

(define raw-key
  (hex->string
   "C9AFA9D845BA75166B5C215767B1D6934E50C3DB36E89B127B8A622B120F6721" ;; test vector
   ))


(define msg
  ;;"the quick brown fox jumped over the lazy dog"
  "sample" ;; test vector
  )

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; XXX we are not checking that the random key is
;;           less than the prime.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(define P256 (u256->big (hex->string "ffffffff00000001000000000000000000000000ffffffffffffffffffffffff")))

(define (check-prime-order bytes)
  (let ((bn (u256->big bytes)))
    (big< bn P256)
    ))

(define (build-skey* sk)
  (let ((skey* (halloc (struct br_ec_private_key))))
    (c-set-int (%c-sref br_ec_private_key.curve skey*) BR_EC_secp256r1)
    (c-set-ptr (%c-sref br_ec_private_key.x skey*) (%c-cast uchar (%string->cref #f sk)))
    (c-set-int (%c-sref br_ec_private_key.xlen skey*) (string-length sk))
    skey*))

(define (genkey)

  (define rng (urandom-make))

  (define (churn)
    (let loop ((cand (rng 32)))
      (if (check-prime-order cand)
          cand
          (loop (rng 32)))))

  (let ((rng (urandom-make))
        (bytes (churn))
        (skey* (build-skey* bytes))
        (pkey* (halloc (struct br_ec_public_key)))
        (kbuf (make-string 65))
        (kbuf* (%c-cast void (%string->cref #f kbuf))))
    (let ((plen (bearssl/br_ec_compute_pub (bearssl/br_ec_p256_m64_get) pkey* kbuf* skey*)))
      (printf "skey " (string->hex bytes) "\n")
      (if (> plen 0)
          (printf "public key " (string->hex kbuf) "\nplen = " (int plen) "\n")
          (printf "compute_pub failed.\n"))
      )))

(define (go)
  (let ((skey* (build-skey* raw-key))
        (sigbuf (make-string 64))
        (sigbuf* (%c-cast void (%string->cref #f sigbuf)))
        (hash (sha256 msg))
        (hash* (%c-cast void (%string->cref #f hash))))
    (let ((rlen (bearssl/br_ecdsa_i31_sign_raw
                 (bearssl/br_ec_p256_m64_get)
                 bearssl/br_sha256_vtable
                 hash* skey* sigbuf*)))
      (printf "rlen = " (int rlen) "\n")
      (when (= rlen 64)
        (printf "r = " (string->hex (substring sigbuf  0 32)) "\n")
        (printf "s = " (string->hex (substring sigbuf 32 64)) "\n"))
      )))

(go)
(genkey)
