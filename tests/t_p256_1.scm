;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/urandom.scm")
(require "lib/codecs/hex.scm")
(require "lib/codecs/base64.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/pem.scm")
(require "lib/asn1/ber.scm")
(require "demo/bignum.scm")

(require-ffi 'bearssl)

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

  (define (churn) ;; an exaggeration of how often this loops
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
      (if (= plen 65)
          {sk=bytes pk=kbuf}
          (raise (:BearSSL/KeyGenFailed plen))))))

(define (key->ber skey pkey)
  (ber:SEQUENCE
   (list
    (ber:INTEGER 1)
    (OCTETSTRING skey)
    (ber:CONTEXT 0 (list (ber:OID (list 1 2 840 10045 3 1 7))))
    (ber:CONTEXT 1 (list (BITSTRING0 pkey)))
    )))

(define (main)
  (let ((key (genkey))
        (key-ber (key->ber key.sk key.pk))
        (key-enc (asn1->ber key-ber)))
    ;; (pp-ber key-ber 0)
    ;; (printf "\n")
    ;; (printf (string->hex key-enc) "\n")
    (for line (pem-write-gen (string-generator key-enc) "EC PRIVATE KEY")
         (printf line)
         )))

(main)
