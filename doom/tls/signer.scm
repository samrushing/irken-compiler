;; -*- Mode: Irken -*-

;; for ed25519,rsa, etc.
(require "lib/crypto/sig.scm")
;; for ed448, for now.
;; ED448
;;(require "lib/crypto/decaf.scm")


;; for p256, for now.
;;(require "lib/crypto/bearssl.scm")
;; p256_ecdsa (based on fiat-crypto)
(require "lib/crypto/p256.scm")

;; these objects are responsible for signing with the cert's private key.

(define (make-rsassa-pss-rsae-sha256-signer skey)
  (define (sign tbs RNG)
    (rsassa-pss-sign skey tbs sha256-hash RNG))
  {sign=sign sigalg=(sigalg:rsa-pss-rsae-sha256)}
  )

(define (make-ed25519-signer skey)
  ;; skey is actually the seed.
  ;; see: https://blog.mozilla.org/warner/2011/11/29/ed25519-keys/
  (let (((sk pk) (ed25519-seed-to-keypair skey)))
    {sign=(lambda (tbs RNG) (ed25519-sign tbs sk)) sigalg=(sigalg:ed25519)}
    ))

;; ED448
;; (define (make-ed448-signer skey)
;;   (let ((pkey (ed448-derive-public-key skey)))
;;     (define (sign tbs RNG)
;;       (ed448-sign tbs skey pkey))
;;     {sign=sign sigalg=(sigalg:ed448)}
;;     ))

(define (make-p256-signer skey)
  (define (sign tbs RNG) (p256-sign tbs skey RNG))
  {sign=sign sigalg=(sigalg:ecdsa-secp256r1-sha256)}
  )

(define make-signer
  (skey:rsa skey)     -> (make-rsassa-pss-rsae-sha256-signer skey)
  (skey:ed25519 skey) -> (make-ed25519-signer skey)
;;  (skey:ed448 skey)   -> (make-ed448-signer skey)
  (skey:p256 skey)    -> (make-p256-signer skey)
  )

