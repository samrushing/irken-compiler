;; -*- Mode: Irken -*-

(require "lib/crypto/dh.scm")
(require "lib/crypto/sodium.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/p256.scm")
(require "lib/urandom.scm")

;;(require "lib/crypto/decaf.scm")

(define (make-x25519-kex)
  (let ((key {sk="" pk=""}))
    (define (gen)
      (set! key (x25519-gen-key)))
    (define (get-pub) key.pk)
    (define (gen-shared pk1)
      (x25519-gen-shared-key key.sk pk1))
    {gen=gen get-pub=get-pub gen-shared=gen-shared size=32 group=(named-group:x25519)}
    ))

;; secp256r1 (rfc8446 4.2.8.2): the key share is an uncompressed point,
;;  04 || x || y, and the shared secret is the x coordinate.  (postgres
;;  before version 18 offers only this.)
(define (make-p256-kex)
  (let ((sk "")
        (pk ""))
    (define (gen)
      (set! sk (p256-make-key (urandom-make)))
      (set! pk (p256-public-key sk)))
    (define (get-pub) pk)
    (define (gen-shared pk1)
      (match (p256-ecdh sk pk1) with
        (maybe:yes secret) -> secret
        (maybe:no) -> (raise (:TLS/Alert (tls-alert-desc:illegal-parameter) "bad secp256r1 key share"))))
    {gen=gen get-pub=get-pub gen-shared=gen-shared size=65 group=(named-group:secp256r1)}
    ))

;; ED448
;; (define (make-x448-kex)
;;   (let ((key {sk="" pk=""}))
;;     (define (gen)
;;       (set! key (x448-gen-key)))
;;     (define (get-pub) key.pk)
;;     (define (gen-shared pk1)
;;       (x448-gen-shared-key key.sk pk1))
;;     {gen=gen get-pub=get-pub gen-shared=gen-shared size=56 group=(named-group:x448)}
;;     ))

(define make-kex
  (named-group:x25519)    -> (make-x25519-kex)
  (named-group:secp256r1) -> (make-p256-kex)
;; ED448
;;  (named-group:x448)   -> (make-x448-kex)
  group                -> (raise (:TLS/Fatal (tls-alert-desc:hsk-failure) "no shared kex"))
  )
