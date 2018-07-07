;; -*- Mode: Irken -*-

(define (make-x25519-kex)
  (let ((key {sk="" pk=""}))
    (define (gen)
      (set! key (x25519-gen-key)))
    (define (get-pub) key.pk)
    (define (gen-shared pk1)
      (x25519-gen-shared-key key.sk pk1))
    {gen=gen get-pub=get-pub gen-shared=gen-shared size=32 group=(named-group:x25519)}
    ))

(define (make-x448-kex)
  (let ((key {sk="" pk=""}))
    (define (gen)
      (set! key (x448-gen-key)))
    (define (get-pub) key.pk)
    (define (gen-shared pk1)
      (x448-gen-shared-key key.sk pk1))
    {gen=gen get-pub=get-pub gen-shared=gen-shared size=56 group=(named-group:x448)}
    ))

(define make-kex
  (named-group:x25519) -> (make-x25519-kex)
  (named-group:x448)   -> (make-x448-kex)
  group                -> (raise (:TLS/Fatal (tls-alert-desc:hsk-failure) "no shared kex"))
  )
