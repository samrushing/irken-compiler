;; -*- Mode: Irken -*-

(require-ffi 'p256_ecdsa)

(define (p256-sign m sk RNG)

  (define (string->u8* s)
    (%c-cast uchar (%string->cref #f s)))

  (define (maybe-pad-bigint s)
    (let ((b0 (char->int (string-ref s 0))))
      (if (>0 (logand b0 #x80))
          (string-append "\x00" s)
          s)))

  (let ((h (sha256 m))
        (k (RNG 32))
        (r (make-string 32))
        (s (make-string 32)))
    (p256_ecdsa/p256_ecdsa_sign
     (string->u8* r)
     (string->u8* s)
     (string->u8* sk)
     (string->u8* k)
     (string->u8* h))
    (asn1->ber
     (ber:SEQUENCE
      (list (ber:BINTEGER (maybe-pad-bigint r))
            (ber:BINTEGER (maybe-pad-bigint s))
            )))
    ))

