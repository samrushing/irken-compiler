;; -*- Mode: Irken -*-

(require-ffi 'bearssl)

(define (p256-build-key sk)
  (let ((skey* (halloc (struct br_ec_private_key))))
    (c-set-int (%c-sref br_ec_private_key.curve skey*) BR_EC_secp256r1)
    (c-set-ptr (%c-sref br_ec_private_key.x skey*) (%c-cast uchar (%string->cref #f sk)))
    (c-set-int (%c-sref br_ec_private_key.xlen skey*) (string-length sk))
    skey*))

(define (maybe-pad-bigint s)
  (let ((b0 (char->int (string-ref s 0))))
    (if (>0 (logand b0 #x80))
        (string-append "\x00" s)
        s)))

(define (p256-sign m sk RNG)
  (let ((skey* (p256-build-key sk))
        (sigbuf (make-string 64))
        (sigbuf* (%c-cast void (%string->cref #f sigbuf)))
        (hash (sha256 m))
        (hash* (%c-cast void (%string->cref #f hash))))
    (let ((rlen (bearssl/br_ecdsa_i31_sign_raw
                 (bearssl/br_ec_p256_m64_get)
                 bearssl/br_sha256_vtable
                 hash* skey* sigbuf*)))
      (assert (= rlen 64))
      (let ((r (maybe-pad-bigint (substring sigbuf  0 32)))
            (s (maybe-pad-bigint (substring sigbuf 32 64))))
        (asn1->ber
         (ber:SEQUENCE
          (list (ber:BINTEGER r)
                (ber:BINTEGER s)))))
      )))
