;; -*- Mode: Irken -*-

;; provide Ed448-Goldilocks (until it's added to libsodium).

(require-ffi 'decaf)

(define (string->u8* s)
  (%c-cast u8 (%string->cref #f s)))

(define (u8*->string s* len)
  (%cref->string #f (%c-cast char s*) len))

(define (ed448-derive-public-key skey)
  (when (not (= (string-length skey) DECAF_EDDSA_448_PRIVATE_BYTES))
    (raise (:DECAF/APIFail "ed448: skey wrong size")))
  (let ((pklen DECAF_EDDSA_448_PUBLIC_BYTES)
        (pk (halloc u8 pklen))
        (pk* (c-aref pk 0)))
    (decaf/decaf_ed448_derive_public_key pk* (string->u8* skey))
    (u8*->string pk* pklen)
    ))

(define (ed448-sign m sk pk)
  (let ((slen DECAF_EDDSA_448_SIGNATURE_BYTES)
        (s (halloc u8 slen))
        (s* (c-aref s 0))
        (m* (string->u8* m))
        (sk* (string->u8* sk))
        (pk* (string->u8* pk))
        (mlen (string-length m)))
    (decaf/decaf_ed448_sign s* sk* pk* m* mlen 0 NULL 0)
    (u8*->string s* slen)
    ))

(define (ed448-verify sig m pk)
  (let ((slen (string-length sig))
        (mlen (string-length m))
        (s* (string->u8* sig))
        (m* (string->u8* m))
        (pk* (string->u8* pk)))
    (= -1 (decaf/decaf_ed448_verify s* pk* m* mlen 0 NULL 0))
    ))

;; diffie-hellman

(define (x448-derive-public-key skey)
  (when (not (= (string-length skey) DECAF_X448_PRIVATE_BYTES))
    (raise (:DECAF/APIFail "x448: skey wrong size")))
  (let ((pklen DECAF_X448_PUBLIC_BYTES)
        (pk (halloc u8 pklen))
        (pk* (c-aref pk 0)))
    (decaf/decaf_x448_derive_public_key pk* (string->u8* skey))
    (u8*->string pk* pklen)
    ))

(require-ffi 'sodium)

(define (x448-gen-key)
  (let ((sk (make-string DECAF_X448_PRIVATE_BYTES))
        (sk* (string->u8* sk)))
    (sodium/randombytes_buf (%c-cast void sk*) DECAF_X448_PRIVATE_BYTES)
    {sk=sk pk=(x448-derive-public-key sk)}
    ))

(define (x448-gen-shared-key sk pk)
  (let ((ss (halloc u8 DECAF_X448_PUBLIC_BYTES))
        (ss* (c-aref ss 0)))
    (if (= -1 (decaf/decaf_x448 ss* (string->u8* pk) (string->u8* sk))) ;; note: order
        (u8*->string ss DECAF_X448_PUBLIC_BYTES)
        (raise (:DECAF/APIFail "decaf_x448"))
        )))
