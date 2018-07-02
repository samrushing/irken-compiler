;; -*- Mode: Irken -*-

;; for now: X25519 only
;; note: until X448 is added to libsodium, it is in lib/decaf.scm.

(require-ffi 'sodium)

;; (-> {sk=string pk=string})
(define (x25519-gen-key)
  (let ((sk (halloc uchar 32))
        (pk (halloc uchar 32)))
    (sodium/randombytes_buf (%c-cast void sk) 32)
    (sodium/crypto_scalarmult_curve25519_base (c-aref pk 0) (c-aref sk 0))
    {sk=(cref->string (%c-cast char sk) 32)
     pk=(cref->string (%c-cast char pk) 32)}
    ))

;; string -> {sk=string pk=string}
(define (x25519-skey->keypair secret-key)
  (let ((sk (halloc uchar 32))
        (pk (halloc uchar 32)))
    (for-range i 32 ;; copy skey into foreign buffer.
      (c-set-int (%c-cast u8 (c-aref sk i)) (char->int (string-ref secret-key i))))
    (sodium/crypto_scalarmult_curve25519_base (c-aref pk 0) (c-aref sk 0))
    {sk=(cref->string (%c-cast char sk) 32)
     pk=(cref->string (%c-cast char pk) 32)}
    ))

;; string string -> string
(define (x25519-gen-shared-key sk pk)
  (let ((ss (halloc uchar 32)))
    (if (= 0 (sodium/crypto_scalarmult_curve25519
              (c-aref ss 0)
              (%c-cast uchar (cstring sk))
              (%c-cast uchar (cstring pk))))
        (cref->string (%c-cast char (c-aref ss 0)) 32)
        (raise (:Sodium/APIFail "crypto_scalarmult_curve25519"))
        )))
