;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/hex.scm")

;; to prepare:
;;    $ ffi/gen/genffi -gen ffi/sodium.ffi
;;    $ sudo cp ffi/sodium_ffi.scm /usr/local/lib/irken/ffi/
;;
;; to build:
;;    $ self/compile tests/t_sodium.scm

(require-ffi 'sodium)

(define (sha256 m)
  (let ((h (make-string crypto_hash_sha256_BYTES))
        (h* (%c-cast uchar (%string->cref #f h)))
        (m* (%c-cast uchar (%string->cref #f m)))
        (len (string-length m)))
    (sodium/crypto_hash_sha256 h* m* len)
    (printf "input:  " (string m) "\n")
    (printf "sha256: " (string->hex h) "\n")
    ))

(define (sha512 m)
  (let ((h (make-string crypto_hash_sha512_BYTES))
        (h* (%c-cast uchar (%string->cref #f h)))
        (m* (%c-cast uchar (%string->cref #f m)))
        (len (string-length m)))
    (sodium/crypto_hash_sha512 h* m* len)
    (printf "input:  " (string m) "\n")
    (printf "sha512: " (string->hex h) "\n")
    ))

(define (ed25519-sign m sk)
  (let ((slen (sodium/crypto_sign_ed25519_bytes))
        (s (halloc uchar slen))
        (slen* (halloc ulonglong)) ;; out param
        (s* (c-aref s 0))
        (m* (%c-cast uchar (%string->cref #f m)))
        (sk* (%c-cast uchar (%string->cref #f sk)))
        (mlen (string-length m)))
    (c-set-int slen* slen)
    (let ((r (sodium/crypto_sign_ed25519_detached s* slen* m* mlen sk*))
          (rlen (c-get-int slen*)))
      (%cref->string #f (%c-cast char s*) rlen))))

;; (sig crypto_sign_ed25519_verify_detached ((* uchar) (* uchar) ulonglong (* uchar) -> int))
;; sig, m, mlen, pk

(define (ed25519-verify sig m pk)
  (let ((pk* (%c-cast uchar (%string->cref #f pk)))
        (m* (%c-cast uchar (%string->cref #f m)))
        (sig* (%c-cast uchar (%string->cref #f sig)))
        (mlen (string-length m)))
    (= 0 (sodium/crypto_sign_ed25519_verify_detached sig* m* mlen pk*))
    ))

;; (sig crypto_sign_ed25519_seed_keypair ((* uchar) (* uchar) (* uchar) -> int))
(define (ed25519-seed-to-keypair seed)
  (when (not (= (string-length seed) (sodium/crypto_sign_ed25519_seedbytes)))
    (raise (:Sodium/APIFail "ed25519: seed wrong size")))
  (let ((sklen (sodium/crypto_sign_ed25519_secretkeybytes))
        (pklen (sodium/crypto_sign_ed25519_publickeybytes))
        (sk (halloc uchar sklen))
        (pk (halloc uchar pklen))
        (r (sodium/crypto_sign_ed25519_seed_keypair
            (c-aref pk 0) (c-aref sk 0)
            (%c-cast uchar (%string->cref #f seed)))))
    (if (= 0 r)
        (:tuple
         (%cref->string #f (%c-cast char (c-aref sk 0)) sklen)
         (%cref->string #f (%c-cast char (c-aref pk 0)) pklen))
        (raise (:Sodium/APIFail "crypto_sign_ed25519_seed_keypair")))))

(define ed25519-skey (hex->string "b84ffaaf6fd47d62113b61df1fbb994d61848168a7dece5e151140c6bdfb4073"))

(define (curve25519-scalarmult-base k)
  (let ((qlen (sodium/crypto_scalarmult_curve25519_bytes))
        (q (make-string qlen)))
    (sodium/crypto_scalarmult_curve25519_base
     (%c-cast uchar (%string->cref #f q))
     (%c-cast uchar (%string->cref #f k)))
    q))

(define (test-c25519-scalarmult)
  (let ((skey (hex->string "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"))
        (pkey (curve25519-scalarmult-base skey)))
    (printf "scalarmult:\n")
    (printf "pkey " (string->hex pkey) "\n")
    (printf "     8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a\n")
    ))

(define msg "testing, testing!\n")

(sha256 msg)
(sha512 msg)

(printf "|sig|  " (int (sodium/crypto_sign_ed25519_bytes)) "\n")
(printf "|pkey| " (int (sodium/crypto_sign_ed25519_publickeybytes)) "\n")
(printf "|skey| " (int (sodium/crypto_sign_ed25519_secretkeybytes)) "\n")
(let (((sk pk) (ed25519-seed-to-keypair ed25519-skey))
      (sig (ed25519-sign msg sk)))
  (printf "seed " (string->hex ed25519-skey) "\n")
  (printf "sk   " (string->hex sk) "\n")
  (printf "pk   " (string->hex pk) "\n")
  (printf "sig  " (string->hex sig) "\n")
  (printf "verify: " (bool (ed25519-verify sig msg pk)) "\n")
  )

(test-c25519-scalarmult)
