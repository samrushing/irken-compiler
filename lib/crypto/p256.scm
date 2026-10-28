;; -*- Mode: Irken -*-

;; P-256 (secp256r1) ECDSA signing, using include/p256.
;;
;; secret keys, nonces and the r, s, x, y values are all 32-byte
;;  big-endian strings.

(require-ffi 'p256_ecdsa)

;; the group order n, big-endian.
(define p256-n
  (string-append
   "\xff\xff\xff\xff\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff"
   "\xbc\xe6\xfa\xad\xa7\x17\x9e\x84\xf3\xb9\xca\xc2\xfc\x63\x25\x51"))

(define (p256-string->u8* s)
  (%c-cast uchar (%string->cref #f s)))

;; is the 32-byte big-endian string s a valid scalar, i.e. 1 <= s < n?
;; the candidate becomes a nonce or a secret key, so this looks at
;;  every byte and doesn't branch on their values: even knowing
;;  whether the top byte is zero helps lattice attacks on ECDSA.
(define (p256-scalar-ok? s)
  (let ((lt 0)     ;; 1 once we know s < n
        (gt 0)     ;; 1 once we know s > n
        (acc 0))   ;; OR of all the bytes
    (for-range i 32
      (let ((a (char->int (string-ref s i)))
            (b (char->int (string-ref p256-n i)))
            (undecided (logxor 1 (logior lt gt))))
        ;; (x >> 8) & 1 is 1 iff x < 0, for -255 <= x <= 255.
        (set! lt (logior lt (logand undecided (logand 1 (>> (- a b) 8)))))
        (set! gt (logior gt (logand undecided (logand 1 (>> (- b a) 8)))))
        (set! acc (logior acc a))))
    (and (= lt 1) (> acc 0))))

;; a uniformly random scalar in [1, n-1].
(define (p256-random-scalar RNG)
  (let loop ((k (RNG 32)))
    (if (p256-scalar-ok? k)
        k
        (loop (RNG 32)))))

;; generate a new secret key.
(define (p256-make-key RNG)
  (p256-random-scalar RNG))

;; the public key for <sk>, as a 65-byte uncompressed point: 04 || x || y.
(define (p256-public-key sk)
  (let ((x (make-string 32))
        (y (make-string 32)))
    (p256_ecdsa/p256_public_key
     (p256-string->u8* x)
     (p256-string->u8* y)
     (p256-string->u8* sk))
    (string-append "\x04" x y)))

;; sign message <m> with secret key <sk>, returning the DER-encoded
;;  ECDSA-Sig-Value: SEQUENCE { INTEGER r, INTEGER s }
(define (p256-sign m sk RNG)
  (let ((h (sha256 m))
        (k (p256-random-scalar RNG))
        (r (make-string 32))
        (s (make-string 32)))
    (p256_ecdsa/p256_ecdsa_sign
     (p256-string->u8* r)
     (p256-string->u8* s)
     (p256-string->u8* sk)
     (p256-string->u8* k)
     (p256-string->u8* h))
    (asn1->ber
     (ber:SEQUENCE
      (list (ber/uint r)
            (ber/uint s)
            )))
    ))
