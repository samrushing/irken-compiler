;; -*- Mode: Irken -*-

(require-ffi 'sodium)

(defmacro sodium
  (sodium fun arg ...)
  -> (when (not (= 0 (fun arg ...)))
       (raise (:Sodium/APIFail (%%stringify fun)))))

(define (string->uchar s)
  (%c-cast uchar (%string->cref #f s)))

;; ---- utility ----

(define (constant-time-string=? a b)
  (let ((alen (string-length a))
        (blen (string-length b)))
    (if (= alen blen)
        (= 0
           (sodium/sodium_memcmp
            (%c-cast void (%string->cref #f a))
            (%c-cast void (%string->cref #f b))
            alen))
        (raise (:Sodium/LengthMismatch a b)))))

;; ---- sha256 ----

(define (sha256 m)
  (let ((h (make-string crypto_hash_sha256_BYTES))
        (h* (string->uchar h))
        (m* (string->uchar m))
        (len (string-length m)))
    (sodium sodium/crypto_hash_sha256 h* m* len)
    h
    ))

(define (sha256/make)
  (let ((state* (halloc (struct crypto_hash_sha256_state))))
    (define (update m)
      (let ((m* (string->uchar m))
            (mlen (string-length m)))
        (sodium sodium/crypto_hash_sha256_update state* m* mlen)))
    ;; XXX should probably be named 'digest' rather than 'final',
    ;;   since we are copying the state object.
    (define (final)
      (let ((h (make-string crypto_hash_sha256_BYTES))
            (h* (string->uchar h))
            (state0* (halloc (struct crypto_hash_sha256_state))))
        (libc/memcpy (%c-cast void state0*) (%c-cast void state*)
                     (%c-sizeof (struct crypto_hash_sha256_state)))
        (sodium sodium/crypto_hash_sha256_final state0* h*)
        h
        ))
    (sodium sodium/crypto_hash_sha256_init state*)
    {update=update final=final}
    ))

(define sha256-hash {size=32 block-size=64 make=sha256/make oneshot=sha256})

;; ---- sha512 ----

(define (sha512 m)
  (let ((h (make-string crypto_hash_sha512_BYTES))
        (h* (string->uchar h))
        (m* (string->uchar m))
        (len (string-length m)))
    (sodium sodium/crypto_hash_sha512 h* m* len)
    h
    ))

(define (sha512/make)
  (let ((state* (halloc (struct crypto_hash_sha512_state))))
    (define (update m)
      (let ((m* (string->uchar m))
            (mlen (string-length m)))
        (sodium sodium/crypto_hash_sha512_update state* m* mlen)))
    (define (final)
      (let ((h (make-string crypto_hash_sha512_BYTES))
            (h* (string->uchar h))
            (state0* (halloc (struct crypto_hash_sha512_state))))
        (libc/memcpy (%c-cast void state0*) (%c-cast void state*)
                     (%c-sizeof (struct crypto_hash_sha512_state)))
        (sodium sodium/crypto_hash_sha512_final state0* h*)
        h))
    (sodium sodium/crypto_hash_sha512_init state*)
    {update=update final=final}
    ))

(define sha512-hash {size=64 block-size=128 make=sha512/make oneshot=sha512})

;; ---- sha384 ----
;;
;; libsodium doesn't explicitly support sha384.
;;
;; however, sha512 & sha384 share the same code,
;;  but for the initial state vector.

;; XXX should probably go in lib/ctype
(define big-endian?
  (let ((val0* (halloc u32 1))
        (val1* (%c-cast (array u8) val0*)))
    (define (get n)
      (c-get-int (c-aref val1* n)))
    (c-set-int (c-aref val0* 0) #xf00fda)
    (match (get 0) (get 1) (get 2) with
      #xda #x0f #xf0 -> #f
      #xf0 #x0f #xda -> #t
      _ _ _          -> (raise (:I_Am_Confused))
      )))

(define sha384-init-state
  ;; we fill in the initial state as uint32_t[16], swapping
  ;;   pairs if little-endian.
  (let ((v0 #(#xcbbb9d5d #xc1059ed8 #x629a292a #x367cd507
              #x9159015a #x3070dd17 #x152fecd8 #xf70e5939
              #x67332667 #xffc00b31 #x8eb44a87 #x68581511
              #xdb0c2e0d #x64f98fa7 #x47b5481d #xbefa4fa4))
        (v1 (halloc u32 16)))
    (for-range i 8
      (let ((i0 (* i 2))
            (i1 (+ 1 i0)))
        (cond (big-endian?
               (c-set-int (c-aref v1 i0) v0[i0])
               (c-set-int (c-aref v1 i1) v0[i1]))
              (else
               (c-set-int (c-aref v1 i0) v0[i1])
               (c-set-int (c-aref v1 i1) v0[i0])))))
    v1 ;; (cref u32)
    ))

(define (sha384/make)
  (let ((state* (halloc (struct crypto_hash_sha512_state))))
    (define (update m)
      (let ((m* (string->uchar m))
            (mlen (string-length m)))
        (sodium sodium/crypto_hash_sha512_update state* m* mlen)))
    (define (final)
      (let ((h (make-string crypto_hash_sha512_BYTES))
            (h* (string->uchar h))
            (state0* (halloc (struct crypto_hash_sha512_state))))
        (libc/memcpy (%c-cast void state0*) (%c-cast void state*)
                     (%c-sizeof (struct crypto_hash_sha512_state)))
        (sodium sodium/crypto_hash_sha512_final state0* h*)
        ;; truncate to 384 bits.
        (substring h 0 48)))
    (sodium sodium/crypto_hash_sha512_init state*)
    ;; note: this hack has knowledge of (struct crypto_hash_sha512_state).
    ;; copy over the sha512 initial state with sha384's.
    (libc/memcpy (%c-cast void state*) (%c-cast void sha384-init-state) 64)
    {update=update final=final}
    ))

(define (sha384 m)
  (let ((h (sha384/make)))
    (h.update m)
    (h.final)))

(define sha384-hash {size=48 block-size=128 make=sha384/make oneshot=sha384})

;; ---- ed25519 ----

;; there are several different non-standards for how ed25519 keys are managed.
;; https://blog.mozilla.org/warner/2011/11/29/ed25519-keys/

(define (ed25519-sign m sk)
  (let ((slen (sodium/crypto_sign_ed25519_bytes))
        (s (halloc uchar slen))
        (slen* (halloc ulonglong)) ;; out param
        (s* (c-aref s 0))
        (m* (string->uchar m))
        (sk* (string->uchar sk))
        (mlen (string-length m)))
    (c-set-int slen* slen)
    (let ((r (sodium/crypto_sign_ed25519_detached s* slen* m* mlen sk*))
          (rlen (c-get-int slen*)))
      (%cref->string #f (%c-cast char s*) rlen))))

(define (ed25519-verify sig m pk)
  (let ((pk* (string->uchar pk))
        (m* (string->uchar m))
        (sig* (string->uchar sig))
        (mlen (string-length m)))
    (= 0 (sodium/crypto_sign_ed25519_verify_detached sig* m* mlen pk*))
    ))

(define (ed25519-seed-to-keypair seed)
  (when (not (= (string-length seed) (sodium/crypto_sign_ed25519_seedbytes)))
    (raise (:Sodium/APIFail "ed25519: seed wrong size")))
  (let ((sklen (sodium/crypto_sign_ed25519_secretkeybytes))
        (pklen (sodium/crypto_sign_ed25519_publickeybytes))
        (sk (halloc uchar sklen))
        (pk (halloc uchar pklen)))
    (sodium sodium/crypto_sign_ed25519_seed_keypair
            (c-aref pk 0) (c-aref sk 0)
            (%c-cast uchar (%string->cref #f seed)))
    (:tuple
     (%cref->string #f (%c-cast char (c-aref sk 0)) sklen)
     (%cref->string #f (%c-cast char (c-aref pk 0)) pklen))
    ))

;; ----
;; AEAD
;; ----

;; ---- chacha20poly1305 ----

(define (aead-chacha20poly1305-encrypt m k nonce ad)
  (let ((m* (string->uchar m))
        (mlen (string-length m))
        (c (make-string mlen))
        (c* (string->uchar c))
        (k* (string->uchar k))
        (nonce* (string->uchar nonce))
        (ad* (string->uchar ad))
        (mac (make-string crypto_aead_chacha20poly1305_IETF_ABYTES)) ;; 16
        (mac* (string->uchar mac))
        (maclen (halloc ulonglong))
        )
    (sodium sodium/crypto_aead_chacha20poly1305_ietf_encrypt_detached
            c*
            mac* maclen
            m* mlen
            ad* (string-length ad)
            NULL
            nonce*
            k*)
    {ciphertext=c mac=mac}
    ))

(define (aead-chacha20poly1305-decrypt c k nonce ad mac)
  (let ((c* (string->uchar c))
        (clen (string-length c))
        (m (make-string clen))
        (m* (string->uchar m))
        (k* (string->uchar k))
        (nonce* (string->uchar nonce))
        (ad* (string->uchar ad))
        (mac* (string->uchar mac))
        )
    (sodium sodium/crypto_aead_chacha20poly1305_ietf_decrypt_detached
            m* NULL
            c* clen
            mac*
            ad* (string-length ad)
            nonce*
            k*)
    m
    ))

(define aead-chacha20poly1305
  {encrypt=aead-chacha20poly1305-encrypt
   decrypt=aead-chacha20poly1305-decrypt
   key-size=32
   iv-size=12
   mac-size=16
   })

;; ---- aes256gcm ----

(define (aead-aes256gcm-encrypt m k nonce ad)
  (let ((m* (string->uchar m))
        (mlen (string-length m))
        (c (make-string mlen))
        (c* (string->uchar c))
        (k* (string->uchar k))
        (nonce* (string->uchar nonce))
        (ad* (string->uchar ad))
        (mac (make-string crypto_aead_aes256gcm_ABYTES)) ;; 16
        (mac* (string->uchar mac))
        (maclen* (halloc ulonglong))
        )
    (sodium sodium/crypto_aead_aes256gcm_encrypt_detached
            c*
            mac* maclen*
            m* mlen
            ad* (string-length ad)
            NULL
            nonce*
            k*)
    {ciphertext=c mac=mac}
    ))

(define (aead-aes256gcm-decrypt c k nonce ad mac)
  (let ((c* (string->uchar c))
        (clen (string-length c))
        (m (make-string clen))
        (m* (string->uchar m))
        (k* (string->uchar k))
        (nonce* (string->uchar nonce))
        (ad* (string->uchar ad))
        (mac* (string->uchar mac))
        )
    (sodium sodium/crypto_aead_aes256gcm_decrypt_detached
            m* NULL
            c* clen
            mac*
            ad* (string-length ad)
            nonce*
            k*)
    m
    ))

(define aead-aes256gcm
  {encrypt=aead-aes256gcm-encrypt
   decrypt=aead-aes256gcm-decrypt
   key-size=32
   iv-size=12
   mac-size=16
   })
