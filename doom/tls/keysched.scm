;; -*- Mode: Irken -*-

(require "lib/crypto/hkdf.scm")
(require "doom/tls/codec.scm")

(define (keysched/make hash cipher draft-version)

  (let ((tscript '())
        (hsize   hash.size)
        (zeros   (format (repeat hsize "\x00")))
        (hempty  (hash.oneshot ""))
        (hkdf    (hkdf/make hash))
        ;; XXX wipe secrets when no longer needed
        (secrets {
           current = ""
           s-hs-tf = ""
           c-hs-tf = ""
           s-ap-tf = ""
           c-ap-tf = ""
           })
        (send-key {key="" iv=""})
        (recv-key {key="" iv=""})
        (send-seq 0)
        (recv-seq 0)
        )

    (define (hkdf-label label context length)
      (rope->string
       (rope/build
        (put-u16 length)
        (put-opaque-8 (format "tls13 " label))
        (put-opaque-8 context))))

    (define (expand-label secret label context length)
      (hkdf.expand secret (hkdf-label label context length) length))

    (define (make-key secret)
      {key=(expand-label secret "key" "" cipher.key-size)
       iv =(expand-label secret "iv"  "" cipher.iv-size)})

    (define (set-hsk-key shared-key)
      (let ((trans0 (get-tscript-hash)))
        ;; handshake secret
        (set! secrets.current (hkdf.extract secrets.current shared-key))
        (set! secrets.s-hs-tf (expand-label secrets.current "s hs traffic" trans0 hsize))
        (set! secrets.c-hs-tf (expand-label secrets.current "c hs traffic" trans0 hsize))
        (set! send-key (make-key secrets.s-hs-tf))
        (set! recv-key (make-key secrets.c-hs-tf))
        (set! send-seq 0)
        (set! recv-seq 0)
        ))

    (define (set-appdata-key)
      (let ((trans0 (get-tscript-hash))
            (derived (expand-label secrets.current "derived" hempty hsize)))
        ;; master secret
        (set! secrets.current (hkdf.extract derived zeros))
        (set! secrets.s-ap-tf (expand-label secrets.current "s ap traffic" trans0 hsize))
        (set! secrets.c-ap-tf (expand-label secrets.current "c ap traffic" trans0 hsize))
        (set! send-key (make-key secrets.s-ap-tf))
        (set! recv-key (make-key secrets.c-ap-tf))
        (set! send-seq 0)
        (set! recv-seq 0)
        ))

    (define (add-tscript-packet pkt)
      (push! tscript pkt))

    (define (get-tscript-hash)
      (let ((h (hash.make)))
        (for-list item (reverse tscript)
          (h.update item))
        (h.final)))

    (define (get-finished server?)
      (let ((base-key (if server? secrets.s-hs-tf secrets.c-hs-tf))
            (finished-key (expand-label base-key "finished" "" hsize))
            (tscript-hash (get-tscript-hash))
            (hmac0 (hmac/make hash))
            (hmac1 (hmac0.make finished-key)))
        (hmac1.update tscript-hash)
        (hmac1.final)
        ))

    (define (xor-sequence iv seq)
      (let ((ivlen (string-length iv))
            (result (copy-string iv ivlen)))
        ;; this XORs the sequence number as a big-endian u64
        ;;   right-justified against the IV (typically 12 bytes).
        (for-range i 8
          (string-set!
           result (- ivlen i 1)
           (int->char
            (logxor (char->int (string-ref iv (- ivlen i 1)))
                    (logand #xff seq))))
          (set! seq (>> seq 8)))
        result))

    (define (make-padding n)
      (let (((q r) (divmod n hsize))
            (chunk (rope:leaf zeros))
            (padding '()))
        (when (> r 0)
          (push! padding (rope:leaf (substring zeros 0 r))))
        (for-range i q
          (push! padding chunk))
        (rope/cat padding)))

    ;; how to encrypt pkt[s]:
    ;; 1) concat the pkts together.
    ;; 2) feed the following to AEAD:
    ;;    PT = pkts + ctype-byte + padding
    ;;    IV = hs-w-iv XOR sequence-number
    ;;    AD = "170303xxyy" (xxyy is 16-bit length of PT)
    ;; 3) AEAD outputs ciphertext and mac.
    ;; 4) append ciphertext and mac, wrap with appdata (i.e. "170303xxyy")

    (define (encrypt-packet kind pkt npad)
      (let ((padding (rope/build (put-u8 (tls-ctype->int kind)) (make-padding npad)))
            (header (rope->string
                     (pack-header
                      (tls-ctype:appdata)
                      (+ 1 npad cipher.mac-size (rope-length pkt)))))
            (AD (if (or (= draft-version #x304) (>= draft-version #x7f18)) header ""))
            (plaintext (rope->string (rope/build pkt padding)))
            (iv (xor-sequence send-key.iv send-seq))
            (ct (cipher.encrypt plaintext send-key.key iv AD)))
        (inc! send-seq)
        (string-append header ct.ciphertext ct.mac)))

    (define (decrypt-packet pkt header)
      ;;   the mac is at the end of the ciphertext
      ;;   AD == packet header == opaque_type || legacy_version || u16 length
      (let ((len (string-length pkt))
            (macpos (- len cipher.mac-size))
            (mac (substring pkt macpos len))
            (ct (substring pkt 0 macpos))
            (iv (xor-sequence recv-key.iv recv-seq))
            (AD (if (or (= draft-version #x304) (>= draft-version #x7f18)) header "")))
        (inc! recv-seq)
        (cipher.decrypt ct recv-key.key iv AD mac)))

    ;; early secret
    (let ((PRK (hkdf.extract "" zeros)))
      (set! secrets.current (expand-label PRK "derived" hempty hsize)))

    {add-tscript-packet = add-tscript-packet
     get-tscript-hash   = get-tscript-hash
     get-finished       = get-finished
     set-hsk-key        = set-hsk-key
     set-appdata-key    = set-appdata-key
     encrypt-packet     = encrypt-packet
     decrypt-packet     = decrypt-packet
     }
    ))





