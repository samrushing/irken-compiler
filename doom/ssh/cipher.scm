;; -*- Mode: Irken -*-

(require "lib/crypto/sodium.scm")

(define (gen-keys cipher K H session-id)

  (define (hash-list vals)
    (let ((h (cipher.hash)))
      (for-list val vals
        (h.update val))
      (h.final)))

  (define (get-one letter size)

    (define (recur vals size)
      (let ((Kn (hash-list vals))
            (left (- size (string-length Kn))))
        (if (> left 0)
            (list:cons Kn (recur (list K H Kn) left))
            (list Kn))))

    (if (= size 0)
        ""
        (substring
         (string-concat
          (recur (list K H letter session-id)
                 size))
         0 size)))

  { c2s-iv  = (get-one "A" cipher.iv-size)
    s2c-iv  = (get-one "B" cipher.iv-size)
    c2s-key = (get-one "C" cipher.key-size)
    s2c-key = (get-one "D" cipher.key-size)
    c2s-mac = (get-one "E" cipher.mac-size)
    s2c-mac = (get-one "F" cipher.mac-size) }
  )

;; ------ 'chacha20-poly1305' cipher -----

;; Not sure there's an RFC for this yet.
;; See openssh-portable/PROTOCOL.chacha20poly1305

(define (chacha20-poly1305-make-codec key iv mac)

  ;; note: iv & mac not used.

  (let ((k1 (substring key 0 32))
        (k2 (substring key 32 64)))

    (define (encrypt pkt seq)
      (let ((plen (string-length pkt))
            (nonce (int->u64->be seq))
            (hdr0 (u32->be plen))
            (hdr1 (chacha20 k2 hdr0 nonce 0))
            (pkt1 (chacha20 k1 pkt nonce 1))
            (both0 (string-append hdr0 pkt1))
            (both1 (string-append hdr1 pkt1))
            (tag (poly1305 k1 both1 nonce)))
        ;; (printf ">>> [encrypted] plen " (int plen) "\n"
        ;;         "    nonce " (string->hex nonce) "\n"
        ;;         "     hdr0 " (string->hex hdr0) "\n"
        ;;         "     hdr1 " (string->hex hdr1) "\n"
        ;;         "      tag " (string->hex tag) "\n"
        ;;         "       k2 " (string->hex k2) "\n"
        ;;         )
        (:tuple both1 tag)
        ))

    (define (decrypt-header header nonce)
      ;; (printf "decrypt-header:\n"
      ;;         "  k2 " (string->hex k2) "\n"
      ;;         "  header " (string->hex header) "\n"
      ;;         "  nonce " (string->hex nonce) "\n")
      (chacha20 k2 header nonce 0))

    (define (decrypt-packet header data nonce tag)
      (if (poly1305-verify k1 (string-append header data) nonce tag)
          (chacha20 k1 data nonce 1)
          (raise (:SSH/CipherError "poly1035-verify failed"))))

    ;; should probably be tested in newkeys
    (assert (= (string-length key) 64))
    (assert (= (string-length iv) 0))

    { encrypt = encrypt
      decrypt-header = decrypt-header
      decrypt-packet = decrypt-packet }

    ))

(define chacha20-poly1305-cipher
  { name             = "chacha20-poly1305@openssh.com"
    iv-size          = 0
    key-size         = 64
    mac-size         = 0
    tag-size         = 16
    block-size       = 1
    hash             = sha256/make
    codec            = chacha20-poly1305-make-codec
    encrypted-length = #t
    })


;; ------ 'aes256-gcm' cipher -----

;; RFC 5647, but not really.  See openssh-portable/PROTOCOL:

;; OpenSSH supports the AES-GCM algorithm as specified in RFC 5647.
;; Because of problems with the specification of the key exchange
;; the behaviour of OpenSSH differs from the RFC as follows:

;; AES-GCM is only negotiated as the cipher algorithms
;; "aes128-gcm@openssh.com" or "aes256-gcm@openssh.com" and never as
;; an MAC algorithm. Additionally, if AES-GCM is selected as the cipher
;; the exchanged MAC algorithms are ignored and there doesn't have to be
;; a matching MAC.

(define (u64-increment s)
  (let ((n (u256->big s))
        (n+1 (big-add big/1 n))
        (n64 (big-mask-bits n+1 64)))
    (big->u256 n64)
    ))

(define (aes256-gcm-make-codec key iv mac)

  (define (update-iv!)
    (let ((fixed (substring iv 0 4))
          (counter0 (substring iv 4 12))
          (counter1 (u64-increment counter0)))
      (set! iv (string-append fixed counter1))))

  (define (encrypt pkt seq)
    (let ((plen (string-length pkt))
          (head (u32->be plen))
          (ctrec (aead-aes256gcm-encrypt pkt key iv head))
          (both (string-append head ctrec.ciphertext))
          )
      (update-iv!)
      (:tuple both ctrec.mac)
      ))

  (define (decrypt-header header nonce)
    header)

  (define (decrypt-packet header ct nonce tag)
    (let ((pt (aead-aes256gcm-decrypt ct key iv header tag)))
      (update-iv!)
      pt))

  (assert (= (string-length key) 32))
  (assert (= (string-length iv) 12))

  { encrypt = encrypt
    decrypt-header = decrypt-header
    decrypt-packet = decrypt-packet
    }
  )

(define aes256-gcm-cipher
  { name             = "aes256-gcm@openssh.com"
    iv-size          = 12
    key-size         = 32
    mac-size         = 0
    tag-size         = 16
    block-size       = 16
    hash             = sha256/make
    codec            = aes256-gcm-make-codec
    encrypted-length = #f
    })

;; ------ 'none' cipher -----

;; this is used to simplify the transport logic before kex,
;;   it's not meant to be used as an actual cipher.

(define (none-cipher-make-codec key iv mac)

  { encrypt        = (lambda (data seq) (:tuple data ""))
    decrypt-header = (lambda (header nonce) header)
    decrypt-packet = (lambda (header data nonce tag) data)
    }
  )

(define none-cipher
  { name             =  "none"
    iv-size          = 0
    key-size         = 0
    mac-size         = 0
    tag-size         = 0
    block-size       = 8
    hash             = sha256/make
    codec            = none-cipher-make-codec
    encrypted-length = #f
    })

(define get-cipher-by-name
  "chacha20-poly1305@openssh.com" -> chacha20-poly1305-cipher
  "aes256-gcm@openssh.com"        -> aes256-gcm-cipher
  "none"                          -> none-cipher
  name                            -> (raise (:SSH/Error (format "unsupported cipher " name)))
  )

