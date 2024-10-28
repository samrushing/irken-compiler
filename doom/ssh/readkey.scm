;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/crypto/pem.scm")
(require "lib/codecs/hex.scm")

(define (decoder s)

  (let ((pos 0)
        (slen (string-length s)))

    (define (get n)
      (let ((r (substring s pos (+ pos n))))
        (inc! pos n)
        r))

    (define (bref s i)
      (char->int (string-ref s i)))

    (define (get-u32)
      (let ((s (get 4)))
        (+ (<< (bref s 0) 24)
           (<< (bref s 1) 16)
           (<< (bref s 2)  8)
           (bref s 3))))

    (define (get-string)
      (let ((size (get-u32)))
        (get size)))

    (define (left)
      (- slen pos))

    {get=get u32=get-u32 string=get-string left=left}

    ))

(define (unpack-pkey data)
  (let ((dec (decoder data))
        (kind (dec.string)))
    (match kind with
      "ssh-ed25519"
      -> (:tuple kind (dec.string))
      _
      -> (raise (:Readkey/UnknownKeyType kind))
      )))

(define (unpack-skeys data nkeys)
  (let ((dec (decoder data))
        (check0 (dec.u32))
        (check1 (dec.u32))
        (skeys (list:nil)))
    (for-range i nkeys
      (let ((kind (dec.string)))
        (match kind with
          "ssh-ed25519"
          -> (push! skeys (:tuple (dec.string) (dec.string)))
          _
          -> (raise (:Readkey/UnknownKeyType kind))
          )))
    (for-list skey skeys
      (let (((pk sk) skey))
        (printf "  pub " (string->hex pk) "\n"
                "  prv " (string->hex sk) "\n")))
    ))

;; as best I can tell the source only supports a single key,
;; ssh-keygen only puts in a single key.
(define (unpack key)
  (let ((dec (decoder key))
        (magic (dec.get 15)))
    (assert (string=? magic "openssh-key-v1\x00"))
    (let ((ciphername (dec.string))
          (kdfname (dec.string))
          (kdfoptions (dec.string))
          (numkeys (dec.u32))
          (pubkeys (list:nil))
          (skeys ""))
      (for-range i numkeys
        (push! pubkeys (dec.string))) ;; encapsulated
      (set! skeys (dec.string)) ;; encapsulated
      (printf "ciphername " (string ciphername) "\n"
              "kdfname    " (string kdfname) "\n"
              "kdfoptions " (string kdfoptions) "\n")
      (printf "pubkeys [" (int numkeys) "] : {\n")
      (for-list pubkey (reverse pubkeys)
        (match (unpack-pkey pubkey) with
          (:tuple kind pubkey)
          -> (printf "  " kind " " (string->hex pubkey) "\n")))
      (printf "}\n")
      (printf "skeys: {\n")
      (if (string=? ciphername "none")
          (unpack-skeys skeys numkeys)
          (printf "  [encrypted]\n"))
      (printf "}\n")
      )))

(define (main)
  (let ((path sys.argv[1])
        (g0 (file-char-generator
             (file/open-read path))))
    (printf ";; path: " sys.argv[1] "\n")
    (for section (pem-read-gen g0)
      (match section with
        (:tuple "OPENSSH PRIVATE KEY" data)
        -> (unpack data)
        _
        -> (raise (:SSH/NotAKeyFile path))
        ))))

(main)

