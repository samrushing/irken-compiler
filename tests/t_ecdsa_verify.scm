;; -*- Mode: Irken -*-

;; ECDSA verification (lib/crypto/ecdsa.scm), with signatures made by
;;  `openssl dgst -sha256|-sha384 -sign` over the message below.
;; expect: #t #f #f for each curve.

(require "lib/basis.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/ecdsa.scm")

(define msg "hello, irken")

(define (flip-last-bit s)
  (let ((r (copy-string s (string-length s)))
        (i (- (string-length s) 1)))
    (string-set! r i (int->char (logxor 1 (char->int (string-ref s i)))))
    r))

(define (try-vector c hash pub sig)
  (let ((pub (hex->string pub))
        (sig (hex->string sig)))
    (printf (rpad 6 c.name)
            " good " (bool (ecdsa-verify c (hash msg) sig pub))
            " badmsg " (bool (ecdsa-verify c (hash "hello, irken!") sig pub))
            " badsig " (bool (ecdsa-verify c (hash msg) (flip-last-bit sig) pub))
            "\n")))

(try-vector
 ec-curve-p256 sha256
 (format "04da5eecb87c23c1fc51a60450f10cb6760e0947070a72a67cefc9eff87ba54d88"
         "c9202d828b5d2ca9f339a0379b7991e572c709f8dfefc74d3f7160420c75f756")
 (format "304402204a00b12ca9d430cbe12231c1f923116d75da4f332141612971b96c1615a0df47"
         "02202654ff1e3d5c6fd014331f532bacf57ed54ebeefa68cc796300db68272b54b7a"))

(try-vector
 ec-curve-p384 sha384
 (format "047653365ada094c8a58db78943bbe37e0beb7cdb06aadee668c90d058c170483b"
         "a5b509cc24ed569259650ef03710dba0288d330d8531d1c0d5d87300fab461a8e8"
         "e55eda75687e49fe3f14662e2a45c3585da032882ecbaeefc4d63d41f6cc2c")
 (format "3066023100ac6407953add101ca958bf26e2a6da3173754e39eca4f2c616708683aa86"
         "3911fe54b3d7d6d6480850747cd492a867860231008ed156d494de3a888077ee32ede0"
         "169addeae3099449315ad61083acf48816d532e86f7c86a0775a9a716fe96ae612e8"))
