;; -*- Mode: Irken -*-
;; read a P-256 key in the PKCS#8 form `openssl req -newkey ec` writes.
;; (tests/t_pkcs8_ec.pem is a throwaway key made for this test.)
(require "lib/basis.scm")
(require "lib/codecs/hex.scm")
(require "lib/crypto/sig.scm")

(match (read-skey "tests/t_pkcs8_ec.pem") with
  (skey:p256 k)
  -> (begin
       (printf (string->hex k) "\n")
       (assert (= 32 (string-length k)))
       (assert (string=? k (hex->string "e78aecc60459c8ab7ca7d014665e0e0708cf44fb51c644b64f024e9f307e617a")))
       (printf "ok\n"))
  _ -> (raise (:NotP256)))
