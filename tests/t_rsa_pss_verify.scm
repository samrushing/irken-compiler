;; -*- Mode: Irken -*-

;; RSASSA-PSS verification: sign with our own rsassa-pss-sign, and
;;  check a signature made by `openssl dgst -sigopt rsa_padding_mode:pss`.
;; expect: #t #f #t #f

(require "lib/basis.scm")
(require "lib/urandom.scm")
(require "lib/crypto/sig.scm")

(define msg "hello, irken")

(let ((skey (match (read-skey "tests/x509/rsa.key") with
              (skey:rsa k) -> k
              _ -> (raise (:Test/NotRSA))))
      (RNG (urandom-make))
      (sig (rsassa-pss-sign skey msg sha256-hash RNG))
      (e (int->big skey.e)))
  (printf "own     " (bool (rsassa-pss-verify skey.n e msg sig sha256-hash)) "\n")
  (printf "badmsg  " (bool (rsassa-pss-verify skey.n e "hello, irken!" sig sha256-hash)) "\n")
  (let ((osig (with-file f (file/open-read "tests/x509/rsa-pss.sig")
                (file/read-buffer f))))
    (printf "openssl " (bool (rsassa-pss-verify skey.n e msg osig sha256-hash)) "\n")
    (printf "sha384  " (bool (rsassa-pss-verify skey.n e msg osig sha384-hash)) "\n")))
