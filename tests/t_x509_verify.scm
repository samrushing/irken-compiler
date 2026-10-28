;; -*- Mode: Irken -*-

;; verify-cert checks the signature over the tbsCertificate as received.
;; expect #t for all three.

(require "lib/basis.scm")
(require "lib/crypto/x509.scm")

(for-list path '("tests/x509/ed25519.pem" "tests/x509/rsa.pem" "tests/x509/ed25519-ber.pem")
  (printf path " ")
  (verify-cert (parse-x509 (read-certificate path))))
