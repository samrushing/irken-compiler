;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/base64.scm")
(include "lib/codecs/hex.scm")
(include "lib/asn1/ber.scm")
(include "lib/crypto/pem.scm")

(define (main)
  (let ((g0 (file-char-generator
             (file/open-read sys.argv[1]))))
    (printf ";; path: " sys.argv[1] "\n")
    (for section (pem-gen g0)
      (match section with
        (:tuple label data)
        -> (begin
             (printf ";; DER " (string->hex data) "\n")
             (printf ";; PEM " (string label))
             (pp-ber (ber->asn1 data) 0)
             (printf "\n")
             )
        ))))

(main)
