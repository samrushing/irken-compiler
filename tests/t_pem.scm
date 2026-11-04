;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/base64.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/pem.scm")

(define (main)
  (let ((g0 (file-char-generator
             (file/open-read sys.argv[1]))))
    (printf ";; path: " sys.argv[1] "\n")
    (for section (pem-read-gen g0)
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
