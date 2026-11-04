;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/base64.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/pem.scm")

;; convert DER to PEM

(define sample-key
  (ber:SEQUENCE
   (list
    (ber:INTEGER 0)
    (ber:SEQUENCE
     (list
      (ber:OID (list 1 3 101 112))))
    (ber:STRING (asn1string:OCTET) "\x04 'u@\x9b\x94\xe3\x7f\xcd&\xfd\x9a}\xc6\xe1\x89\xd8Qc\x8b\xce\xd2|\x85}\xf5\"idRC\x9a\xa6")
    )))

(define (main)
  (for line (gen-pem
             (string-generator
              (asn1->ber sample-key))
             "PRIVATE KEY")
    (printf line)))

(main)
