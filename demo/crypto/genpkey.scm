;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/urandom.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/pem.scm")
(require "lib/crypto/p256.scm")

(define oid-ecPublicKey '(1 2 840 10045 2 1))
(define oid-prime256v1  '(1 2 840 10045 3 1 7))

(defmacro SEQ
  (SEQ item ...) -> (ber:SEQUENCE (list item ...)))

(define (BITSTRING s) (BITSTRING0 s))

;; RFC 5958 OneAsymmetricKey (PKCS#8) wrapping an RFC 5915 ECPrivateKey.
(define (pkcs8 sk pub)
  (SEQ (ber:INTEGER 0)
       (SEQ (ber:OID oid-ecPublicKey) (ber:OID oid-prime256v1))
       (OCTETSTRING
        (asn1->ber
         (SEQ (ber:INTEGER 1)
              (OCTETSTRING sk)
              (ber:CONTEXT 1 (list (BITSTRING pub))))))))

(define (main)
  (let ((RNG (urandom-make))
        (sk (p256-make-key RNG))
        (der (asn1->ber (pkcs8 sk (p256-public-key sk)))))
    (for s (pem-write-gen (string-generator der) "PRIVATE KEY")
       (printf s)
       )))

(main)
