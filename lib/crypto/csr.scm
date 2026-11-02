;; -*- Mode: Irken -*-

;; P-256 keys and certificate signing requests (rfc2986), for
;;  getting certificates from a CA.

(require "lib/asn1/ber.scm")
(require "lib/crypto/pem.scm")
(require "lib/crypto/p256.scm")

(define oid-ecPublicKey        '(1 2 840 10045 2 1))
(define oid-prime256v1         '(1 2 840 10045 3 1 7))
(define oid-ecdsa-with-SHA256  '(1 2 840 10045 4 3 2))
(define oid-extensionRequest   '(1 2 840 113549 1 9 14))

;; SubjectPublicKeyInfo for a P-256 public key (04 || x || y)
(define (p256-spki pub)
  (ber:SEQUENCE
   (list (ber:SEQUENCE (list (ber:OID oid-ecPublicKey) (ber:OID oid-prime256v1)))
         (BITSTRING0 pub))))

;; rfc5958 OneAsymmetricKey (PKCS#8) wrapping an rfc5915 ECPrivateKey:
;;  what openssl writes, and what read-skey reads.
(define (p256-pkcs8 sk)
  (ber:SEQUENCE
   (list (ber:INTEGER 0)
         (ber:SEQUENCE (list (ber:OID oid-ecPublicKey) (ber:OID oid-prime256v1)))
         (OCTETSTRING
          (asn1->ber
           (ber:SEQUENCE
            (list (ber:INTEGER 1)
                  (OCTETSTRING sk)
                  (ber:CONTEXT 1 (list (BITSTRING0 (p256-public-key sk)))))))))))

(define (write-pem path kind der mode)
  (let ((file (file/open-write path #t mode)))
    (for s (pem-write-gen (string-generator der) kind)
      (file/write file s))
    (file/flush file)
    (file/close file)))

(define (pem-string kind der)
  (string-concat (generator->list (pem-write-gen (string-generator der) kind))))

;; a DER CertificationRequest for <names> (DNS names), signed by <sk>.
;;  the subject is CN=<first name>; all the names go in subjectAltName.
(define (p256-csr names sk RNG)
  (let ((attr (ber:SEQUENCE (list (ber:OID '(2 5 4 3))
                                  (ber:STRING (asn1string:UTF8) (first names)))))
        (subject (ber:SEQUENCE (list (ber:SET (list attr)))))
        (alt-names (ber:SEQUENCE
                    (map (lambda (name) (ber:RAW (asn1class:ctxt) #f 2 name)) names)))
        (san-ext (ber:SEQUENCE (list (ber:OID '(2 5 29 17))
                                     (OCTETSTRING (asn1->ber alt-names)))))
        ;; Attribute { extensionRequest, SET { Extensions } }
        (ext-req (ber:SEQUENCE (list (ber:OID oid-extensionRequest)
                                     (ber:SET (list (ber:SEQUENCE (list san-ext)))))))
        (info (ber:SEQUENCE (list (ber:INTEGER 0)
                                  subject
                                  (p256-spki (p256-public-key sk))
                                  ;; attributes [0] IMPLICIT SET OF Attribute
                                  (ber:CONTEXT 0 (list ext-req)))))
        (sig (p256-sign (asn1->ber info) sk RNG)))
    (asn1->ber
     (ber:SEQUENCE (list info
                         (ber:SEQUENCE (list (ber:OID oid-ecdsa-with-SHA256)))
                         (BITSTRING0 sig))))))
