;; -*- Mode: Irken -*-

;; generate a P-256 key and a self-signed certificate for it, for use
;;  with demo/h2/serve.scm (or any TLS server).
;;
;; $ demo/crypto/gencert [-cn localhost] [-days 365] [-key key.pem] [-cert cert.pem] [-k]
;;
;; -k: keep (re-use) the existing key in key.pem, and only write a new cert.
;;
;; key.pem is PKCS#8 (what `openssl genpkey` writes), and cert.pem is an
;;  X.509 v3 certificate with:
;;   - subject and issuer CN=<cn>
;;   - subjectAltName DNS:<cn> (plus 127.0.0.1 and ::1 for localhost):
;;       browsers ignore the CN and only look at subjectAltName.
;;   - keyUsage digitalSignature, extendedKeyUsage serverAuth.
;;   - basicConstraints CA:FALSE: Firefox refuses to accept a CA
;;       certificate as a server certificate, even a self-signed one.
;;   - a validity under 398 days by default (Apple refuses anything
;;       over 825 days for TLS, trusted or not).

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/time.scm")
(require "lib/urandom.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/pem.scm")
(require "lib/crypto/sig.scm")
(require "lib/crypto/p256.scm")

;; --- OIDs ---
(define oid-ecPublicKey       '(1 2 840 10045 2 1))
(define oid-prime256v1        '(1 2 840 10045 3 1 7))
(define oid-ecdsa-with-SHA256 '(1 2 840 10045 4 3 2))
(define oid-commonName        '(2 5 4 3))
(define oid-subjectKeyId      '(2 5 29 14))
(define oid-authorityKeyId    '(2 5 29 35))
(define oid-keyUsage          '(2 5 29 15))
(define oid-subjectAltName    '(2 5 29 17))
(define oid-basicConstraints  '(2 5 29 19))
(define oid-extKeyUsage       '(2 5 29 37))
(define oid-serverAuth        '(1 3 6 1 5 5 7 3 1))

(defmacro SEQ
  (SEQ item ...) -> (ber:SEQUENCE (list item ...)))

(define (BITSTRING s) (BITSTRING0 s))

;; Name ::= SEQUENCE OF SET OF { type, value }
(define (cn-name cn)
  (SEQ (ber:SET (list (SEQ (ber:OID oid-commonName)
                           (ber:STRING (asn1string:UTF8) cn))))))

;; UTCTime is good until 2050; after that it has to be GeneralizedTime.
(define (asn1-time t)
  (let ((tm (gmtime t)))
    (define (d2 n) (format (zpad 2 (int n))))
    (if (< tm.year 2050)
        (ber:STRING (asn1string:UTCTIME)
                    (format (d2 (mod tm.year 100)) (d2 (+ 1 tm.mon)) (d2 tm.mday)
                            (d2 tm.hour) (d2 tm.min) (d2 tm.sec) "Z"))
        (ber:STRING (asn1string:GENERALIZEDTIME)
                    (format (int tm.year) (d2 (+ 1 tm.mon)) (d2 tm.mday)
                            (d2 tm.hour) (d2 tm.min) (d2 tm.sec) "Z")))))

(define (extension oid critical? value)
  (if critical?
      (SEQ (ber:OID oid) (ber:BOOLEAN #t) (OCTETSTRING (asn1->ber value)))
      (SEQ (ber:OID oid) (OCTETSTRING (asn1->ber value)))))

;; GeneralName: dNSName [2] IA5String, iPAddress [7] OCTET STRING
(define (dns-name s) (ber:RAW (asn1class:ctxt) #f 2 s))
(define (ip-addr s)  (ber:RAW (asn1class:ctxt) #f 7 s))

(define (alt-names cn)
  (if (string=? cn "localhost")
      (list (dns-name cn)
            (ip-addr "\x7f\x00\x00\x01")
            (ip-addr "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01"))
      (list (dns-name cn))))

(define (spki pub)
  (SEQ (SEQ (ber:OID oid-ecPublicKey) (ber:OID oid-prime256v1))
       (BITSTRING pub)))

;; RFC 5958 OneAsymmetricKey (PKCS#8) wrapping an RFC 5915 ECPrivateKey.
(define (pkcs8 sk pub)
  (SEQ (ber:INTEGER 0)
       (SEQ (ber:OID oid-ecPublicKey) (ber:OID oid-prime256v1))
       (OCTETSTRING
        (asn1->ber
         (SEQ (ber:INTEGER 1)
              (OCTETSTRING sk)
              (ber:CONTEXT 1 (list (BITSTRING pub))))))))

(define (make-cert sk cn days RNG)
  (let ((pub (p256-public-key sk))
        (now (gettime/realtime))
        ;; a random 128-bit serial number.
        (serial (RNG 16))
        ;; RFC 7093: the key id may be a truncated SHA-256 of the key.
        (keyid (substring (sha256 pub) 0 20))
        (alg (SEQ (ber:OID oid-ecdsa-with-SHA256)))
        (tbs
         (SEQ (ber:CONTEXT 0 (list (ber:INTEGER 2)))              ;; v3
              (ber/uint serial)
              alg
              (cn-name cn)                                         ;; issuer
              (SEQ (asn1-time (- now.sec 3600))                    ;; validity
                   (asn1-time (+ now.sec (* days 86400))))
              (cn-name cn)                                         ;; subject
              (spki pub)
              (ber:CONTEXT
               3 (list
                  (SEQ
                   (extension oid-basicConstraints #t (SEQ))
                   (extension oid-keyUsage #t (ber:STRING (asn1string:BIT) "\x07\x80"))
                   (extension oid-extKeyUsage #f (SEQ (ber:OID oid-serverAuth)))
                   (extension oid-subjectAltName #f (ber:SEQUENCE (alt-names cn)))
                   (extension oid-subjectKeyId #f (OCTETSTRING keyid))
                   ;; self-signed, so the authority is us.  keyIdentifier [0] IMPLICIT
                   (extension oid-authorityKeyId #f (SEQ (ber:RAW (asn1class:ctxt) #f 0 keyid)))
                   )))))
        (tbs-der (asn1->ber tbs))
        (sig (p256-sign tbs-der sk RNG)))
    (SEQ tbs alg (BITSTRING sig))))

(define (write-pem path kind der mode)
  (let ((file (file/open-write path #t mode)))
    (for s (pem-write-gen (string-generator der) kind)
      (file/write file s))
    (file/flush file)
    (file/close file)))

(define program-options
  (makeopt
   (arg 'cn   (string "localhost"))
   (arg 'days (int 365))
   (arg 'key  (string "key.pem"))
   (arg 'cert (string "cert.pem"))
   (flag 'k)
   (flag 'h)
   ))

(define (usage)
  (printf "generate a P-256 key and a self-signed certificate.\n\n"
          "Usage: " sys.argv[0] "\n"
          "  [-cn localhost]   the host name (CN and subjectAltName)\n"
          "  [-days 365]       validity\n"
          "  [-key key.pem]    the private key (PKCS#8)\n"
          "  [-cert cert.pem]  the certificate\n"
          "  [-k]              keep the existing key, write only a new cert\n"
          "  [-h]              print this usage.\n")
  (%exit #f -1))

(define (main)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage)))
        (cn "localhost")
        (days 365)
        (keyfile "key.pem")
        (certfile "cert.pem")
        (keep? #f)
        (RNG (urandom-make)))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (when-maybe v (get-bool-opt opts 'k) (set! keep? #t))
    (when-maybe v (get-string-opt opts 'cn) (set! cn v))
    (when-maybe v (get-int-opt opts 'days) (set! days v))
    (when-maybe v (get-string-opt opts 'key) (set! keyfile v))
    (when-maybe v (get-string-opt opts 'cert) (set! certfile v))
    (let ((sk (if keep?
                  (match (read-skey keyfile) with
                    (skey:p256 sk) -> sk
                    _ -> (raise (:GenCert/NotAP256Key keyfile)))
                  (p256-make-key RNG))))
      (when (not keep?)
        (write-pem keyfile "PRIVATE KEY" (asn1->ber (pkcs8 sk (p256-public-key sk))) #o600)
        (printf "wrote " keyfile "\n"))
      (write-pem certfile "CERTIFICATE" (asn1->ber (make-cert sk cn days RNG)) #o644)
      (printf "wrote " certfile " for " cn ", valid for " (int days) " days\n")
      )))

(main)
