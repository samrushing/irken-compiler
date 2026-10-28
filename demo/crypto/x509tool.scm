;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/x509.scm")

;; sample ed25519 key from `openssl genpkey`:
 ;;  0  46: SEQUENCE {
 ;;  2   1:   INTEGER 0
 ;;  5   5:   SEQUENCE {
 ;;  7   3:     OBJECT IDENTIFIER '1 3 101 112'
 ;;       :     }
 ;; 12  34:   OCTET STRING, encapsulates {
 ;; 14  32:     OCTET STRING
 ;;       :       20 3A 2A 09 47 AE B1 C2 55 5B 90 A9 DD 06 AC 20
 ;;       :       73 B2 53 7D AD 1C B6 B2 8B 59 3A 45 DF AB 3F 4A
 ;;       :     }
 ;;       :   }

;; definitely need a DSL/format-macro for this...

(define key (hex->string "203a2a0947aeb1c2555b90a9dd06ac2073b2537dad1cb6b28b593a45dfab3f4a"))

(define (make-sample)
  (ber:SEQUENCE
   (list (ber:INTEGER 0)
         (ber:SEQUENCE (list (ber:OID (list 1 3 101 112))))
         (ber:STRING
          (asn1string:OCTET)
          ;; encapsulated
          (asn1->ber (ber:STRING (asn1string:OCTET) key))
          ))))

(let ((f (file/open-write "/tmp/key.der" #t #o644)))
  (file/write f (asn1->ber (make-sample)))
  (file/flush f)
  (file/close f)
  )

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

(let ((x509 (parse-x509 (read-certificate sys.argv[1]))))
  (print-x509 x509)
  (verify-cert x509)
  )
