;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/pem.scm")

(define (maybe-ber data)
  (when
      (not
       (try
        (let ((asn1 (ber->asn1 data)))
          (printf ";; ASN1 ")
          (pp-ber asn1 0)
          (printf "\n")
          #t)
        except
        (:BER/ExtraData _ _) -> #f
        (:BER/LengthTooLong _) -> #f
        ))
    (printf ";; not DER encoded.\n")
    ))

(define program-options
  (makeopt
   (pos 'pemfile (string ""))
   ))

(define (get-options)
  (if (< sys.argc 2)
      (usage)
      (try
       (process-options program-options (rest (vector->list sys.argv)))
       except
       (:Getopt/MissingArg _ _) -> (usage)
       (:Getopt/UnknownArg _ _) -> (usage)
       )))

(define (usage)
  (printf "\nPEM Tool. Decode a PEM file.\n\n")
  (printf "Usage: " sys.argv[0] " <input.pem>\n")
  (printf "    example: $ " sys.argv[0] " x509.pem\n")
  (%exit #f -1))

(define (main)
  (let ((opts (get-options))
        (path (probably (get-string-opt opts 'pemfile)))
        (g0 (file-char-generator
             (file/open-read path))))
    (printf ";; path: " path "\n")
    (for section (pem-read-gen g0)
      (match section with
        (:tuple label data)
        -> (begin
             (printf ";; PEM " (string label) "\n")
             (printf ";; DATA " (string->hex data) "\n")
             (maybe-ber data)
             )
        ))))

(main)

