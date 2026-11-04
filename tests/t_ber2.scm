;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/asn1/ber.scm")

;; like dumpasn1, but with sexp output.

(define (main)
  (let ((file (file/open-read sys.argv[1]))
        (fgen (make-file-generator file))
        (parts '()))
    (for buf fgen
      (push! parts buf))
    (let ((sder (string-concat (reverse parts)))
          (semit (string-emitter sder))
          ((ber size) (ber/decode semit)))
      (pp-ber ber 0)
      (printf "\n")
      )))

(main)
