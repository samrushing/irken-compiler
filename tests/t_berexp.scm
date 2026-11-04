;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/asn1/ber.scm")
(require "lib/asn1/berexp.scm")

(define (main path)
  (let ((f (file/open-read path))
        (r (lambda () (file/read-char f)))
        (exps (read2 path r #t)))
    (for-list exp exps
      (let ((ber (sexp->ber exp)))
        (pp-ber ber 0))
      (printf "\n")
      )))

(main sys.argv[1])
