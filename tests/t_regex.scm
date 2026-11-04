;; -*- Mode: Irken -*-


(require "lib/regex.scm")

(define (main)
  (let ((regex (regex/compile sys.argv[1])))
    (printn (regex sys.argv[2]))
    ))

(main)

	
