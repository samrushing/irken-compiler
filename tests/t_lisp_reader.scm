;; -*- Mode: Irken -*-

;; XXX this is about to be moved to lib.
(include "self/lisp_reader.scm")
(include "lib/os.scm")

(define (test-file)
  (let ((t (read-file
	    (if (> sys.argc 1)
		sys.argv[1]
		"lib/core.scm"))))
    (for-each (lambda (x) (pp x 80) (newline)) t)
    #u
    ))

(test-file)
