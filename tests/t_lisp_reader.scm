;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")
(include "lib/lisp_reader.scm")

(define (read-file path)
  (let ((file (file/open-read path))
	(result (reader path (lambda () (file/read-char file)))))
    result))

(define (test-file)
  (let ((t (read-file
	    (if (> sys.argc 1)
		sys.argv[1]
		"lib/core.scm"))))
    (for-each (lambda (x) (pp x 80) (newline)) t)
    #u
    ))

(test-file)
