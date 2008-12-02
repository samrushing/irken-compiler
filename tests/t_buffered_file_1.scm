
(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/vector.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")
(include "lib/io.scm")

(define (thing)
  (let* ((file (buffered-file 0 10))
	 (read-line (file 'read-line)))
    (let loop ((line (read-line)))
      (cond ((not (eq? ch eof-object))
	     (%printn ch)
	     (loop (read-char)))))))

(thing)
