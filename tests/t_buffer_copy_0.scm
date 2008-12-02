;;

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (thing)
  (let ((s0 "01234567890123456789")
	(s1 "abcdefghij"))
    (buffer-copy s1 3 4 s0 10)
    s0))

;; "0123456789defg456789"
(thing)
