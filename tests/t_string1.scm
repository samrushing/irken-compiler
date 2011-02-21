
(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define s0 "testing")

(string-set! s0 0 #\T)
(printn s0)
;; should trigger an out-of-bounds error
(string-ref s0 20)

