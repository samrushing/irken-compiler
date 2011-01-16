
(include "lib/core.scm")

;; http://groups.google.com/group/comp.lang.scheme/msg/3e2d267c8f0ef180

(define (negate n)
  (- 0 n))

(let ((f negate))
  (let f ((n (f 1))) n))
