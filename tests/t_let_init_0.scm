
(include "lib/pair.scm")

(define thing
  (let ((list '()))
    (lambda (x)
      (set! list (cons x list))
      list)))

(%printn (thing 1))
(%printn (thing 2))
(%printn (thing 3))
(%printn (thing 4))
(%printn (thing 5))
