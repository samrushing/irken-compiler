
;(include "lib/primops.scm")

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (add x y)
  (+ x y))

(define (maybe)
  #t)

(if (maybe)
    (add 34 19)
    (add 12 2))

;(let loop ((n 1000000))
;  (if (= n 0)
;      #t
;      (loop (- n 1))))
    