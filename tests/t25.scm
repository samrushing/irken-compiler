
(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(let ((x (+ 3 4)))
  (+ 9 x))
