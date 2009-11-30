
(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(let ((x (+ 3 4)))
  (+ 9 x))
