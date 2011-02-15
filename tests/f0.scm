
(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(+ 1 #\A)

