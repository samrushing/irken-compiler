

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define plus +)

(plus 1 (plus 2 3))

