

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define plus +)

(plus 1 (plus 2 3))

