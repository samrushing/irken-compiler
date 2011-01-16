(define (- a b)
  (%%cexp (int int -> int) "%0-%1" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%0==%1" a b))

(if (= 3 4)
    (+ 2 3)
    (- 2 3))