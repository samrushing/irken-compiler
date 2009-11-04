(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(if (= 3 4)
    (+ 2 3)
    (- 2 3))