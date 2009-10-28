(define (ident x) x)

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(let ((x 5))
  (if (= 0 x)
      (+ x 1)
      (+ x 2)))
