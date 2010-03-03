
;; verify that let_reg works correctly

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

(define (min x y)
  (if (< x y) x y))

(let ((x 3)
      (y 5)
      (m (min x y))
      )
  (+ m (+ x y)))
