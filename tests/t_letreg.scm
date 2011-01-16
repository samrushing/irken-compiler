
;; verify that let_reg works correctly

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (< a b)
  (%%cexp (int int -> bool) "%0<%1" a b))

(define (min x y)
  (if (< x y) x y))

(let ((x 3)
      (y 5)
      (m (min x y))
      )
  (+ m (+ x y)))
