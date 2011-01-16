
(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define y 19)

(let ((x:int 34))
  (+ x y)
  )
