
(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define y 19)

(let ((x:int 34))
  (+ x y)
  )
