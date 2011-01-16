(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (thing z)
  (+ 3 (%raccess/left z)))

(let ((x (%rextend/right (%rextend/left (%rmake) 19) #\A)))
  (let ((y (%rextend/right (%rmake) (lambda (x) x))))
    (+ (%raccess/left x) (thing x))))
