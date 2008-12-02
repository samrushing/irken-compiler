
(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(lambda (f)
  (lambda (x)
    (- (f 3) (f x))))
