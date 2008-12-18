
(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(lambda (y)
  (let ((f (lambda (x) y)))
    (if (f #t)
	(+ (f #t) 5)
	6)))
