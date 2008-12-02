(define (fib n)
  (define (fiba n a b)
    (if (%zero? n)
	a
	(fiba (%- n 1) b (%+ a b))))
  (fiba n 0 1))
(fib 50)
