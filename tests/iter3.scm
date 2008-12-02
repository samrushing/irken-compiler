(define (cons a b)
  ;; rather than using a let, I'm trying to keep this small enough to inline
  ;; maybe later when I figure out hoisting, the more obvious defn will do.
  (%%cexp "( t = %s, ((pxll_pair*)t)->car = %s, ((pxll_pair*)t)->cdr = %s, t)" (%make-tuple #x18 2) a b))

(let ((x #f))
  (let loop ((n 1000000))
    (set! x (cons 3 4))
    (if (%zero? n)
	x
	(loop (%- n 1)))))
