(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (ident x) x)

(let loop ((n 100)
	   (a 0))
  (if (= n 0)
      a
      (loop (- n 1) (+ a n))))
