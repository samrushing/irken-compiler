
(define (>= a b)
  (%%cexp (int int -> bool) "%s>=%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (tak x y z)
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))))

(let loop ((n 20))
  (tak 18 12 6)
  (if (= n 0)
      "done"
      (loop (- n 1))))
