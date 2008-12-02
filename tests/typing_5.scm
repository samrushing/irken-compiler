
(define (- x y)
  (%%cexp (int int -> int) "%s-%s" x y))

(define (zero? z)
  (%%cexp (int -> bool) "%s==0" z))

(let loop ((n 1000000))
  (if (zero? n)
      #t
      (loop (- n 1))))