
(define (random)
  (%%cexp (-> int) "random()"))

(define (srandom n)
  (%%cexp (int -> undefined) "srandom (%0)" n))
  