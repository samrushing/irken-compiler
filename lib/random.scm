
(define (random)
  (%%cexp (-> int) "random()"))

(define (srandom n)
  (%%cexp (int -> undefined) "(srandom (%0), PXLL_UNDEFINED)" n))
  