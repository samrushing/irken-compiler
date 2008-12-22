
(define (random)
  (%%cexp (-> int) "random()"))

(define (srandom n)
  (%%cexp (int -> undefined) "(srandom (%s), PXLL_UNDEFINED)" n))
  