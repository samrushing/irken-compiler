
(define (random)
  (%%cexp "box(random())"))

(define (srandom n)
  (%%verify "TC_INT" 1 n)
  (%%cexp "(srandom (unbox (%s)), PXLL_UNDEFINED)" n))
  