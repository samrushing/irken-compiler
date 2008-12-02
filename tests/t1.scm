
(define (malloc n)
  (let ((r (cexp "box (malloc (%s))" n)))
    (if (%zero? r) #f r)))

(define (free p)
  (cexp "(free (unbox (%s)), PXLL_UNDEFINED)" p))

(let ((p (malloc 1024)))
  (%printn p)
  (free p)
  99)
	     
	     
    
    