

(define (a)
  (b))

(define (b)
  (c)
  (e)
  (f))

(define (c)
  (d)
  (g))
  
(define (d)
  (c)
  (h))

(define (e)
  (a)
  (f))

(define (f)
  (g))

(define (g)
  (h)
  (f))

(define (h)
  (h))
  
  