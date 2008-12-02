
(define thing #f)

(define (plus x y)
  (%+ x y))

(define (set-thing)
  (set! thing #t))

(set-thing)
(%printn thing)
(%printn (plus 10 20))
