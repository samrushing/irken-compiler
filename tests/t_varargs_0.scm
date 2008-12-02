
(define (thing . args)
  (let ((x 1)
	(y 2))
    (%printn x)
    (%printn args)
    (let ((z 4))
      (%printn z)
      (%printn args))))

(thing 1 2 3 4 5)
