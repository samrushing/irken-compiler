(include "lib/random.scm")

(let loop ((n 100))
  (if (%eq? n 0)
      #f
      (begin
	(%printn (random))
	(loop (%- n 1)))))
 