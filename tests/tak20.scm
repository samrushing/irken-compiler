
(define (tak x y z)
  (if (%ge? y x)
      z
      (tak (tak (%- x 1) y z)
	   (tak (%- y 1) z x)
	   (tak (%- z 1) x y))))

(let loop ((n 20))
  (tak 18 12 6)
  (if (%zero? n)
      "done"
      (loop (%- n 1))))
      
  