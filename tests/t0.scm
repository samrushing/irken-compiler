(let loop ((n 100)
	   (a 0))
  (if (%zero? n)
      a
      (loop (%- n 1) (%+ a n))))
