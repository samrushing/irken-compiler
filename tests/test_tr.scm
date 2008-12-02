(let loop ((n 5))
  (let ((x (%* n n)))
    (%printn x)
    (let ((lenv (%getlenv)))
      (%printn lenv)
      (if (%zero? n)
	  "done"
	  (loop (%- n 1))))))
  