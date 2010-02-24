
(include "lib/core.scm")
(include "lib/pair.scm")

(let ((l (cons 1 (cons 2 (cons 3 (list:nil))))))
  (let ((t0 (member 3 l =))
	(t1 (length l))
	(t2 (append l l))
	(t3 (range 10))
	(t4 (n-of 5 "five"))
	(t5 (reverse t3))
	)
    {a=t0 b=t1 c=t2 d=t3 e=t4 f=t5}))
