
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(let ((o1 #(1 2 3)))
  (printn o1[0])
  (printn o1[5])
  )
