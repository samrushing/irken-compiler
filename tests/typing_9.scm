
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(let ((o1 #(1 2 3))
      (o2 #("hello" "there"))
      )
  (printn o1[0])
  (set! o1[1] 5)
  (printn o1)
  (printn o2)
  )
