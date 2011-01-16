
(datatype bool (:true) (:false))

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (bigger z n)
  (%rextend/c z n))

(let ((x {a=1 b=#t})
      (y (bigger x 34)))
  (printn x)
  (printn y)
  (let ((z (bigger y 19)))
    (printn z)))

  