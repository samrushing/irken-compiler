
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(let ((v (%make-vec16 25)))
  (printn v)
  (set! v[19] 34)
  (printn v)
  v[19]
  )
