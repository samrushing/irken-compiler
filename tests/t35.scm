
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(let ()
  (printn 19)
  (printn #\A))
