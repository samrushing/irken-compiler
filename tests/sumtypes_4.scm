
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(datatype option
  (union 
    (empty)
    (full ?)
    ))

(let* ((x (option/full 19))
       (y (option/full #f))
       (z (option/empty))
       )
  (printn x)
  (printn y)
  (printn z)
  )

