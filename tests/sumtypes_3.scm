
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(datatype option
  (union 
    (empty int)
    (full ?)
    ))

(let* ((x (option/full 19))
       (y (option/full #f))
       (z (option/empty 0))
       )
  (printn x)
  (printn y)
  )

