
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(datatype literal
  (union
    (integer int)
    (boolean bool)
    (string string)
    (child literal)
    )
  )

(let* ((x (literal/integer 19))
       (y (literal/child x)))
  (printn x)
  (typecase literal x
    ((integer i) (printn 3141) (+ i 3))
    ((boolean b) 100)
    ((string s) 200)
    ((child c) 300))
  (printn y)
  )

