
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

;; (datatype exp
;;   (union 
;;     (literal int)
;;     (sum exp exp)
;;     (prod exp exp)
;;     ))

(let* ((x (literal/integer 19))
       (y (literal/child x)))
  (printn x)
  (typecase x
    (integer (printn 3141) (+ x 3))
    (boolean 100)
    (string 200)
    (child 300))
  (printn y)
  )

