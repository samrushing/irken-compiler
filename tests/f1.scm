
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (z x)
  (vcase x
    ((:thing v) (+ v 1))))

;;; OK.  houston, we have a problem.  this should fail.

(let ((y (:thing #\A)))
  (printn (z y))
  )
