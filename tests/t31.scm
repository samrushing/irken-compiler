
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (z x)
  (vcase x
    ((:thing v) (+ v 1))))

;;; OK.  houston, we have a problem.  this should fail.

(let ((y (:thing #\A)))
  (printn (z y))
  )
