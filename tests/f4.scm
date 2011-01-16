
;;; OK.  houston, we have a problem.  this should fail.

(define (z x)
  (vcase x
    ((:thing v) (%%cexp (int int -> int) "%0+%1" v 1))))

(let ((y (:thing #\A)))
  (z y))

