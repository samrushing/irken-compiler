
;;; OK.  houston, we have a problem.  this should fail.

(define (z x)
  (vcase x
    ((:thing v) (%%cexp (int int -> int) "%s+%s" v 1))))

(let ((y (:thing #\A)))
  (z y))

