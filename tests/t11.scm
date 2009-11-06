(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(let ((x (:blurb))
      (y (:blort 12 13 14)))
  (vcase y
     ((:blurb) 20)
     ((:blort a b c) (+ a (+ b c)))))
     