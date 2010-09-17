
(include "lib/core.scm")

(define thing
  {a=x b=2} -> x
  {a=3 b=y} -> y
  {a=m b=n} -> (+ m n)
  )

;; => 
;; (define (thing r)
;;   (match r.a r.b with
;;     x 2 -> x
;;     3 y -> y
;;     ))

(printn (thing {a=3 b=1}))
(printn (thing {a=3 b=2}))
(printn (thing {a=4 b=5}))

