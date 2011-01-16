;; testing variants
;; this will currently fail because case <else> is not yet implemented

(datatype bool (:true) (:false))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(let ((x (:fnord 12))
      (y (:blort #f))
      (z (:shlum "howdy"))
      )
  ;; three args: (success-cont, failure-cont, sum)
  (&vcase (fnord)
   (lambda (a) (+ a 3))
   (lambda (b)
     (&vcase (blort)
      (lambda (c) (if c 99 34))
      (lambda (d) 19)
      b))
   z))

