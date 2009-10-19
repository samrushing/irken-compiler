
(include "lib/core.scm")

;(datatype pair
;  (product ? ? ))

;(define (add-pair p:pair)
;  (+ p{0} p{1}))

(let ((p0 (pair 1 2))
      (p1 (pair #t #f))
      (p2 (pair "howdy" 34))
      (a0 #(1 2 3))
      )
  (printn p0)
  (printn p1)
  (printn p2)
  (printn a0[0])
  (printn p2{0})
  ;; will fail
  ;; (printn p2{5})
  (printn p2{1})
  (printn (+ a0[0] a0[1]))
  (printn (+ p1{0} p1{1}))
;  (printn (add-pair p0))
  )
