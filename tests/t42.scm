
(include "lib/core.scm")

(define (make-int-generator n)
  (make-generator
   (lambda (consumer)
     (let loop ((n 0))
       (consumer n)
       (loop (+ n 1))))))
                 
(let ((g (make-int-generator 100)))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  )
