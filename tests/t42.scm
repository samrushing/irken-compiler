
(include "lib/core.scm")

(define (make-int-generator n)
  (make-generator
   (lambda (consumer)
     (let loop ((n n))
       (consumer n)
       (loop (+ n 1))))))
                 
(let ((g (make-int-generator 42)))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  (printn (g))
  )
