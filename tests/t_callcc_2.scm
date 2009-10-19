
(include "lib/core.scm")

(define (make-int-generator)
  (make-generator
   (lambda (consumer)
     (let loop ((n 0))
       (consumer n)
       (loop (+ n 1))))))

(let* ((g (make-int-generator)))

  (define (every-other)
    (let ((first (g)))
      (cond ((= 0 first) first)
            (else (g)))))

  (printn g)
  (printn (every-other))
  (printn (every-other))
  (printn (every-other))
  (printn (every-other))
  (printn (every-other))
  )
