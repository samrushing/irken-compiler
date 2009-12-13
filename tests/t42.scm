
(include "lib/core.scm")

(define (my-make-generator producer)
  (let ((ready #f)
        ;; just holding useless continuations
        (caller (getcc))
        (saved-point (getcc)))

    (define (entry-point)
      (set! caller (getcc))
      (if ready
	  (putcc saved-point #f)
	  (producer yield)))

    (define (yield v)
      (set! saved-point (getcc))
      (set! ready #t)
      (putcc caller v))

    entry-point

    ))

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
