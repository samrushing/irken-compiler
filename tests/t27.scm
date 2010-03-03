
(define (id x) x)

(let ((x 19)
      (y (id #\A))
      )
  ((id id) (id x)))


