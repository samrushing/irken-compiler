
(define (bigger z)
  (%rextend/c z 0))

(let ((x {a=1 b=#t})
      (y (bigger x))
      (z (bigger y)))
  z)
  