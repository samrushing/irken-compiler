(define (minus x y)
  (%- x y))

(let loop ((n 1000000))
  (if (%zero? n)
      n
      (loop (minus n 1))))
