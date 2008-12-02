(define (decrement x)
  (%- (%- (%+ x x) x) 1))

(let loop ((n 1000000))
  (if (%zero? n)
      n
      (loop (decrement n))))
