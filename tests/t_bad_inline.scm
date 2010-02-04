;; should return 10, not 5!

(let ((n 10))
  ((lambda (x) (set! n 5) x) n))

