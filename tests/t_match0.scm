
(include "lib/core.scm")

(define fact
  0 -> 1
  n -> (* n (fact (- n 1)))
  )

(define fact-iter
  0 a -> a
  n a -> (fact-iter (- n 1) (* n a))
  )

{a=(fact 5) b=(fact-iter 5 1)}
