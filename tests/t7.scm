;; testing variants

(let ((x (%vcon/fnord 12))
      (y (%vcon/blort #f))
      )
  ;; three args: (success-cont, failure-cont, sum)
  (%vcase/fnord
   (lambda (a) 5)
   (lambda (b)
     (%vcase/blort
      (lambda (c) (if c 99 34))
      (lambda (d) 19)
      b))
   y))

