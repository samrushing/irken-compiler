

;; note how <y> is re-assigned.  nasty!
(let* ((x 19)
       (y 10)
       (y (%+ x y)))
  (%printn y))
