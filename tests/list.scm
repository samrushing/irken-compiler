
(include "lib/core.scm")
(include "lib/list.scm")

(printn (cons 3141 '()))

(let ((l1 (cons 5 (cons 4 (cons 3 (cons 2 (cons 1 '()))))))
      (l2 (cons 6 (cons 7 (cons 8 (cons 9 (cons 0 '()))))))
      )
  (printn l1)
  (printn (reverse l1))
  (printn (append l1 l2))
  (printn (length (append l1 l2)))
  (printn (memq 3 l1))
  (printn (memq 7 l1))
  )



