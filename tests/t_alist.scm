
(include "lib/core.scm")
(include "lib/alist.scm")

(let ((l (alist/new)))
  (set! l (alist/add l 'thing 34))
  (printn l)
  (printn (alist/lookup l 'thing))
  (printn (alist/lookup l 'not'))
  )
