
(include "lib/core.scm")
(include "lib/pair.scm")

(let ((v (list->vec16 '(1 2 3 4 5))))
  (printn v)
  (printn v[4])
  )
