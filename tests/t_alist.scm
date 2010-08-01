
(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")

(let ((l0 (alist/make))
      (l1 (alist/make)))
  (alist/push l0 'thing 34)
  (alist/push l1 'blurb #f)
  )

