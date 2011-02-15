(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")

(printn (foldr
	 (lambda (x al) (alist:entry x 0 al))
	 (alist:nil)
	 '(0 1 2 3 4 5)))

