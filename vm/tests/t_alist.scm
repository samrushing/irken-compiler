;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")

(define l0 (alist/make))

(alist/push l0 19 34)
(alist/push l0 21 19)
(alist/push l0 47 12)

(print l0)
(print (alist/lookup l0 47))
(print (alist/lookup l0 33))

