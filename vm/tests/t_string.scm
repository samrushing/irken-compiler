;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(let ((s "0123456789"))
  (print (string-ref "0123456789" 5))
  (string-set! s 0 #\A)
  s)


