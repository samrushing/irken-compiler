;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(printn (fold + 0 '(1 2 3 4 5)))
(printn (foldr list:cons '() '(a b c d e fg)))

