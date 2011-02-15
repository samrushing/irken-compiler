;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(for-range x (+ 5 5)
   (for-range y 10
      (print-string
       (format "x=" (int x) " y=" (int y) " ")))
   (newline)
   )

(printn (map-range i 10 i))
(printn (map-range i 10 (printn i) (+ i i)))
