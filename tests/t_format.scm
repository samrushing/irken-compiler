;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(printn (format "Int: " (int 3) " Hex: " (hex 16384) " Bool: " (bool #f) " woo hoo!"))
(print-string (format-join " " "testing" (int 1) (bool #t) (char #\A) "\n"))
(printn (format "list(" (join id "," '("sexp")) ")" ))
(printn (format (lpad 10 "hi-") (rpad 10 "there")))
(printn (rpad 10 "xyz"))
(printn (lpad 10 "xyz"))
(printn (cpad 10 "xyz"))
(printn (format "thing"))

