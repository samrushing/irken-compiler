;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(printn (format "Int: " (int 3) " Hex: " (hex 16384) " Bool: " (bool #f) " woo hoo!"))
(print-string (format-join " " "testing" (int 1) (bool #t) (char #\A) "\n"))

