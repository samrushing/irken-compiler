;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/enum.scm")

(make-enum color (red 0) (green 1) (blue 2))
(printn (color->int (color:red)))
(printn (color->int (color:green)))
(printn (color->int (color:blue)))
(printf "blue is named " (sym (color->name (color:blue))) "\n")
(printf "1 is named " (sym (color->name (int->color 1))) "\n")
