;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

;; not until we figure out how to call back into irken...

(printn '(a b c d e))
(printn (list->vector '(a b c d e)))
(printn (string->symbol "f"))
(let ((lx '(a b c d e)))
  (printn (string->symbol "f"))
  (printn the-symbol-table)
  (for-map k v the-symbol-table
    (printf "k= " (string k) " v=" (sym v) " index=" (int (symbol->index v)) "\n")
    ))
