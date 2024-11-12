;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(define (dumpsyms)
  (for-map k v the-symbol-table
    (printf (sym v) " " (int (symbol->index v)) "\n")))

(printn (get-internal-symbols))

(printn (eq? 'thingy (string->symbol "thingy")))
(dumpsyms)
(let ((s0 (string->symbol "abc"))
      (s1 (string->symbol "def"))
      )
  (printn (symbol->index 'thingy))
  (printn (symbol->index s0))
  (printn (symbol->index s1))
  (printn the-symbol-table)
  (dumpsyms)
  )

