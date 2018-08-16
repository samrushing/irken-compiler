;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(define (sort-dirlist dl)
  (sort (lambda (a b) (symbol<? a.kind b.kind)) dl))

(for-list entry (sort-dirlist (listdir sys.argv[1]))
  (printf (lpad 15 (sym entry.kind)) " " entry.name "\n")
  )
