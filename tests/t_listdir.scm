;; -*- Mode: Irken -*-

(include "lib/basis.scm")
;; (lib/map.scm is in basis)

(define (sort-dirlist dl)
  (sort (lambda (a b) (symbol<? a.kind b.kind)) dl))

(for-list entry (sort-dirlist (listdir sys.argv[1]))
  (printf (lpad 15 (sym entry.kind)) " " entry.name "\n")
  )
