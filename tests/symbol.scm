
(include "lib/core.scm")
(include "lib/vector.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")

(define (make-string-equal?-tree)
  (make-tree string-<? #f))

(define the-symbol-table #f)

(define (string->uninterned-symbol str)
  (let ((sym (%make-tuple #x1c 1)))
    (%%cexp "%s[1] = %s" sym str)
    sym))

(define (string->symbol str)
  (let ((probe (tree-member the-symbol-table str)))
    (if probe
	probe
	(let ((sym (string->uninterned-symbol str)))
	  (set! the-symbol-table (tree-insert the-symbol-table str sym))
	  sym))))

(set! the-symbol-table (make-string-equal?-tree))

(let ((sym0 (string->symbol "hello"))
      (sym1 (string->symbol "hello")))
  (%printn (%eq? sym0 sym1)))

