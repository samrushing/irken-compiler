
(define (string->uninterned-symbol str)
  (%%make-tuple symbol symbol str))

(define the-symbol-table (node/empty))

(define (intern-symbol str)
  (let ((sym (string->uninterned-symbol str)))
    (set! the-symbol-table
	  (node:insert the-symbol-table string-<? str sym))
    sym))

(define (string->symbol str)
  (let ((probe (node:member the-symbol-table string-<? str)))
    (typecase maybe probe
      ((no) (intern-symbol str))
      ((yes sym) sym))))

