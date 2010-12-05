
;; this should probably become a datatype, or at least a record.

;; XXX a big question remains about 'lisp symbols' vs what we will
;;   have: can/should we have new symbols available at runtime?  If we
;;   say "no", then there is no need for a runtime map, we can find
;;   them all at compile time.  This question is more important now
;;   that we emit literal constants, because if we intend for these
;;   funs to work correctly it needs to be populated with the set of
;;   all symbols found at compile time.

(define (string->uninterned-symbol str)
  (symbol:t str))

(define symbol->string
  (symbol:t str) -> str
  )

(define the-symbol-table (tree:empty))

(define (intern-symbol sym)
  (set! the-symbol-table
	(tree/insert the-symbol-table
		     string<?
		     (symbol->string sym)
		     sym))
  sym
  )

(define (string->symbol str)
  (let ((probe (tree/member the-symbol-table string<? str)))
    (match probe with
      (maybe:no) -> (intern-symbol (string->uninterned-symbol str))
      (maybe:yes sym) -> sym
      )))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (initialize-symbol-table)
  (let ((v (%%cexp (vector symbol) "(object *) pxll_internal_symbols")))
    (let loop ((i 0))
      (if (= i (vector-length v))
	  #u
	  (begin
	    (intern-symbol v[i])
	    (loop (+ i 1)))))))

(initialize-symbol-table)

  


	