
;; this should probably become a datatype, or at least a record.

;; XXX a big question remains about 'lisp symbols' vs what we will
;;   have: can/should we have new symbols available at runtime?  If we
;;   say "no", then there is no need for a runtime map, we can find
;;   them all at compile time.  This question is more important now
;;   that we emit literal constants, because if we intend for these
;;   funs to work correctly it needs to be populated with the set of
;;   all symbols found at compile time.

(define (string->uninterned-symbol str)
  ;; This is the *only* place that %%make-tuple is used.  If we rewrite
  ;;   this using a datatype we can get rid of the make_tuple node.
  ;;   What special knowledge does the runtime have of symbols?  Can't we
  ;;   just use the same tricks used for the (list) datatype?
  (%%make-tuple symbol symbol str))

(define (symbol->string sym)
  (%%cexp (symbol -> string) "%s[1]" sym))

(define the-symbol-table (tree:empty))

(define (intern-symbol str)
  (let ((sym (string->uninterned-symbol str)))
    (set! the-symbol-table
	  (tree/insert the-symbol-table string<? str sym))
    sym))

(define (string->symbol str)
  (let ((probe (tree/member the-symbol-table string<? str)))
    (vcase maybe probe
      ((:no) (intern-symbol str))
      ((:yes sym) sym))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))
