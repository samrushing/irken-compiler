;; -*- Mode: Irken -*-

;; note: this is in derived.scm (which is auto-included)
;; (datatype symbol (:t string int)) ;; string unique-id

(define symbol->string
  (symbol:t str _) -> str
  )

(define symbol->index
  (symbol:t _ index) -> index
  )

(define symbol-table-size 0)
(define the-symbol-table (tree/empty))

(define (intern-symbol sym)
  (set! the-symbol-table
	(tree/insert the-symbol-table
		     string-compare
		     (symbol->string sym)
		     sym))
  (set! symbol-table-size (+ 1 symbol-table-size))
  sym
  )

(define (string->symbol str)
  (let ((probe (tree/member the-symbol-table string-compare str)))
    (match probe with
      (maybe:no) -> (intern-symbol (symbol:t str symbol-table-size))
      (maybe:yes sym) -> sym
      )))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (symbol-cmp s1 s2)
  (string-compare (symbol->string s1) (symbol->string s2)))

(define (symbol-index<? s1 s2)
  (< (symbol->index s1) (symbol->index s2)))

(define (symbol-index-cmp s1 s2)
  (int-cmp (symbol->index s1) (symbol->index s2)))

(define (get-internal-symbols)
  (%backend (c llvm)
    (%%cexp (vector symbol) "(object *) pxll_internal_symbols"))
  (%backend bytecode
    (list->vector (%%cexp (list symbol) "gist")))
  )

(define (initialize-symbol-table)
  (let ((v (get-internal-symbols)))
    (set! the-symbol-table (tree/empty)) ;; necessary because of problems with topological sort
    (for-range i (vector-length v)
      (intern-symbol v[i]))
    ))

(initialize-symbol-table)
