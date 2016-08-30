;; -*- Mode: Irken -*-

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
		     string<?
		     (symbol->string sym)
		     sym))
  (set! symbol-table-size (+ 1 symbol-table-size))
  sym
  )

(define (string->symbol str)
  (let ((probe (tree/member the-symbol-table string<? str)))
    (match probe with
      (maybe:no) -> (intern-symbol (symbol:t str symbol-table-size))
      (maybe:yes sym) -> sym
      )))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (symbol-index<? s1 s2)
  (< (symbol->index s1) (symbol->index s2)))

(define (initialize-symbol-table)
  (let ((v (%%cexp (vector symbol) "(object *) pxll_internal_symbols")))
    (set! the-symbol-table (tree/empty)) ;; necessary because of problems with topological sort
    (for-range i (vector-length v)
       (intern-symbol v[i]))
    ))

(initialize-symbol-table)
