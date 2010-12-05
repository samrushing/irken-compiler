;; -*- Mode: Irken -*-

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

  


	