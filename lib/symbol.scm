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
(define the-symbol-table (initial-symbol-table))

(define (intern-symbol sym)
  (tree/insert! the-symbol-table string-compare (symbol->string sym) sym)
  (inc! symbol-table-size)
  sym
  )

(define (string->symbol str)
  (match (tree/member the-symbol-table string-compare str) with
    (maybe:no) -> (intern-symbol (symbol:t str symbol-table-size))
    (maybe:yes sym) -> sym
    ))

(define symbol<?
  (symbol:t s0 _) (symbol:t s1 _) -> (string<? s0 s1))

(define symbol-cmp
  (symbol:t s0 _) (symbol:t s1 _) -> (string-compare s0 s1))

(define symbol-index<?
  (symbol:t _ i0) (symbol:t _ i1) -> (< i0 i1))

(define symbol-index-cmp
  (symbol:t _ i0) (symbol:t _ i1) -> (int-cmp i0 i1))

(define (get-internal-symbols)
  (%backend c
    (%%cexp (vector symbol) "(object *) irk_internal_symbols"))
  (%backend llvm
    (%llvm-get ("@irk_internal_symbols_p" (-> (vector symbol)) #f)))
  (%backend bytecode
    (list->vector (%%cexp (list symbol) "gist")))
  )

(define (initial-symbol-table)
  (let ((v (get-internal-symbols))
        (map (tree/empty)))
    (for-range i (vector-length v)
      (tree/insert! map string-compare (symbol->string v[i]) v[i]))
    (set! symbol-table-size (vector-length v))
    map
    ))
