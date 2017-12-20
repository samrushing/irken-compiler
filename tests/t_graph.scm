;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/graph.scm")

(define (print-graph g krepr)
  (printf "graph = {\n")
  (for-map k v g
    (printf "  " (krepr k) " : " (join krepr ", " (set->list v)) "\n"))
  (printf "}\n")
  )

(define test-g
  '((foo baz)        ;; these three
    (baz bar)        ;; form a
    (bar foo)        ;; cycle.
    (biff barf)      ;; as do
    (barf snoo snee) ;; these
    (snoo biff)      ;; three.
    (snee)
    (top foo biff)))

(define (make-sample)
  (let ((g (tree/empty)))
    (for-list l test-g
      (tree/insert!
       g
       symbol-index-cmp
       (car l)
       (list->set (cdr l) symbol-index-cmp (set/empty))))
    g))

(let ((g0 (make-sample))
      (g1 (transpose g0 symbol-index-cmp)))
  (print-graph g0 symbol->string)
  (print-graph g1 symbol->string)
  (printn (strongly g0 symbol-index-cmp))
  )
