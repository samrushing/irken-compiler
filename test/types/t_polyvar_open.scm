;; -*- Mode: Irken -*-

;; a match on polymorphic variants with a default case should accept
;;  variants it doesn't name: here is-red? sees (:blue).  instead the
;;  typer gives `c` a closed row (just `red`), so passing (:blue) is a
;;  type error ("abs" vs "(pre product)").

;; xfail: c b arm amd rv -- matching a polyvariant with `_` closes the row

(include "lib/basis.scm")

(define (is-red? c)
  (match c with
    (:red) -> #t
    _      -> #f))

(printn (is-red? (:red)))
(printn (is-red? (:blue)))
;; => #t
;; => #f
