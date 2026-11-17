;; -*- Mode: Irken -*-

;; a type alias with a type variable, used without arguments, means its
;;  definition: in a datatype its 'a is the datatype's own (#78: this
;;  crashed generate-metadata, and every use shared one unknown type).

(include "lib/basis.scm")

(typealias cell {val='a})
(typealias seq (list 'a))

(datatype thing
  (:t cell)
  (:s seq))

(define get
  (thing:t c) -> c.val
  (thing:s (hd . _)) -> hd
  (thing:s ()) -> (raise (:Empty)))

(define (cell-of x) : ('b -> (cell 'b)) {val=x})

(printn (get (thing:t {val=3})))
(printn (get (thing:s (list "x" "y"))))
(printn (get (thing:t (cell-of #\z))))
;; => 3
;; => "x"
;; => #\z
