;; -*- Mode: Irken -*-

;; polymorphic variants: constructors that don't belong to a declared
;;  datatype.  exceptions are built on these.

(include "lib/basis.scm")

(define describe
  (:red)       -> "red"
  (:rgb r g b) -> (format "rgb " (int r) "," (int g) "," (int b))
  (:gray n)    -> (format "gray " (int n)))

(printn (map describe (list (:red) (:rgb 1 2 3) (:gray 7))))
;; => ("red" "rgb 1,2,3" "gray 7")


;; tuples are a polymorphic variant too
(let (((a b) (:tuple 1 "two")))
  (printn (list a))
  (printn b))
;; => (1)
;; => "two"
