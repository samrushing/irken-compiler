;; -*- Mode: Irken -*-

;; records inside datatypes, including open rows ({... ...}).

(include "lib/basis.scm")

(datatype thing
  (:one {x=int y=char ...})
  (:two int))

(define describe
  (thing:one r) -> (format "one " (int r.x) " " (char->string r.y))
  (thing:two n) -> (format "two " (int n)))

(printn (describe (thing:one {x=3 y=#\A})))
(printn (describe (thing:two 9)))
;; => "one 3 A"
;; => "two 9"

;; a closed record type in a datatype, used recursively
(datatype rtree
  (:leaf int)
  (:node {left=rtree right=rtree label=string}))

(define rtree/sum
  (rtree:leaf n) -> n
  (rtree:node r) -> (+ (rtree/sum r.left) (rtree/sum r.right)))

(define rtree/labels
  (rtree:leaf _) -> '()
  (rtree:node r) -> (append (rtree/labels r.left) (list r.label) (rtree/labels r.right)))

(let ((t (rtree:node {left=(rtree:leaf 1)
                     right=(rtree:node {left=(rtree:leaf 2) right=(rtree:leaf 3) label="inner"})
                     label="root"})))
  (printn (rtree/sum t))
  (printn (rtree/labels t)))
;; => 6
;; => ("root" "inner")
