;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(define thing0 (list (maybe:yes 1) (maybe:no) (maybe:yes 3)))

(define (t0)
  (for-list item thing0
    (printf " - " (int (if-maybe val item (+ val 3) 0)) "\n")
    ))

(t0)
