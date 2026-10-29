;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define v (make-vector 5 0))
(for-range i 5
  (set! v[i] (* i i)))
(printn v)
(printn (list (vector-length v) v[2] v[4]))
;; => #(0 1 4 9 16)
;; => (5 4 16)

(printn #(1 2 3))
(printn (vector->list #(#\a #\b)))
(printn (list->vector (list "x" "y")))
;; => #(1 2 3)
;; => (#\a #\b)
;; => #("x" "y")

;; vectors of vectors
(define grid (make-vector 3 #()))
(for-range i 3
  (set! grid[i] (make-vector 3 i)))
(set! grid[1][2] 99)
(printn grid)
;; => #(#(0 0 0) #(1 1 99) #(2 2 2))

;; an empty vector, and sorting one
(printn (vector-length #()))
(printn (sort-vector < #(5 3 9 1)))
;; => 0
;; => #(1 3 5 9)

