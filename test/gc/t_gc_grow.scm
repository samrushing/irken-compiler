;; -*- Mode: Irken -*-

;; a vector bigger than the heap the program starts with (50M words):
;;  %ensure-heap says how much it needs, and the heap grows to fit.
;;  (it used to run off the end of the heap.)

(include "lib/basis.scm")

(define N 51000000)

(let ((v (make-vector N 0)))
  (set! v[0] 1)
  (set! v[(- N 1)] 2)
  (printn (+ v[0] v[(- N 1)]))
  (printn (vector-length v)))
;; => 3
;; => 51000000

;; and the heap still works afterwards.
(let loop ((i 0) (acc '()))
  (if (< i 100000)
      (loop (+ i 1) (list:cons i acc))
      (printn (length acc))))
;; => 100000
