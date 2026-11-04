;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(define thing (list 1 2 3))

(for-range i (- 3 (length thing))
  (printf "i? " (int i) "\n")
  (let ((x (nth thing i)))
    (printf "x[i] = " (int x) "\n")
  ))
