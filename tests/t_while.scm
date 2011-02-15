;; -*- Mode: Irken -*-

(include "lib/core.scm")

(let ((n 100))
  (while (> n 0)
	 (printn n)
	 (set! n (- n 1))
	 ))
