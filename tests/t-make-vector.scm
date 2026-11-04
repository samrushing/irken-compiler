;; -*- Mode: Irken -*-

(require "lib/core.scm")

(let ((v (make-vector 10 #t)))
  (printn v)
  (printn v[5])
  (set! v[5] #f)
  (printn v[5])
  (printn v)
  (printn (vector-length v))
  (printn (vector-length #()))
  )


