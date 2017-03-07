;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define (thing)
  (let loop ((n 10000))
    (if (= n 100)
        (%exit #f 42)
        (loop (- n 1)))))

(thing)

