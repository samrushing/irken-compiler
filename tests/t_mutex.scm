;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "doom/doom.scm")
(require "doom/sync.scm")

(define the-mutex (mutex/make))

(define (worker n)
  (with-mutex the-mutex
     (debugf "worker: " (int n) "\n")
     (when (and (> n 2) (< n 6))
       (timeq/sleep 3000))))

(for-range i 8
  (poller/fork (lambda () (worker i))))

(poller/wait-and-schedule)
