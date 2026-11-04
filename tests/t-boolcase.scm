;; -*- Mode: Irken -*-

(require "lib/core.scm")
(require "lib/pair.scm")

(define (thing)
  (match (:tuple #\a (list 30724728358 392411733372922745)) with
    (:tuple _    ())  -> 0
    (:tuple #\b digs) -> 1
    (:tuple #\c digs) -> 2
    (:tuple _ _)      -> 3
    ))

(thing)
