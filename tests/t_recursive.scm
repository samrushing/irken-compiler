;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(define length
  acc {next=(maybe:yes x)} -> (length (+ acc 1) x)
  acc {next=(maybe:no)}    -> acc

(length 0 {next=(maybe:no)})

