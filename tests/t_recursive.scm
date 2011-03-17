;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(define zength
  acc {next=(maybe:yes x)} -> (zength (+ acc 1) x)
  acc {next=(maybe:no)}    -> acc)

(zength 0 {next=(maybe:no)})

