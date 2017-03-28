;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(for-range i 1000
  (make-string (* 1024 1024 5)))
