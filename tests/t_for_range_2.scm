;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(for-range* i -5 5
  (printf (lpad 5 (int i)) "\n")
  )
