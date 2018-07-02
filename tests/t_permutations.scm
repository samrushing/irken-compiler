;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/combinatorics.scm")

(for perm (permutations '(0 1 2 3 4))
  (printn perm))
