;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

;; the old definition of the constant rule in match.py
;;  caused an exponential explosion with code like this,
;;  because the 'default' was getting duplicated at each
;;  level.  Code like this would explode to 250kB of C.

(define thing
  (0 x 1 . rest) -> 1
  (1 2 x 3 4 y)  -> 2
  (2 . z)	 -> 3
  (3 0 x)	 -> 4
  (4 x y z)      -> 5
  (x . y)        -> 6
  ()             -> 7
  )

(thing '(1 2 0 3 4 5))
