;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(let ((t (make-array (3 3 3) 99))
      (v 0))
  (for-range i 3
    (for-range j 3
      (for-range k 3
        (set! t[i][j][k] v)
        (inc! v))))
  t)

