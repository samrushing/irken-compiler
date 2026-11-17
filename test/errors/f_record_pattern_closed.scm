;; -*- Mode: Irken -*-

;; a record pattern without `...` matches records with exactly those
;;  fields (#34): this one is given an extra field `c`.

;; expect-error: f_record_pattern_closed.scm:11:12: error: type error: no field (or variant) `c` here

(include "lib/basis.scm")
(define f
  {a=x b=y} -> (+ x y))
(printn (f {a=1 b=2 c=3}))
