;; -*- Mode: Irken -*-

;; record patterns with different fields.

;; expect-error: f_match_record.scm:11:3: error: this record pattern has fields {a c}, but the first one has {a b}

(include "lib/basis.scm")

(define g
  {a=x b=y} -> x
  {a=x c=z} -> z)

(printn (g {a=1 b=2}))
