;; -*- Mode: Irken -*-

;; one rule of a pattern-matching define has an extra pattern: at that rule.

;; expect-error: f_match_rule_arity.scm:12:3: error: this rule has 2 patterns, but the others have 1

(include "lib/basis.scm")

(define B
  0 -> "no more bottles"
  1 -> "1 bottle"
  x y -> (format (int x) " bottles")
  )

(printn (B 3))
