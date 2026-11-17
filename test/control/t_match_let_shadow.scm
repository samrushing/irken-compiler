;; -*- Mode: Irken -*-

;; a `let` in a match body that rebinds a pattern variable in terms of
;;  itself: the init's `x` is the pattern's.  (this was "unknown variable
;;  `x`", and a closed record pattern on a pattern variable, which rebinds
;;  it the same way, failed inside `for-list`.)

(include "lib/basis.scm")

(define g
  (x . _) -> (let ((x (+ x 1))) x)
  ()      -> 0)

(define (f insns)
  (for-list insn insns
    (match insn with
      {src=-2  dst=dst} -> (printn dst)
      {src=src dst=_}   -> (printn src))))

(printn (g (list 41)))
(f (list {src=-2 dst=1} {src=3 dst=4}))
;; => 42
;; => 1
;; => 3
