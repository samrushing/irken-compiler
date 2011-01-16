(include "lib/core.scm")
(include "lib/pair.scm")

;; we need a partial evaluator?, the inliner can't
;;   see <eq?> and make this Do The Right Thing.
;; XXX see appel "modern compiler implementation..." 15.4.
;;     according to that we need loop-preheader and loop-invariant
;;     optimizations.
(printn (member? 0 '(0 1 2 3) eq?))






