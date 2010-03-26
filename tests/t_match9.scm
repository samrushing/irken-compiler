
(include "lib/core.scm")

;; test <match>

(define (frob n)
  (match (* n n) with
     0 -> 'zed
     1 -> 'one
     4 -> 'two
     x -> (error "bad arg to frob")))

{a=(frob 0) b=(frob 1) c=(frob 2)}
