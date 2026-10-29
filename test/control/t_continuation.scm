;; -*- Mode: Irken -*-

;; first-class continuations: let/cc for early exit, and getcc/putcc.

(include "lib/basis.scm")

;; early exit from a loop
(define (find-first p l)
  (let/cc return
    (for-list x l
      (when (p x) (return (maybe:yes x))))
    (maybe:no)))
(define show-maybe
  (maybe:yes x) -> (format "yes " (int x))
  (maybe:no)    -> "no")
(printn (list (show-maybe (find-first even? (list 1 3 6 7 8))) (show-maybe (find-first even? (list 1 3)))))
;; => ("yes 6" "no")

;; escaping from deep recursion
(define (product l)
  (let/cc k
    (define recur
      ()        -> 1
      (0 . _)   -> (k 0)
      (hd . tl) -> (* hd (recur tl)))
    (recur l)))
(printn (list (product (list 1 2 3 4)) (product (list 1 2 0 4))))
;; => (24 0)

;; printn returns #u, so it can be the body of a one-armed `when`
(when (> (product (list 2 3)) 5)
  (printn 'big))
;; => big
