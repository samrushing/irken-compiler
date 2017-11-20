;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(define fact
  0 a -> a
  n a -> (fact (- n 1) (* n a))
  )

#(0 1 2 3 4 (fact 5 1) 6)
;; (list->vector
;;  (list:cons
;;   0 (list:cons
;;      1 (list:cons
;;         2 (list:cons
;;            3 (list:cons
;;               4 (list:cons
;;                  (fact 5 1)
;;                  (list:cons 6 (list:nil)))))))))
