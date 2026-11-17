;; -*- Mode: Irken -*-

;; recursive datatypes that do have values (#40 must not reject these):
;;  through a list, a record, a function, or a non-recursive alternative.

(include "lib/basis.scm")

(datatype rose (:node int (list rose)))
(datatype chain (:end) (:link {v=int next=chain}))
(datatype lazy (:thunk (-> lazy)) (:val int))

(define rose-sum
  (rose:node n kids) -> (fold (lambda (k acc) (+ acc (rose-sum k))) n kids))

(define chain-len
  (chain:end) -> 0
  (chain:link r) -> (+ 1 (chain-len r.next)))

(define force
  (lazy:thunk f) -> (force (f))
  (lazy:val n) -> n)

(printn (rose-sum (rose:node 1 (list (rose:node 2 '()) (rose:node 3 '())))))
(printn (chain-len (chain:link {v=1 next=(chain:link {v=2 next=(chain:end)})})))
(printn (force (lazy:thunk (lambda () (lazy:val 7)))))
;; => 6
;; => 2
;; => 7
