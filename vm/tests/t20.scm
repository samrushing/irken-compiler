;; -*- Mode: Irken -*-

(include "lib/core.scm")

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (length l)
  (define fun
    ()        acc -> acc
    (hd . tl) acc -> (fun tl (+ 1 acc)))
  (fun l 0))

(length (list:cons 1 (list:cons 2 (list:cons 3 (list:nil)))))
