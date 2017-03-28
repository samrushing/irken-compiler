;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define (get-closure)
  (%%cexp (-> (irken-closure (string -> int))) "getc"))

(define (thing s)
  (let ((closure (get-closure)))
    (%%cexp ((irken-closure (string -> int)) int string -> int) "irk" closure 1 s)
    ))

(print (thing "--- testing ---\n"))
(print (thing "... and testing again!\n"))



