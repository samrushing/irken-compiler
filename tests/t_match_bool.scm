;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define thing
  #t -> 0
  #f -> 1
  )

(printn (thing (bool:false)))
(printn (thing (bool:true)))


