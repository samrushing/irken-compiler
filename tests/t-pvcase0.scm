;; -*- Mode: Irken -*-

(require "lib/core.scm")

;; tags are assigned in order of appearance:
(define thing
  (:zero)        -> 0                   ;; 1
  (:one a)       -> a                   ;; 3
  (:two a b)     -> (+ a b)             ;; 0
  (:three a b c) -> (+ a b c)           ;; 2
  )

;; this is their order of appearance:
(printn (thing (:two 3 4)))             ;; 0
(printn (thing (:zero)))                ;; 1
(printn (thing (:three 3 4 7)))         ;; 2
(printn (thing (:one 99)))              ;; 3

