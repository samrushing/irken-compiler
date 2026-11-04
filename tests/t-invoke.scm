;; -*- Mode: Irken -*-

;; (require "lib/core.scm")
;; (require "lib/pair.scm")
;; (require "lib/alist.scm")
;; (require "lib/string.scm")
;; (require "lib/format.scm")

(define (+ a b) (%llarith add a b))
(define (- a b) (%llarith sub a b))
(define (= a b) (%llicmp eq a b))

;; (define (+ a b) (%%cexp (int int -> int) "%s+%s" a b))
;; (define (- a b) (%%cexp (int int -> int) "%s-%s" a b))
;; (define (= a b) (%%cexp (int int -> int) "%s==%s" a b))
;; (define (zero? a) (%%cexp (int -> bool) "%s==0" a))

(define (thing n)
  (if (= n 0)
      0
      (+ 1 (thing (- n 1)))))

(thing 3)



