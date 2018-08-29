;; -*- Mode: Irken -*-

(require "lib/core.scm")
(require "lib/pair.scm")

;; CPS transform, with defmacro.
;; http://matt.might.net/articles/cps-conversion/

;; modified to support arity > 1.

(defmacro T-k
  (T-k (<lambda> x ...) k)
  -> (k (M (lambda x ...)))
  (T-k (f a ...) k)
  -> (T-kargs () () (f a ...) k)
  (T-k exp k)
  -> (k (M exp))
  )

(defmacro T-c
  (T-c (<lambda> x ...) c)
  -> (c (M (lambda x ...)))
  (T-c (f a ...) c)
  -> (T-cargs () () (f a ...) c)
  (T-c exp c)
  -> (c (M exp))
  )

(defmacro M
  (M (<lambda> (var) exp))
  -> (lambda (var $k) (T-c exp $k))
  (M exp)
  -> exp
  )

;; T-cargs collects expressions and names...
(defmacro T-cargs
  (T-cargs (name ...) (val ...) () c)
  -> (T-args0 (name ...) (val ...) (name ... c))
  (T-cargs (name ...) (val ...) (exp0 exp ...) c)
  -> (T-cargs (name ... $x) (val ... exp0) (exp ...) c)
  )

;; ...same here, but different. [notice (lambda ($rv) ...)]
(defmacro T-kargs
  (T-kargs (name ...) (val ...) () k)
  -> (T-args0 (name ...) (val ...) (name ... (lambda ($rv) (k $rv))))
  (T-kargs (name ...) (val ...) (exp0 exp ...) k)
  -> (T-kargs (name ... $x) (val ... exp0) (exp ...) k)
  )

;; T-args0 produces the final expression, wrapped with T-k calls.
;; [note: both T-cargs and T-kargs can use this macro]
(defmacro T-args0
  (T-args0 () () exp)
  -> exp
  (T-args0 (name0 name1 ...) (val0 val1 ...) exp)
  -> (T-args0 (name1 ...) (val1 ...) (T-k val0 (lambda (name0) exp)))
  )

(define (add a b k)
  (k (+ a b)))

(define (sub a b k)
  (k (- a b)))

(printn (T-c (add 3 4) id))
;; transforms to:
;; (printn
;;  ((lambda ($x2)
;;     ((lambda ($x1)
;;        ((lambda ($x0)
;;           ($x0 $x1 $x2 id))
;;         add))
;;      3))
;;   4))
;;
;; [irken's compiler unwraps all those lambdas into `lets` such
;;  that the final expression is essentially `(printn (+ 3 4))`]

(printn
 (T-c (add
       (sub 19 3)
       (add 5 6))
      id))

