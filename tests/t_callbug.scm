;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")

;; trying to repro a bug I found when working on self/match.scm, where I
;;  accidentally invoked a symbol arg... somehow the solver let it slide,
;;  dunno why.  If I changed (tag) to (3) or (rules-stack) I get an error,
;;  somehow the key part of alist/iterate allowed it to get through.

;; (alist/iterate
;;  (lambda (tag rules-stack)
;;    (let ((alt (dt.get (tag)))
;;          (vars0 (nthunk alt.arity new-match-var))
;;          (wild (make-vector alt.arity #t))
;;          (rules1 '()))
     

(define (get s)
  s)

(define thing
  (alist/make
   ('x {get=get n=0})
   ('y {get=get n=1})
   ))

(alist/iterate
 (lambda (k v)
   (let ((x (v.get k)))
     (printn x)))
 thing
 )
