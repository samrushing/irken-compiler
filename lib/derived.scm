;; -*- Mode: Irken -*-

;; derived expressions

(defmacro and
  (and)                 -> #t
  (and test)            -> test
  (and test1 test2 ...) -> (if test1 (and test2 ...) #f)
  )

;; *not* the same as a Scheme <or>, this returns a boolean.
(defmacro or
  (or)			-> #f
  (or test)		-> test
  (or test1 test2 ...)	-> (if test1 #t (or test2 ...))
  )

(defmacro let

  (let () body ...)
  -> (begin body ...)

  ;; normal <let> here, we just rename it to our core
  ;; binding construct, <let_splat>
  (let ((name val) ...) body1 body2 ...)
  -> (let_splat ((name val) ...) body1 body2 ...)

  ;; named let
;;   (let tag ((name val) ...) body1 body2 ...)
;;   -> (letrec ((tag (lambda (name ...) body1 body2 ...)))
;;        (tag val ...))

  ;; strict version, see http://groups.google.com/group/comp.lang.scheme/msg/3e2d267c8f0ef180
  (let tag ((name val) ...) body1 body2 ...)
  -> ((letrec ((tag (lambda (name ...) body1 body2 ...)))
	tag) val ...)

  )

;; simplified <cond>
(defmacro cond
  (cond (<else> e1 e2 ...)) -> (begin e1 e2 ...)
  (cond ( test  e1 e2 ...)) -> (if test (begin e1 e2 ...))
  (cond ( test  e1 e2 ...)
	c1 c2 ...) -> (if test
			  (begin e1 e2 ...)
			  (cond c1 c2 ...)))


;; because we have pattern matching, we don't really *need* <case>.
;; however, without or-patterns it's not quite as compact.
;; the right solution is to add or-patterns, not to use <case>.

;; XXX cannot be done with the macro system (yet), mostly
;;     because of my use of %thing/hack0/hack1/...
;;
;; nvcase
;; match
;;

;; This converts vcase into &vcase, a primapp that uses success and
;;   failure continuations, which simplifies typing a bit.  Similar
;;   to a cond->if transformation...
(defmacro notvcase

  (vcase var)
  -> (%vfail var)

  (vcase var (<else> body0 ...))
  -> (begin body0 ...)

  (vcase var (((<colon> ignore label) bind0 ...) body0 ...) clause0 ...)
  -> (&vcase (label)
	     (lambda (bind0 ...) body0 ...)
	     (lambda (vval) (vcase vval clause0 ...))
	     var
	     )
  )
  