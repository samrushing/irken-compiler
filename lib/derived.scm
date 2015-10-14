;; -*- Mode: Irken -*-

(datatype symbol (:t string int)) ;; string unique-id

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

  ;; named let, strict version
  ;; http://groups.google.com/group/comp.lang.scheme/msg/3e2d267c8f0ef180
  ;; bad: this causes all recursive functions to 'escape' and thus lose tr_call!
  ;;   (let tag ((name val) ...) body1 body2 ...)
  ;;   -> ((letrec ((tag (lambda (name ...) body1 body2 ...)))
  ;; 	tag) val ...)

  ;; not-strict version (using (function $tag ...) names the lambda)
  (let $tag ((name val) ...) body1 body2 ...)
  -> (letrec (($tag (function $tag (name ...) #f body1 body2 ...)))
       ($tag val ...))

  ;; normal <let> here, we just rename it to our core
  ;; binding construct, <let_splat>
  (let ((name val) ...) body1 body2 ...)
  -> (let-splat ((name val) ...) body1 body2 ...)

  )

;; simplified <cond>
(defmacro cond
  (cond (<else> e1 e2 ...)) -> (begin e1 e2 ...)
  (cond ( test  e1 e2 ...)) -> (if test (begin e1 e2 ...))
  (cond ( test  e1 e2 ...)
	c1 c2 ...) -> (if test
			  (begin e1 e2 ...)
			  (cond c1 c2 ...)))

(defmacro while
  (while test body ...)
  -> (let $loop ()
       (if test
	   (begin body ... ($loop))
	   #u)))

(defmacro when
  (when test body ...)
  -> (if test (begin body ...)))

(defmacro for-range
  (for-range vname num body ...)
  -> (let (($n num))
       (let $loop ((vname 0))
	 (if (= vname $n)
	     #u
	     (begin body ...
		    ($loop (+ vname 1)))))))

(defmacro forever
  (forever body ...)
  -> (let $loop () body ... ($loop)))
