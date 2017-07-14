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
  (let tag ((name val) ...) body1 body2 ...)
  -> (letrec ((tag (function tag (name ...) #f body1 body2 ...)))
       (tag val ...))

  ;; XXX this still does not allow free mixing of single and 
  ;;  multiple bindings, because of the catch-all let->let-splat
  ;;  below.  To do this correctly we need to cascade the two macros,
  ;;  which might require some hackery to avoid chaining lets...
  ;; multiple-value-bind
  (let (((name0 name1 ...) val) bind ...) body ...)
  -> (let-values (((name0 name1 ...) val))
       (let (bind ...)
         body ...))
  
  ;; normal <let> here, we just rename it to our core
  ;; binding construct, <let_splat>
  (let ((name val) ...) body1 body2 ...)
  -> (let-splat ((name val) ...) body1 body2 ...)

  )

;; functions using this construct must return their multiple
;;  values using the (:tuple ...) polyvariant.

(defmacro let-values
  (let-values (((x y ...) val)) body ...)
  -> (match val with (:tuple x y ...) -> (begin body ...))
  (let-values (((x y ...) val) bind ...) body ...)
  -> (match val with (:tuple x y ...) -> (let-values (bind ...) body ...))
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

(defmacro for-vector
  (for-vector vname vec body ...)
  -> (let (($v vec)) ;; avoid duplicating <vec> expression.
       (for-range $i (vector-length $v)
	 (let ((vname $v[$i]))
	   body ...))))

(defmacro forever
  (forever body ...)
  -> (let $loop () body ... ($loop)))


;; make an iterator from a 'visit' function.
;; XXX: needs more work - the ability to specify other args
;;      to visitfun...
(defmacro iterator
  (iterator visitfun ob (v0 ...))
  -> (make-generator
      (lambda (consumer)
	(visitfun ob (lambda (v0 ...) (consumer (maybe:yes v0 ...))))
	(forever (consumer (maybe:no))))))

;; this is a generic 'for' iterator, that will iterate
;;  over any generator using the 'maybe' type for its
;;  items...

(defmacro for

  ;; multiple variables
  (for (var0 ...) generator-exp body0 ...)
  -> (let (($gen generator-exp))
       (let $genloop (($item ($gen)))
	 (match $item with
	   (maybe:no) -> #u
	   (maybe:yes (:tuple var0 ...))
	   -> (begin body0 ...
		     ($genloop ($gen))))))

  ;; [about arity: I'm not sure if the confusion possible here is worth
  ;;  the performance advantage of avoiding the :tuple wrapper.]

  ;; single-variable
  (for var generator-exp body0 ...)
  -> (let (($gen generator-exp))
       (let $genloop (($item ($gen)))
	 (match $item with
	   (maybe:no) -> #u
	   (maybe:yes var)
	   -> (begin body0 ...
		     ($genloop ($gen))))))
  )
