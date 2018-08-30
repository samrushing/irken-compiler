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

;; note: in Irken, all `let` are `let*`.

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

  ;; multiple-value-bind
  ;; functions using this construct must return their multiple
  ;;  values using the (:tuple ...) polyvariant.
  (let (((name0 name1 ...) val)) body ...)
  -> (match val with
       (:tuple name0 name1 ...)
       -> (begin body ...))
  (let (((name0 name1 ...) val) bind ...) body ...)
  -> (match val with
       (:tuple name0 name1 ...)
       -> (let (bind ...)
            body ...))

  ;; normal <let> here, we just rename it to our core
  ;; binding construct, <let_splat>
  ;; note: these cascading let-splat forms will be combined
  ;;  by the optimize phase when possible.
  (let (bind0) body ...)
  -> (let-splat (bind0) body ...)
  (let (bind0 bind1 ...) body ...)
  -> (let-splat (bind0)
        (let (bind1 ...) body ...))

  )

;; simplified <cond>
(defmacro cond
  (cond (<else> e1 e2 ...)) -> (begin e1 e2 ...)
  (cond ( test  e1 e2 ...)) -> (if test (begin e1 e2 ...))
  (cond ( test  e1 e2 ...)
	c1 c2 ...) -> (if test
			  (begin e1 e2 ...)
			  (cond c1 c2 ...)))

(defmacro inc!
  (inc! n)   -> (set! n (+ n 1))
  (inc! n m) -> (set! n (+ n m))
  )

(defmacro dec!
  (dec! n)   -> (set! n (- n 1))
  (dec! n m) -> (set! n (- n m))
  )

(defmacro while
  (while test body ...)
  -> (let $loop ()
       (if test
	   (begin body ... ($loop))
	   #u)))

(defmacro when
  (when test body ...)
  -> (if test (begin body ...)))

(defmacro when-maybe
  (when-maybe var mob
    body ...)
  -> (match mob with
       (maybe:yes var) -> (begin body ...)
       (maybe:no) -> #u
       ))

(defmacro if-maybe
  (if-maybe var mob then else)
  -> (match mob with
       (maybe:yes var) -> then
       (maybe:no) -> else
       ))

(defmacro while-maybe
  (while-maybe var mob
    body ...)
  -> (let $loop ()
       (match mob with
         (maybe:yes var) -> (begin body ... ($loop))
         (maybe:no) -> #u
         )))

;; actually this could probably be done with for-range
;; with this pattern: (for-range i (10 20) ...)
;; XXX doesn't work when <hi> is calculated.
(defmacro for-range*
  (for-range* vname lo hi body ...)
  -> (let $loop ((vname lo)
                 ($stop hi))
       (if (= vname $stop)
           #u
           (begin
             body ...
             ($loop (+ vname 1) $stop)))))

(defmacro for-range
  (for-range vname num body ...)
  -> (for-range* vname 0 num body ...)
  )

(defmacro for-range-rev*
  (for-range-rev* vname lo hi body ...)
  -> (let $loop ((vname (- hi 1))
                 ($stop lo))
       (if (< vname $stop)
           #u
           (begin body ...
                  ($loop (- vname 1) $stop)))))

(defmacro for-range-rev
  (for-range-rev vname num body ...)
  -> (for-range-rev* vname 0 num body ...))

(defmacro for-vector
  (for-vector vname vec body ...)
  -> (let (($v vec)) ;; avoid duplicating <vec> expression.
       (for-range $i (vector-length $v)
	 (let ((vname $v[$i]))
	   body ...))))

(defmacro for-vector-rev
  (for-vector-rev vname vec body ...)
  -> (let (($v vec)) ;; avoid duplicating <vec> expression.
       (for-range-rev $i (vector-length $v)
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

(defmacro pipe
  (pipe a)       -> a
  (pipe a b)     -> (a b)
  (pipe a b ...) -> (a (pipe b ...))
  )
