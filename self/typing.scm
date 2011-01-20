;; -*- Mode: Irken -*-

(include "self/nodes.scm")

;; The 'subst', or type substitution/map, is not an actual data structure,
;;   but rather lives in the '.val' attribute of the set of all type variables.
;; To 'apply the subst', simply follow the path through each type variable
;;   until you get to something that's not a tvar.

;; let's ignore moo for now.  I think we could maybe do it by adding a bool to the tvar record.

(define (apply-subst-to-type t)
  (let recur ((t t))
    (match t with
      (type:base _) -> t
      (type:pred name args) -> (type:pred name (map recur args))
      (type:tvar tvr)
      -> (let ((t0 (lookup-subst t tvr)))
	   (match t0 with
	     ;; search ends in another type variable
	     (type:tvar _) -> t0
	     ;; search ends with a type, which we also need to expand
	     _ -> (begin (set! tvr.pending #t) ;; need to think about macros for this!
			 (let ((t1 (recur t0)))
			   (set! tvr.pending #f)
			   t1)))))))

(define (lookup-subst t0 tvr0)
  (let loop ((t0 t0))
    (match t0 with
      (type:tvar {val=(maybe:no)}) -> t0
      (type:tvar {val=(maybe:yes t1)})
      -> (begin
	   ;; path compression
	   (set! tvr0.val (maybe:yes t1))
	   (loop t1))
      _ -> t0)))

(define (extend-subst tvr t)
  (match tvr.val with
    (maybe:no)    -> (set! tvr.val (maybe:yes t))
    (maybe:yes _) -> (error1 "type var already bound!" tvr)))

(define (unify exp t0 t1)

  (define (type-error t0 t1)
    (newline)
    (pp-node exp 0)
    (print-string
     (format "\nType Error:\n\t" (type-repr t0) "\n\t" (type-repr t1) "\n"))
    (error "type error")
    )

  (define U*
    (hd0 . tl0) (hd1 . tl1) -> (begin (U hd0 hd1) (U* tl0 tl1))
    () () -> #u
    _ _	  -> (impossible) ;; we've already verified the lengths match
    )

  (define (U t0 t1)
    (let ((t0 (apply-subst-to-type t0))
	  (t1 (apply-subst-to-type t1)))
      (match t0 t1 with
	(type:base b0) (type:base b1) -> (if (eq? b0 b1) #u (type-error t0 t1))
	(type:tvar tvr) _ -> (extend-subst tvr t1)
	_ (type:tvar tvr) -> (extend-subst tvr t0)
	(type:pred n0 args0) (type:pred n1 args1)
	-> (if (eq? n0 n1)
	       (if (= (length args0) (length args1))
		   (U* args0 args1)
		   (type-error t0 t1))
	       (type-error t0 t1))
	_ _ -> (type-error t0 t1)
	)))
  (U t0 t1)
  )

(define (instantiate-type-scheme type)
  (let ((tvars (alist/make)))
    (let walk ((t type))
      (match t with
	(type:base _)	      -> t
	(type:pred name args) -> (type:pred name (map walk args))
	(type:tvar tvr)	      -> (match (alist/lookup tvars tvr.serial) with
				   (maybe:yes tv) -> tv
				   (maybe:no)	  -> (let ((new (new-tvar)))
						       (alist/push tvars tvr.serial new)
						       new))))))

(define (type-of exp tenv)
  (match exp.t with
    (node:literal lit)	      -> (literal->type lit)
    (node:cexp sig _)	      -> (type-of-cexp sig exp tenv)
    (node:if)		      -> (type-of-conditional exp tenv)
    (node:sequence)	      -> (type-of-sequence exp.subs tenv)
    (node:function _ formals) -> (type-of-function formals (car exp.subs) tenv)
    (node:varref name)        -> (type-of-varref name tenv)
    _ -> (error1 "NYI" exp)))

(define literal->type
  (literal:string _) -> string-type
  (literal:int _)    -> int-type
  (literal:char _)   -> char-type
  (literal:bool _)   -> bool-type
  (literal:undef)    -> undefined-type
  )

(define (type-of-cexp sig exp tenv)
  (let ((sig (instantiate-type-scheme sig)))
    (match sig with
      (type:pred 'arrow pargs)
      -> (if (not (= (- (length pargs) 1) (length exp.subs)))
	     (error1 "wrong number of args to cexp" exp)
	     (match pargs with
	       () -> (error1 "malformed arrow type" sig)
	       (result-type . parg-types)
	       -> (let ((arg-types (map (lambda (x) (type-of x tenv)) exp.subs)))
		    (map2 (lambda (a b) (unify exp a b)) parg-types arg-types)
		    result-type
		    )))
      _ -> sig)))

(define (type-of-conditional exp tenv)
  (match (map (lambda (x) (type-of x tenv)) exp.subs) with
    (t0 t1 t2)
    -> (begin
	 (unify exp t0 bool-type)
	 (unify exp t1 t2)
	 t2)
    _ -> (error1 "malformed conditional" exp)
    ))

(define (type-of-sequence exps tenv)
  (let loop ((l exps))
    (match l with
      ()	-> (impossible)
      (one)	-> (type-of one tenv)
      (hd . tl) -> (begin
		     ;; ignore all but the last
		     (type-of hd tenv)
		     (loop tl)))))

;; XXX TODO - handle user-supplied types
(define (optional-type formal tenv)
  (new-tvar))

(define (type-of-function formals body tenv)
  (let ((arg-types '()))
    (for-each
     (lambda (formal)
       (let ((type (optional-type formal tenv)))
	 (PUSH arg-types type)
	 (alist/push tenv formal (:pair #f type))))
     formals)
    (type:pred 'arrow (list:cons (type-of body tenv) arg-types))))

(define (apply-tenv name tenv)
  (match (alist/lookup tenv name) with
    (maybe:no) -> (error1 "unbound variable" name)
    (maybe:yes (:pair generalize? t))
    -> (if generalize? (instantiate-type-scheme t) t)))

(define (type-of-varref name tenv)
  (apply-tenv name tenv))

(define (test-typing)
  (let ((context (make-context))
	(transform (transformer context))
	;;(exp0 (sexp:list (read-string "(%%cexp (int int -> int) \"%0+%1\" 3 #\\a)")))
	;;(exp0 (sexp:list (read-string "(begin #\\A (if #t 3 4))")))
	(exp0 (sexp:list (read-string "(lambda (x) x)")))
	(exp1 (transform exp0))
	(node0 (walk exp1)))
    (pp-node node0 0)
    (newline)
    (printn (type-repr (type-of node0 (alist/make))))
    ))

(test-typing)
