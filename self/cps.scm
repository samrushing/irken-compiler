;; -*- Mode: Irken -*-

(include "self/nodes.scm")

;; RTL instructions
(datatype insn
  (:return int)				;; return register
  (:literal literal cont)		;; <value> <k>
  (:cexp type string (list int) cont)	;; <sig> <template> <args> <k>
  (:test int insn insn cont)		;; <reg> <then> <else> <k>
  (:jump int cont)			;; <reg> <k>
  (:close symbol insn cont)		;; <name> <body> <k>
  (:varref int int cont)		;; <depth> <index> <k>
  (:varset int int int cont)		;; <depth> <index> <reg> <k>
  (:new-env int cont)			;; <size> <k>
  (:store int int int int cont)		;; <offset> <arg> <tuple> <i> <k>
  (:invoke int int cont)		;; <closure> <args> <k>
  (:tail int int cont)			;; <closure> <args> <k>
  (:push int cont)			;; <env>
  (:pop int cont)			;; <result>
  (:primop symbol sexp (list int) cont)	;; <name> <params> <args> <k>
  (:move int int cont)			;; <var> <src> <k>
  (:fatbar int insn insn cont)		;; <label> <alt0> <alt1> <k>
  (:fail int int cont)			;; <label> <npop> <k>
  (:nvcase int symbol (list symbol) (list insn) insn cont)
  )

;; continuation
;; XXX wonder if this would make more sense as a record?
(datatype cont
  (:k int (list int) insn) ;; <target-registers> <free-registers> <code>
  )

(datatype cpsenv
  (:nil)
  (:rib (list symbol) cpsenv)
  (:reg int cpsenv)
  (:fat int cpsenv)
  )

(define (make-register-allocator)
  (let ((max-reg -1))
    (define (allocate free)
      (let loop ((i 0))
	(if (member? i free =)
	    (loop (+ i 1))
	    (begin (set! max-reg (max i max-reg)) i))))
    (define (get-max) max-reg)
    {alloc=allocate get-max=get-max}
    ))

(define k/free   (cont:k _ free _)   -> free)
(define k/target (cont:k target _ _) -> target)

(define (compile exp context)

  (define (cont free generator)
    (let ((reg (context.regalloc.alloc free)))
      (cont:k reg free (generator reg))))

  (define (dead free k)
    (cont:k -1 free k))

  (define (compile tail? exp lenv k)

    ;; override continuation when in tail position
    (if tail?
	(set! k (cont (k/free k) gen-return)))
  
    (match exp.t with
      (node:literal lit)	    -> (c-literal lit k)
      (node:sequence)		    -> (c-sequence tail? exp.subs lenv k)
      (node:if)			    -> (c-conditional tail? exp lenv k)
      (node:function name formals)  -> (c-function name formals (car exp.subs) lenv k)
      (node:varref name)	    -> (c-varref name lenv k)
      (node:varset name)	    -> (c-varset name (car exp.subs) lenv k)
      (node:cexp gens sig template) -> (c-cexp sig template exp.subs lenv k)
      (node:call)		    -> (c-call tail? exp lenv k)
      (node:fix formals)	    -> (c-let-splat tail? formals exp.subs lenv k)
      (node:let formals)            -> (c-let-splat tail? formals exp.subs lenv k)
      (node:primapp name params)    -> (c-primapp tail? name params exp.subs lenv k)
      (node:nvcase dt alts)         -> (c-nvcase tail? dt alts exp.subs lenv k)
      _ -> (begin (pp-node exp 0) (error1 "NYI" exp))
      )
    )

  (define (c-literal val k) (insn:literal val k))

  (define (c-sequence tail? nodes lenv k)
    (match nodes with
      ()		 -> (error "empty sequence?")
      (exp)	 -> (compile tail? exp lenv k)
      (exp . exps) -> (compile #f exp lenv (dead (k/free k) (c-sequence tail? exps lenv k)))
      ))

  ;; XXX consider redoing with fatbar?
  (define (c-conditional tail? exp lenv k)
    (match exp.subs with
      (test then else)
      -> (compile
	  #f test lenv
	  (cont (k/free k)
		(lambda (reg)
		  (insn:test
		   reg
		   (compile tail? then lenv (cont (k/free k) (lambda (reg) (insn:jump reg k))))
		   (compile tail? else lenv (cont (k/free k) (lambda (reg) (insn:jump reg k))))
		   k))
		))
      _ -> (error1 "c-conditional" exp)
      ))

  (define extend-lenv
    () lenv -> lenv ;; don't extend with an empty rib
    fs lenv -> (cpsenv:rib fs lenv)
    )

  (define (c-function name formals body lenv k)
    (insn:close
     name
     (compile #t
	      body
	      (extend-lenv formals lenv)
	      (cont '() gen-return))
     k))

  (define search-rib
    name0 _ ()		  -> (maybe:no)
    name0 i (name1 . names) -> (if (eq? name0 name1)
				   (maybe:yes i)
				   (search-rib name0 (+ i 1) names)))

  (define lexical-address
    name _ (cpsenv:nil) -> (error1 "unbound variable" name)
    name d (cpsenv:rib names lenv) -> (match (search-rib name 0 names) with
					(maybe:yes i) -> (:pair d i)
					(maybe:no)    -> (lexical-address name (+ d 1) lenv))
    name d (cpsenv:fat _ lenv) -> (lexical-address name d lenv)
    name d (cpsenv:reg r lenv) -> (:reg r)
    )

  (define (c-varref name lenv k)
    (match (lexical-address name 0 lenv) with
      (:reg r) -> (insn:move r -1 k)
      (:pair depth index) -> (insn:varref depth index k)
      ))

  (define (c-varset name exp lenv k)
    (let ((kfun
	   (match (lexical-address name 0 lenv) with
	     (:pair depth index)
	     -> (lambda (reg) (insn:varset depth index reg k))
	     (:reg index)
	     -> (lambda (reg) (insn:move reg index k)))))
      (compile #f exp lenv (cont (k/free k) kfun))))

  (define (c-primapp tail? name params args lenv k)
    (match name with
      '%fail   -> (c-fail tail? lenv k)
      '%fatbar -> (c-fatbar tail? args lenv k)
      _ -> (c-primargs args name params lenv k)))
   
  (define (c-cexp sig template args lenv k)
    (collect-primargs args '() lenv k
		      (lambda (regs)
			(insn:cexp sig template regs k))))

  (define (collect-primargs args regs lenv k ck)
    (match args with
      ()        -> (ck (reverse regs))
      (hd . tl) -> (compile #f hd lenv
			    (cont (append (k/free k) regs)
				  (lambda (reg) (collect-primargs tl (cons reg regs) lenv k ck))))
      ))

  (define (c-primargs args op parm lenv k)
    (collect-primargs args '() lenv k
		      (lambda (regs) (insn:primop op parm regs k))))

  (define (c-call tail? exp lenv k)
    (let ((gen-invoke (if tail? gen-tail gen-invoke)))
      (match exp.subs with
	(fun . args)
	-> (letrec ((make-call
		     (lambda (args-reg)
		       (compile #f fun lenv (cont (cons args-reg (k/free k))
						  (lambda (closure-reg) (gen-invoke closure-reg args-reg k)))))))
	     (if (> (length args) 0)
		 (compile-args args lenv (cont (k/free k) make-call))
		 (make-call -1)))
	() -> (error "c-call: no function?")
	)))

  (define (compile-args args lenv k)
    (match args with
      () -> (insn:new-env 0 k)
      _  -> (let ((nargs (length args)))
	      (insn:new-env
	       nargs
	       (cont (k/free k)
		     (lambda (tuple-reg)
		       (compile-store-args 0 1 args tuple-reg
					   (cons tuple-reg (k/free k)) lenv k)))))
      ))

  (define (compile-store-args i offset args tuple-reg free-regs lenv k)
    (compile
     #f (car args) lenv
     (cont free-regs
	   (lambda (arg-reg)
	     (insn:store
	      offset arg-reg tuple-reg i
	      (if (null? (cdr args)) ;; was this the last argument?
		  k
		  (dead
		   free-regs
		   (compile-store-args (+ i 1) offset (cdr args) tuple-reg free-regs lenv k))))))))

  (define (c-let-splat tail? formals subs lenv k)
    (let ((rsubs (reverse subs)) ;; subs = (init0 init1 ... body)
	  (body (car rsubs))
	  (inits (reverse (cdr rsubs)))
	  (nargs (length formals))
	  (free (k/free k))
	  (k-body (dead free
			(compile tail? body (extend-lenv formals lenv)
				 (cont (k/free k) (lambda (reg) (insn:pop reg k)))))))
      (insn:new-env
       nargs
       (cont free
	     (lambda (tuple-reg)
	       (insn:push
		tuple-reg
		(dead free
		      (compile-store-args 0 1 inits tuple-reg
					  (list:cons tuple-reg free)
					  (extend-lenv formals lenv)
					  k-body))))))))

  (define (c-nvcase tail? dtname alt-formals subs lenv k)
    (let ((free (k/free k)))
    ;; nvcase subs = <value>, <else-clause>, <alt0>, ...
      (match (alist/lookup context.datatypes dtname) with
	(maybe:no) -> (error1 "no such datatype" dtname)
	(maybe:yes dt)
	-> (let ((value (nth subs 0))
		 (eclause (nth subs 1))
		 (alts (cdr (cdr subs))))
	     (define (finish test-reg)
	       (let ((jump-k (cont free (lambda (reg) (insn:jump reg k))))
		     (alts (map (lambda (alt) (compile tail? alt lenv jump-k)) alts))
		     (ealt (compile tail? eclause lenv jump-k)))
		 (if (not (= (dt.get-nalts) (length alts)))
		     (match eclause.t with
		       (node:primapp '%match-error _)
		       -> (error1 "incomplete match" alt-formals)
		       _ -> #u))
		 (insn:nvcase test-reg dtname alt-formals alts ealt k)))
	     (compile #f value lenv (cont free finish))))))

  (define fatbar-counter (make-counter 0))

  (define (c-fatbar tail? subs lenv k)
    (let ((label (fatbar-counter.inc))
	  (lenv0 (cpsenv:fat label lenv)))
      (insn:fatbar label
		   (compile tail? (nth subs 0) lenv0 (cont (k/free k) (lambda (reg) (insn:jump reg k))))
		   (compile tail? (nth subs 1) lenv  (cont (k/free k) (lambda (reg) (insn:jump reg k))))
		   k)))

  (define (c-fail tail? lenv k)
    ;; lookup the closest surrounding fatbar label
    (let loop ((depth 0)
	       (lenv lenv))
      (match lenv with
	(cpsenv:nil)	   -> (error "%fail without fatbar?")
	(cpsenv:rib _ lenv)  -> (loop (+ depth 1) lenv)
	(cpsenv:reg _ lenv)  -> (loop depth lenv)
	(cpsenv:fat label _) -> (insn:fail depth label k))))

  (define (gen-return reg)
    (insn:return reg))
  (define (gen-invoke closure-reg args-reg k)
    (insn:invoke closure-reg args-reg k))
  (define (gen-tail closure-reg args-reg k)
    (insn:tail closure-reg args-reg k))

  (compile #t exp (cpsenv:nil) (cont '() gen-return))
  )

;;; XXX redo this with the new format macro
(define (print-insn insn d)
  (define (print-line print-info k)
    (match k with
      (cont:k target free k0)
      -> (begin
	   (newline)
	   (indent d)
	   (if (= target -1) (print-string "-") (print target))
	   ;;(print-string " ") (print free)
	   (print-string " ")
	   (print-info)
	   (print-insn k0 d)
	   )))
  (define (ps x) (print x) (print-string " "))
  (define (ps2 x) (print-string x) (print-string " "))
  (match insn with
    (insn:return target)	    -> (begin (newline) (indent d) (ps2 "- ret") (print target))
    (insn:literal lit k)	    -> (print-line (lambda () (ps2 "lit") (ps2 (literal->string lit))) k)
    (insn:cexp sig template args k) -> (print-line (lambda () (ps2 "cexp") (ps2 (type-repr sig)) (ps template) (ps args)) k)
    (insn:test reg then else k)	    -> (print-line (lambda () (ps2 "test") (print reg) (print-insn then (+ d 1)) (print-insn else (+ d 1))) k)
    (insn:jump reg k)		    -> (print-line (lambda () (ps2 "jmp") (print reg)) k)
    (insn:close name body k)	    -> (print-line (lambda () (ps2 "close") (print name) (print-insn body (+ d 1))) k)
    (insn:varref d i k)		    -> (print-line (lambda () (ps2 "ref") (ps d) (ps i)) k)
    (insn:varset d i v k)	    -> (print-line (lambda () (ps2 "set") (ps d) (ps i) (ps v)) k)
    (insn:store o a t i k)	    -> (print-line (lambda () (ps2 "stor") (ps o) (ps a) (ps t) (ps i)) k)
    (insn:invoke c a k)		    -> (print-line (lambda () (ps2 "invoke") (ps c) (ps a)) k)
    (insn:tail c a k)		    -> (print-line (lambda () (ps2 "tail") (ps c) (ps a)) k)
    (insn:new-env n k)		    -> (print-line (lambda () (ps2 "env") (ps n)) k)
    (insn:push r k)                 -> (print-line (lambda () (ps2 "push") (ps r)) k)
    (insn:pop r k)                  -> (print-line (lambda () (ps2 "pop") (ps r)) k)
    (insn:primop name p args k)     -> (print-line (lambda () (ps2 "primop") (ps name) (ps2 (repr p)) (ps args)) k)
    (insn:move var src k)           -> (print-line (lambda () (ps2 "move") (ps var) (ps src)) k)
    (insn:fatbar lab k0 k1 k)       -> (print-line (lambda () (ps2 "fatbar") (ps lab) (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1))) k)
    (insn:fail lab npop k)          -> (print-line (lambda () (ps2 "fail") (ps lab) (ps npop)) k)
    (insn:nvcase tr dt formals alts ealt k)
    -> (print-line (lambda () (ps2 "nvcase")
			   (ps tr) (ps dt) (ps formals)
			   (for-each (lambda (insn) (print-insn insn (+ d 1))) alts)
			   (print-insn ealt (+ d 1)))
		   k)
    ))
