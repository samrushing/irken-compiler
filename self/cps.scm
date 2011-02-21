;; -*- Mode: Irken -*-

(include "self/nodes.scm")

(datatype tag
  (:bare int)
  (:uobj int)
  )

;; RTL instructions
(datatype insn
  (:return int)						;; return register
  (:literal literal cont)				;; <value> <k>
  (:cexp type string (list int) cont)			;; <sig> <template> <args> <k>
  (:test int insn insn cont)				;; <reg> <then> <else> <k>
  (:testcexp (list int) type string insn insn cont)	;; <regs> <sig> <template> <then> <else> <k>
  (:jump int int)					;; <reg> <target>
  (:close symbol insn cont)				;; <name> <body> <k>
  (:varref int int cont)				;; <depth> <index> <k>
  (:varset int int int cont)				;; <depth> <index> <reg> <k>
  (:new-env int cont)					;; <size> <k>
  (:alloc tag int cont)					;; <tag> <size> <k>
  (:store int int int int cont)				;; <offset> <arg> <tuple> <i> <k>
  (:invoke (maybe symbol) int int cont)			;; <name> <closure> <args> <k>
  (:tail (maybe symbol) int int)			;; <name> <closure> <args>
  (:trcall int symbol (list int))			;; <depth> <name> <args>
  (:push int cont)					;; <env>
  (:pop int cont)					;; <result>
  (:primop symbol sexp (list int) cont)			;; <name> <params> <args> <k>
  (:move int int cont)					;; <var> <src> <k>
  (:fatbar int insn insn cont)				;; <label> <alt0> <alt1> <k>
  (:fail int int)					;; <label> <npop>
  (:nvcase int symbol (list symbol) (list insn) insn cont)
  )

;; continuation
;; XXX wonder if this would make more sense as a record?
(datatype cont
  (:k int (list int) insn) ;; <target-register> <free-registers> <code>
  (:nil)
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

(define k/free
  (cont:k _ free _)   -> free
  (cont:nil) -> (error "k/free"))

(define k/target
  (cont:k target _ _) -> target
  (cont:nil) -> (error "k/target")
  )

(define (compile exp context)

  (let ((current-funs '(top)))

    (define (set-flag! flag)
      (vars-set-flag! context (car current-funs) flag))

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
	(node:literal lit)	      -> (c-literal lit k)
	(node:sequence)		      -> (c-sequence tail? exp.subs lenv k)
	(node:if)		      -> (c-conditional tail? exp lenv k)
	(node:function name formals)  -> (c-function name formals (car exp.subs) lenv k)
	(node:varref name)	      -> (c-varref name lenv k)
	(node:varset name)	      -> (c-varset name (car exp.subs) lenv k)
	(node:cexp gens sig template) -> (c-cexp sig template exp.subs lenv k)
	(node:call)		      -> (c-call tail? exp lenv k)
	(node:fix formals)	      -> (c-let-splat tail? formals exp.subs lenv k)
	(node:let formals)            -> (c-let-splat tail? formals exp.subs lenv k)
	(node:primapp name params)    -> (c-primapp tail? name params exp lenv k)
	(node:nvcase dt alts)         -> (c-nvcase tail? dt alts exp.subs lenv k)
	_ -> (begin (pp-node exp 0) (error1 "NYI" exp))
	)
      )

    (define (c-literal val k) (insn:literal val k))

    (define (c-sequence tail? nodes lenv k)
      (match nodes with
	()	     -> (error "empty sequence?")
	(exp)	     -> (compile tail? exp lenv k)
	(exp . exps) -> (compile #f exp lenv (dead (k/free k) (c-sequence tail? exps lenv k)))
	))

    ;; XXX consider redoing with fatbar?
    (define (c-conditional tail? exp lenv k)
      (let ((target (k/target k))
	    (free (k/free k)))
	(match exp.subs with
	  (test then else)
	  -> (match test.t with
	       (node:cexp _ sig template)
	       -> (c-simple-conditional tail? test then else sig template lenv k)
	       _ -> (compile
		     #f test lenv
		     (cont free
			   (lambda (reg)
			     (insn:test
			      reg
			      (compile tail? then lenv (cont free (lambda (reg) (insn:jump reg target))))
			      (compile tail? else lenv (cont free (lambda (reg) (insn:jump reg target))))
			      k))
			   )))
	  _ -> (error1 "c-conditional" exp)
	  )))

    (define (c-simple-conditional tail? test then else sig template lenv k)
      (let ((free (k/free k))
	    (target (k/target k)))
	(define (finish regs)
	  ;; <regs> <sig> <template> <then> <else> <k>
	  (insn:testcexp
	   regs sig template
	   (compile tail? then lenv (cont free (lambda (reg) (insn:jump reg target))))
	   (compile tail? else lenv (cont free (lambda (reg) (insn:jump reg target))))
	   k))
	(collect-primargs test.subs '() lenv k finish)))

    (define extend-lenv
      () lenv -> lenv ;; don't extend with an empty rib
      fs lenv -> (cpsenv:rib fs lenv)
      )

    (define (c-function name formals body lenv k)
      (set-flag! VFLAG-ALLOCATES)
      (PUSH current-funs name)
      (let ((r
	     (insn:close
	      name
	      (compile #t
		       body
		       (extend-lenv formals lenv)
		       (cont '() gen-return))
	      k)))
	(pop current-funs)
	r))

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

    (define (c-primapp tail? name params exp lenv k)
      (let ((args exp.subs))
	(match name with
	  '%fail   -> (c-fail tail? lenv k)
	  '%fatbar -> (c-fatbar tail? args lenv k)
	  '%dtcon  -> (begin (if (> (length args) 0)
				 (set-flag! VFLAG-ALLOCATES))
			     (c-primargs args name params lenv k))
	  '%rextend -> (c-record-literal exp lenv k)
	  '%raccess -> (let ((arg0 (nth args 0))
			     (sig (get-record-sig-sexp arg0.type)))
			 (c-primargs args '%record-get
				     (sexp params sig) ;; (field sig)
				     lenv k))
	  _ -> (c-primargs args name params lenv k))))
   
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

    (define (safe-for-tr-call exp fun)
      (match fun with
	(node:varref name)
	-> (and (node-get-flag exp NFLAG-RECURSIVE)
		(not (vars-get-flag context (car current-funs) VFLAG-ESCAPES)))
	_ -> #f))

    (define (c-trcall depth name args lenv k)
      (collect-primargs args '() lenv k
			(lambda (regs) (insn:trcall depth name regs))))

    (define (c-call tail? exp lenv k)
      (match exp.subs with
	(fun . args)
	-> (if (and tail? (safe-for-tr-call exp fun.t))
	       (let ((name (varref->name fun.t)))
		 (match (lexical-address name 0 lenv) with
		   (:reg _) -> (error "c-call function in register?")
		   (:pair depth index) -> (c-trcall depth name args lenv k)
		   ))
	       (let ((gen-invoke (if tail? gen-tail gen-invoke))
		     (name (match fun.t with
			     (node:varref name) -> (maybe:yes name)
			     _ -> (maybe:no))))
		 (define (make-call args-reg)
		   (compile #f fun lenv (cont (cons args-reg (k/free k))
					      (lambda (closure-reg) (gen-invoke name closure-reg args-reg k)))))
		 (if (> (length args) 0)
		     (compile-args args lenv (cont (k/free k) make-call))
		     (make-call -1))))
	() -> (error "c-call: no function?")
	))

    (define (compile-args args lenv k)
      (set-flag! VFLAG-ALLOCATES)
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
		 (let ((jump-k (cont free (lambda (reg) (insn:jump reg (k/target k)))))
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
	    (lenv0 (cpsenv:fat label lenv))
	    (free (k/free k))
	    (target (k/target k)))
	(insn:fatbar label
		     (compile tail? (nth subs 0) lenv0 (cont free (lambda (reg) (insn:jump reg target))))
		     (compile tail? (nth subs 1) lenv  (cont free (lambda (reg) (insn:jump reg target))))
		     k)))

    (define (c-fail tail? lenv k)
      ;; lookup the closest surrounding fatbar label
      (let loop ((depth 0)
		 (lenv lenv))
	(match lenv with
	  (cpsenv:nil)	   -> (error "%fail without fatbar?")
	  (cpsenv:rib _ lenv)  -> (loop (+ depth 1) lenv)
	  (cpsenv:reg _ lenv)  -> (loop depth lenv)
	  (cpsenv:fat label _) -> (insn:fail depth label))))

    (define (c-record-literal exp lenv k)
      (let loop ((exp exp)
		 (fields '()))
        ;; (%rextend field0 (%rextend field1 (%rmake) ...)) => {field0=x field1=y}
	(match exp.t with
	  (node:primapp '%rextend (sexp:symbol field)) ;; add another field
	  -> (match exp.subs with
	       (exp0 val)
	       -> (loop exp0 (list:cons (:pair field val) fields))
	       _ -> (error1 "malformed %rextend" exp))
	  (node:primapp '%rmake _) ;; done - put the names in canonical order
	  -> (let ((field0 (sort 
			    (lambda (a b)
			      (match a b with
				(:pair f0 _) (:pair f1 _)
				-> (symbol<? f0 f1)))
			    fields))
		   (sig (map pair-first fields))
		   (args (map pair-second fields))
		   (tag (get-record-tag sig))
		   (free (k/free k)))
	       (insn:alloc (tag:uobj tag)
			   (length args)
			   (cont free
				 (lambda (reg)
				   (compile-store-args
				    0 0 args reg
				    (list:cons reg free)
				    lenv k)))))
	  _ -> (c-record-extension fields exp lenv k))))
		       
    (define (c-record-extension fields exp lenv k)
      (error "c-record-extension: NYI"))

    (define (record-label-tag label)
      (let loop ((l context.labels))
	(match l with
	  ((:pair key val) . tl)
	  -> (if (eq? key label)
		 #u
		 (loop tl))
	  () -> (let ((index (length context.labels)))
		  (PUSH context.labels (:pair label index)))
	  )))

    (define (sig=? sig0 sig1)
      (and (= (length sig0) (length sig1))
	   (every2? eq? sig0 sig1)))

    (define (get-record-tag sig)
      (let loop ((l context.records))
	(match l with
	  ((:pair key val) . tl)
	  -> (if (sig=? key sig)
		 val
		 (loop tl))
	  ;; create a new entry
	  () -> (let ((index (length context.records)))
		  (for-each record-label-tag sig)
		  (PUSH context.records (:pair sig index))
		  index)
	  )))

    (define (gen-return reg)
      (insn:return reg))
    (define (gen-invoke name closure-reg args-reg k)
      (set-flag! VFLAG-ALLOCATES)
      (insn:invoke name closure-reg args-reg k))
    (define (gen-tail name closure-reg args-reg k)
      (insn:tail name closure-reg args-reg))

    (compile #t exp (cpsenv:nil) (cont '() gen-return))
    ))

;;; XXX redo this with the new format macro - this function is horrible.
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
	   )
      _ -> #u
      ))
  (define (ps x) (print x) (print-string " "))
  (define (ps2 x) (print-string x) (print-string " "))
  (match insn with
    (insn:return target)	    -> (begin (newline) (indent d) (ps2 "- ret") (print target))
    (insn:tail n c a)		    -> (print-line (lambda () (ps2 "tail") (ps n) (ps c) (ps a)) (cont:nil))
    (insn:trcall d n args)	    -> (print-line (lambda () (ps2 "trcall") (ps d) (ps n) (ps args)) (cont:nil))
    (insn:literal lit k)	    -> (print-line (lambda () (ps2 "lit") (ps2 (literal->string lit))) k)
    (insn:cexp sig template args k) -> (print-line (lambda () (ps2 "cexp") (ps2 (type-repr sig)) (ps template) (ps args)) k)
    (insn:test reg then else k)	    -> (print-line (lambda () (ps2 "test") (print reg) (print-insn then (+ d 1)) (print-insn else (+ d 1))) k)
    (insn:testcexp r s t k0 k1 k)   -> (print-line (lambda () (ps2 "testcexp") (ps r) (ps2 (type-repr s)) (ps t) (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1))) k)
    (insn:jump reg trg)		    -> (print-line (lambda () (ps2 "jmp") (print trg)) (cont:nil))
    (insn:close name body k)	    -> (print-line (lambda () (ps2 "close") (print name) (print-insn body (+ d 1))) k)
    (insn:varref d i k)		    -> (print-line (lambda () (ps2 "ref") (ps d) (ps i)) k)
    (insn:varset d i v k)	    -> (print-line (lambda () (ps2 "set") (ps d) (ps i) (ps v)) k)
    (insn:store o a t i k)	    -> (print-line (lambda () (ps2 "stor") (ps o) (ps a) (ps t) (ps i)) k)
    (insn:invoke n c a k)	    -> (print-line (lambda () (ps2 "invoke") (ps n) (ps c) (ps a)) k)
    (insn:new-env n k)		    -> (print-line (lambda () (ps2 "env") (ps n)) k)
    (insn:alloc tag size k)         -> (print-line (lambda () (ps2 "alloc") (ps tag) (ps size)) k)
    (insn:push r k)                 -> (print-line (lambda () (ps2 "push") (ps r)) k)
    (insn:pop r k)                  -> (print-line (lambda () (ps2 "pop") (ps r)) k)
    (insn:primop name p args k)     -> (print-line (lambda () (ps2 "primop") (ps name) (ps2 (repr p)) (ps args)) k)
    (insn:move var src k)           -> (print-line (lambda () (ps2 "move") (ps var) (ps src)) k)
    (insn:fatbar lab k0 k1 k)       -> (print-line (lambda () (ps2 "fatbar") (ps lab) (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1))) k)
    (insn:fail lab npop)            -> (print-line (lambda () (ps2 "fail") (ps lab) (ps npop)) (cont:nil))
    (insn:nvcase tr dt formals alts ealt k)
    -> (print-line (lambda () (ps2 "nvcase")
			   (ps tr) (ps dt) (ps formals)
			   (for-each (lambda (insn) (print-insn insn (+ d 1))) alts)
			   (print-insn ealt (+ d 1)))
		   k)
    ))

(define (walk-insns p insn)
  (define (walk insn d)
    (p insn d)
    (let ((k
	   (match insn with
	     ;; no continuation
	     (insn:return target) -> (cont:nil)
	     (insn:tail _ _ _)	  -> (cont:nil)
	     (insn:trcall _ _ _)  -> (cont:nil)
	     (insn:jump _ _)	  -> (cont:nil)
	     (insn:fail _ _)      -> (cont:nil)
	     ;; these insns contain sub-bodies...
	     (insn:fatbar _ k0 k1 k)	     -> (begin (walk k0 (+ d 1)) (walk k1 (+ d 1)) k)
	     (insn:close _ body k)	     -> (begin (walk body (+ d 1)) k)
	     (insn:test _ then else k)	     -> (begin (walk then (+ d 1)) (walk else (+ d 1)) k)
	     (insn:testcexp _ _ _ k0 k1 k)   -> (begin (walk k0 (+ d 1)) (walk k1 (+ d 1)) k)
	     (insn:nvcase _ _ _ alts ealt k) -> (begin (for-each (lambda (x) (walk x (+ d 1))) alts)
						       (walk ealt (+ d 1)) k)
	     ;; ... the rest just have one continuation
	     (insn:literal _ k)	    -> k
	     (insn:cexp _ _ _ k)    -> k
	     (insn:varref _ _ k)    -> k
	     (insn:varset _ _ _ k)  -> k
	     (insn:store _ _ _ _ k) -> k
	     (insn:invoke _ _ _ k)  -> k
	     (insn:new-env _ k)	    -> k
	     (insn:alloc _ _ k)	    -> k
	     (insn:push _ k)        -> k
	     (insn:pop _ k)         -> k
	     (insn:primop _ _ _ k)  -> k
	     (insn:move _ _ k)      -> k
	     )))
      (match k with
	(cont:k target free insn) -> (walk insn d)
	(cont:nil) -> #u)))
  (walk insn 0))

(define done-insn (:pair (insn:return -1) -1))

(define (make-insn-generator insn)
  (make-generator
   (lambda (consumer)
     (walk-insns (lambda (insn depth) (consumer (:pair insn depth))) insn)
     (let loop ()
       (consumer done-insn)
       (loop)))))
   
(define (iterate-insns insn)
  (let ((g (make-insn-generator insn)))
    (for-range
	i 10
	(match (g) with
	  (:pair insn depth)
	  -> (begin (indent depth)
		    (printn (%%cexp ('a -> int) "get_case(%0)" insn)))))
    ))
     
     
      