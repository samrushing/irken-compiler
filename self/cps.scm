;; -*- Mode: Irken -*-

(include "self/nodes.scm")

(datatype tag
  (:bare int)
  (:uobj int)
  )

;; RTL instructions
(datatype insn
  (:return int)                                                 ;; return register
  (:literal literal cont)                                       ;; <value> <k>
  (:litcon int symbol cont)                                     ;; <index> <value> <k>
  (:cexp type type string (list int) cont)                      ;; <sig> <solved-type> <template> <args> <k>
  (:test int insn insn cont)                                    ;; <reg> <then> <else> <k>
  (:testcexp (list int) type string insn insn cont)             ;; <regs> <sig> <template> <then> <else> <k>
  (:jump int int)                                               ;; <reg> <target>
  (:close symbol insn cont)                                     ;; <name> <body> <k>
  (:varref int int cont)                                        ;; <depth> <index> <k>
  (:varset int int int cont)                                    ;; <depth> <index> <reg> <k>
  (:new-env int bool cont)	                                ;; <size> <top?> <k>
  (:alloc tag int cont)                                         ;; <tag> <size> <k>
  (:store int int int int cont)                                 ;; <offset> <arg> <tuple> <i> <k>
  (:invoke (maybe symbol) int int cont)                         ;; <name> <closure> <args> <k>
  (:tail (maybe symbol) int int)                                ;; <name> <closure> <args>
  (:trcall int symbol (list int))                               ;; <depth> <name> <args>
  (:push int cont)                                              ;; <env>
  (:pop int cont)                                               ;; <result>
  (:primop symbol sexp type (list int) cont)                    ;; <name> <params> <args> <k>
  (:move int int cont)                                          ;; <var> <src> <k>
  (:fatbar int insn insn cont)                                  ;; <label> <alt0> <alt1> <k>
  (:fail int int)                                               ;; <label> <npop>
  (:nvcase int symbol (list symbol) (list insn) (maybe insn) cont)      ;; <reg> <dt> <tags> <alts> <ealt> <k>
  (:pvcase int (list symbol) (list int) (list insn) (maybe insn) cont)  ;; <reg> <tags> <arities> <ealt> <k>
  )

;; continuation
;; XXX wonder if this would make more sense as a record?
(datatype cont
  (:k int (list int) insn) ;; <target-register> <free-registers> <code>
  (:nil)
  )

;; we use several kinds of environment 'ribs' during this phase
(datatype cpsenv
  (:nil) 				;; empty
  (:rib (list symbol) cpsenv)		;; variables
  (:reg symbol int cpsenv)		;; variables-in-registers
  (:fat int cpsenv)			;; fatbar context
  )

(define lenv-top?
  (cpsenv:nil) -> #t
  _ -> #f
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

;; perhaps name these cont/xxx could be confusing.
(define k/free
  (cont:k _ free _)   -> free
  (cont:nil) -> (error "k/free"))

(define k/target
  (cont:k target _ _) -> target
  (cont:nil) -> (error "k/target"))

(define add-free-regs
  (cont:k target free k) regs -> (cont:k target (append free regs) k)
  (cont:nil) _		      -> (error "add-free-regs"))

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
	(node:literal lit)		-> (c-literal lit exp.id k)
	(node:sequence)			-> (c-sequence tail? exp.subs lenv k)
	(node:if)			-> (c-conditional tail? exp lenv k)
	(node:function name formals)	-> (c-function name formals exp.id (car exp.subs) lenv k)
	(node:varref name)		-> (c-varref name lenv k)
	(node:varset name)		-> (c-varset name (car exp.subs) lenv k)
	(node:cexp gens sig template)	-> (c-cexp sig template exp lenv k)
	(node:call)			-> (c-call tail? exp lenv k)
	(node:primapp name params)	-> (c-primapp tail? name params exp lenv k)
	(node:nvcase 'nil tags arities) -> (c-pvcase tail? tags arities exp.subs lenv k)
	(node:nvcase dt tags arities)	-> (c-nvcase tail? dt tags exp.subs lenv k)
	(node:fix formals)		-> (c-let-splat tail? formals exp.subs lenv k)
	(node:let formals)		-> (if (safe-for-let-reg exp formals context)
					       (c-let-reg tail? formals exp.subs lenv k)
					       (c-let-splat tail? formals exp.subs lenv k))
	(node:subst _ _)		-> (impossible)
	)
      )

    (define (add-literal lit)
      (let ((index (length context.literals)))
	(PUSH context.literals lit)
	index))

    ;; inlining often causes literals to be copied all over the place.
    ;;   we can detect this because their node id's are the same.  So keep
    ;;   a map from id->litindex so we can reference via litcon.

    (define (get-literal-index lit id)
      (match (tree/member context.literal-ids < id) with
	(maybe:yes v) -> v
	(maybe:no)
	-> (let ((index (add-literal lit)))
	     (set! context.literal-ids
		   (tree/insert context.literal-ids < id index))
	     index)))

    (define (get-symbol-index sym)
      (match (alist/lookup context.symbols sym) with
	(maybe:yes index) -> index
	(maybe:no)
	-> (let ((string-index (add-literal (literal:string (symbol->string sym))))
		 (symbol-index (add-literal (literal:symbol sym))))
	     (alist/push context.symbols sym symbol-index)
	     symbol-index)))

    ;; scan through a literal for symbols and strings, make sure they're recorded as well.
    (define scan-literals
      () -> 0
      (hd . tl) -> (begin
		     (match hd with
		       (literal:symbol sym)    -> (get-symbol-index sym)
		       (literal:string s)      -> (add-literal hd)
		       (literal:cons _ _ args) -> (scan-literals args)
		       (literal:vector args)   -> (scan-literals args)
		       _ -> 0)
		     (scan-literals tl)))

    (define (c-literal lit id k)
      (match lit with
	;; non-immediate literals are 'constructed' and referenced by index.
	(literal:symbol s)	-> (insn:litcon (get-symbol-index s) 'symbol k)
	(literal:string s)	-> (insn:litcon (get-literal-index lit id) 'string k)
	(literal:vector args)	-> (begin
				     (scan-literals args)
				     (insn:litcon (get-literal-index lit id) 'vector k))
	;; problem: any literal without args should be encoded as an
	;;   immediate, and not show up in 'constructed'.  I think it
	;;   can be done like this:
	;; (literal:cons x y ()) -> (insn:literal lit k)
	(literal:cons 'bool s _) -> (insn:literal lit k)
	(literal:cons _ _ args) -> (begin
				     (scan-literals args)
				     (insn:litcon (get-literal-index lit id) 'constructor k))
	;; immediate literals are done 'inline'.
	_ -> (insn:literal lit k)))

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
	(collect-primargs test.subs lenv k finish)))

    (define extend-lenv
      () lenv -> lenv ;; don't extend with an empty rib
      fs lenv -> (cpsenv:rib fs lenv)
      )

    (define (c-function name formals id body lenv k)
      (set-flag! VFLAG-ALLOCATES)
      (PUSH current-funs name)
      (let ((r
	     (insn:close
	      name
	      (compile #t
		       body
		       (extend-lenv formals lenv)
		       (cont '() gen-return)
		       )
	      k)))
	(PUSH context.profile-funs current-funs)
	(pop current-funs)
	r))

    (define search-rib
      name0 _ ()		  -> (maybe:no)
      name0 i (name1 . names) -> (if (eq? name0 name1)
				     (maybe:yes i)
				     (search-rib name0 (+ i 1) names)))

    ;; Note: only 'real' environment ribs increase lexical depth.
    (define lexical-address
      name _ (cpsenv:nil)	       -> (error1 "unbound variable" name)
      name d (cpsenv:rib names lenv)   -> (match (search-rib name 0 names) with
					    (maybe:yes i) -> (if (eq? lenv (cpsenv:nil))
								 (:top d i)
								 (:pair d i))
					    (maybe:no)    -> (lexical-address name (+ d 1) lenv))
      name d (cpsenv:fat _ lenv)       -> (lexical-address name d lenv)
      name d (cpsenv:reg name0 r lenv) -> (if (eq? name name0)
					      (:reg r)
					      (lexical-address name d lenv))
      )

    (define (c-varref name lenv k)
      (match (lexical-address name 0 lenv) with
	(:reg r) -> (insn:move r -1 k)
	(:pair depth index) -> (insn:varref depth index k)
	(:top _ index) -> (insn:varref -1 index k)
	))

    (define (c-varset name exp lenv k)
      (let ((kfun
	     (match (lexical-address name 0 lenv) with
	       (:pair depth index)
	       -> (lambda (reg) (insn:varset depth index reg k))
	       (:top _ index)
	       -> (lambda (reg) (insn:varset -1 index reg k))
	       (:reg index)
	       -> (lambda (reg) (insn:move reg index k))
	       )))
	(compile #f exp lenv (cont (k/free k) kfun))))

    (define (c-primapp tail? name params exp lenv k)
      (let ((args exp.subs))
	(match name with
	  '%fail    -> (c-fail tail? lenv k)
	  '%fatbar  -> (c-fatbar tail? args lenv k)
	  '%dtcon   -> (begin (if (> (length args) 0)
				 (set-flag! VFLAG-ALLOCATES))
			     (c-primargs args name params exp.type lenv k))
	  '%vcon    -> (c-vcon params args lenv k)
	  '%rextend -> (c-record-literal exp lenv k)
	  '%raccess -> (let ((arg0 (nth args 0))
			     (sig (get-record-sig-sexp arg0.type)))
			 (c-primargs args '%record-get
				     (sexp params sig) ;; (field sig)
				     exp.type
				     lenv k))
	  '%rset    -> (let ((arg0 (nth args 0))
			     (sig (get-record-sig-sexp arg0.type)))
			 (c-primargs args '%record-set
				     (sexp params sig) ;; (field sig)
				     exp.type
				     lenv k))
	  '%cset    -> (let ((val (nth args 2))
			     (tval val.type)
			     (buffer (nth args 0)))
			 (match buffer.type with
			   (type:pred 'buffer (tbase) _)
			   -> (let ((cast-type (arrow tbase (LIST tval))))
				;; we need both types in order to cast correctly
				(c-primargs args name params cast-type lenv k))
			   _ -> (impossible)))
	  ;; do-nothing prim used to verify exception types
	  '%exn-raise  -> (compile tail? (first args) lenv k)
	  ;; note: discards first argument...
	  '%exn-handle -> (compile tail? (second args) lenv k)
	  _ -> (c-primargs args name params exp.type lenv k))))
   
    (define (c-cexp sig template exp lenv k)
      ;;(print-string (format "c-cexp: sig = " (type-repr sig) " solved type = " (type-repr exp.type) "\n"))
      (collect-primargs exp.subs lenv k
			(lambda (regs)
			  (insn:cexp sig exp.type template regs k))))

    ;; collect-primargs is used by primops, simple-conditional, and tr-call.
    ;;   in order to avoid the needless consumption of registers, we re-arrange
    ;;   the eval order of these args - by placing the complex args first.

    (define (collect-primargs args lenv k ck)
      (let ((args0 (map-range
		       i (length args)
		       (:pair (nth args i) i)))
	    (args1 (sort (lambda (a b)
			   (match a b with
			     (:pair arg_a _) (:pair arg_b _)
			     -> (> arg_a.size arg_b.size)))
			 args0))
	    (perm (map pair->second args1))
	    (args2 (map pair->first args1)))
	(collect-primargs* args2 '() perm lenv k ck)))

    (define (collect-primargs* args regs perm lenv k ck)
      (match args with
	()        -> (let ((regs (reverse regs))
			   ;; undo the permutation of the arg regs
			   (perm-regs
			    (map-range
				i (length perm)
				(nth regs (index-eq i perm)))))
		       (ck perm-regs))
	(hd . tl) -> (compile #f hd lenv
			      (cont (append (k/free k) regs)
				    (lambda (reg) (collect-primargs* tl (cons reg regs) perm lenv k ck))))
	))

    (define (c-primargs args op parm type lenv k)
      (collect-primargs args lenv k
			(lambda (regs) (insn:primop op parm type regs k))))

    (define (safe-for-let-reg exp names context)
      (and (node-get-flag exp NFLAG-LEAF)
	   (< (length names) 5)
	   (not (some?
		 (lambda (name)
		   (vars-get-flag context name VFLAG-ESCAPES))
		 names))))

    (define (safe-for-tr-call exp fun)
      (match fun with
	(node:varref name)
	-> (and (node-get-flag exp NFLAG-RECURSIVE)
		(not (vars-get-flag context (car current-funs) VFLAG-ESCAPES)))
	_ -> #f))

    (define (c-trcall depth name args lenv k)
      ;; NOTE: this means tail-calls do not have a guaranteed argument eval order!
      (collect-primargs args lenv k
			(lambda (regs) (insn:trcall depth name regs))))

    (define (c-call tail? exp lenv k)
      (match exp.subs with
	(fun . args)
	-> (if (and tail? (safe-for-tr-call exp fun.t))
	       (let ((name (varref->name fun.t)))
		 (match (lexical-address name 0 lenv) with
		   (:reg _) -> (error "c-call function in register?")
		   (:pair depth _) -> (c-trcall depth name args lenv k)
		   (:top depth _) -> (c-trcall depth name args lenv k)
		   ))
	       (let ((gen-invoke (if tail? gen-tail gen-invoke))
		     (name (match fun.t with
			     (node:varref name)
			     -> (if (vars-get-flag context name VFLAG-FUNCTION)
				    (maybe:yes name)
				    (maybe:no))
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
	() -> (insn:new-env 0 (lenv-top? lenv) k)
	_  -> (let ((nargs (length args)))
		(insn:new-env
		 nargs
		 (lenv-top? lenv)
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

    (define (c-let-reg tail? formals subs lenv k)
      (define (loop names inits lenv regs)
	(if (= 0 (length names))
	    ;; note: the last 'init' is the body
	    ;; build a new version of the continuation with <regs> listed as free regs.
	    (compile tail? (car inits) lenv (add-free-regs k regs))
	    (compile #f
		     (car inits)
		     lenv
		     (cont
		      (append regs (k/free k))
		      (lambda (reg)
			(loop (cdr names)
			      (cdr inits)
			      (cpsenv:reg (car names) reg lenv)
			      (list:cons reg regs)))))))
      (loop formals subs lenv '()))

    (define (c-let-splat tail? formals subs lenv k)
      (let ((rsubs (reverse subs)) ;; subs = (init0 init1 ... body)
	    (body (car rsubs))
	    (inits (reverse (cdr rsubs)))
	    (nargs (length formals))
	    (free (k/free k))
	    (k-body (dead free
			  (compile tail? body (extend-lenv formals lenv)
				   (cont (k/free k) (lambda (reg) (insn:pop reg k)))))))
	(set-flag! VFLAG-ALLOCATES)
	(insn:new-env
	 nargs
	 (lenv-top? lenv)
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
		       (ealt1
			(if (not (= (dt.get-nalts) (length alts)))
			    (match eclause.t with
			      (node:primapp '%match-error _)
			      -> (error1 "incomplete match" alt-formals)
			      ;; incomplete match with ealt
			      _ -> (maybe:yes (compile tail? eclause lenv jump-k)))
			    ;; complete match, no ealt
			    (maybe:no))))
		   (insn:nvcase test-reg dtname alt-formals alts ealt1 k)))
	       (compile #f value lenv (cont free finish))))))

    (define (c-pvcase tail? alt-formals arities subs lenv k)
      (let ((free (k/free k)))
	;; pvcase subs = <value>, <else-clause>, <alt0>, ...
	 (let ((value (nth subs 0))
	       (eclause (nth subs 1))
	       (alts (cdr (cdr subs))))
	   (define (finish test-reg)
	     (let ((jump-k (cont free (lambda (reg) (insn:jump reg (k/target k)))))
		   (alts (map (lambda (alt) (compile tail? alt lenv jump-k)) alts))
		   (else? (match eclause.t with (node:primapp '%match-error _) -> #f _ -> #t))
		   (ealt (if else? (maybe:yes (compile tail? eclause lenv jump-k)) (maybe:no))))
	       (insn:pvcase test-reg alt-formals arities alts ealt k)))
	   (compile #f value lenv (cont free finish)))))

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
	  (cpsenv:nil)		-> (error "%fail without fatbar?")
	  (cpsenv:rib _ lenv)	-> (loop (+ depth 1) lenv)
	  (cpsenv:reg _ _ lenv) -> (loop depth lenv)
	  (cpsenv:fat label _)	-> (insn:fail label depth))))

    (define (c-vcon params args lenv k)
      (match params with
	(sexp:list ((sexp:symbol label) _))
	-> (let ((tag (alist/get context.variant-labels label "unknown variant label?"))
		 (free (k/free k))
		 (nargs (length args)))
	     (if (> nargs 0)
		 (set-flag! VFLAG-ALLOCATES))
	     ;; in python this was implemented as a %make-tuple primitive, which used
	     ;;   compile-primargs rather than using compile-store-rands.  The generated
	     ;;   code isn't much different, and may put less pressure on the registers.
	     (if (> nargs 0)
		 (insn:alloc (tag:uobj tag)
			     nargs
			     (cont free
				   (lambda (reg)
				     (compile-store-args
				      0 0 args reg
				      (list:cons reg free)
				      lenv k))))
		 (insn:alloc (tag:uobj tag) 0 k)))
	_ -> (error1 "bad %vcon params" params)))

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
	  -> (let ((fields0 (sort 
			    (lambda (a b)
			      (match a b with
				(:pair f0 _) (:pair f1 _)
				-> (symbol<? f0 f1)))
			    fields))
		   (sig (map pair->first fields0))
		   (args (map pair->second fields0))
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

  (define (mprint-insn minsn d)
    (match minsn with
      (maybe:yes insn) -> (print-insn insn d)
      (maybe:no) -> #u))

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
      (cont:nil)
      -> (begin
	   (newline)
	   (indent d)
	   (print-string "- ")
	   (print-info))
      ))

  (define (ps x) (print x) (print-string " "))
  (define (ps2 x) (print-string x) (print-string " "))
  (match insn with
    (insn:return target)	    -> (begin (newline) (indent d) (ps2 "- ret") (print target))
    (insn:tail n c a)		    -> (print-line (lambda () (ps2 "tail") (ps n) (ps c) (ps a)) (cont:nil))
    (insn:trcall d n args)	    -> (print-line (lambda () (ps2 "trcall") (ps d) (ps n) (ps args)) (cont:nil))
    (insn:literal lit k)	    -> (print-line (lambda () (ps2 "lit") (ps2 (literal->string lit))) k)
    (insn:litcon i kind k)          -> (print-line (lambda () (ps2 "litcon") (ps i) (ps kind)) k)
    (insn:cexp sig typ tem args k)  -> (print-line (lambda () (ps2 "cexp") (ps2 (type-repr sig)) (ps2 (type-repr typ)) (ps tem) (ps args)) k)
    (insn:test reg then else k)	    -> (print-line (lambda () (ps2 "test") (print reg) (print-insn then (+ d 1)) (print-insn else (+ d 1))) k)
    (insn:jump reg trg)		    -> (print-line (lambda () (ps2 "jmp") (print trg)) (cont:nil))
    (insn:close name body k)	    -> (print-line (lambda () (ps2 "close") (print name) (print-insn body (+ d 1))) k)
    (insn:varref d i k)		    -> (print-line (lambda () (ps2 "ref") (ps d) (ps i)) k)
    (insn:varset d i v k)	    -> (print-line (lambda () (ps2 "set") (ps d) (ps i) (ps v)) k)
    (insn:store o a t i k)	    -> (print-line (lambda () (ps2 "stor") (ps o) (ps a) (ps t) (ps i)) k)
    (insn:invoke n c a k)	    -> (print-line (lambda () (ps2 "invoke") (ps n) (ps c) (ps a)) k)
    (insn:new-env n top? k)	    -> (print-line (lambda () (ps2 "env") (ps n) (ps top?)) k)
    (insn:alloc tag size k)         -> (print-line (lambda () (ps2 "alloc") (ps tag) (ps size)) k)
    (insn:push r k)                 -> (print-line (lambda () (ps2 "push") (ps r)) k)
    (insn:pop r k)                  -> (print-line (lambda () (ps2 "pop") (ps r)) k)
    (insn:primop name p t args k)   -> (print-line (lambda () (ps2 "primop") (ps name) (ps2 (repr p)) (ps2 (type-repr t)) (ps args)) k)
    (insn:move var src k)           -> (print-line (lambda () (ps2 "move") (ps var) (ps src)) k)
    (insn:fatbar lab k0 k1 k)       -> (print-line (lambda () (ps2 "fatbar") (ps lab) (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1))) k)
    (insn:fail lab npop)            -> (print-line (lambda () (ps2 "fail") (ps lab) (ps npop)) (cont:nil))
    (insn:testcexp r s t k0 k1 k)
    -> (print-line
	(lambda ()
	  (ps2 "testcexp") (ps r) (ps2 (type-repr s)) (ps t)
	  (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1)))
	k)
    (insn:nvcase tr dt labels alts ealt k)
    -> (print-line
	(lambda () (ps2 "nvcase")
		(ps tr) (ps dt) (ps labels)
		(for-each (lambda (insn) (print-insn insn (+ d 1))) alts)
		(mprint-insn ealt (+ d 1)))
	k)
    (insn:pvcase tr labels arities alts ealt k)
    -> (print-line
	(lambda () (ps2 "pvcase")
		(ps tr) (ps labels) (ps arities)
		(for-each (lambda (insn) (print-insn insn (+ d 1))) alts)
		(mprint-insn ealt (+ d 1)))
	k)
    ))

(define (walk-insns p insn)

  (define (mwalk minsn d)
    (match minsn with
      (maybe:yes insn) -> (walk insn d)
      (maybe:no) -> #u))

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
						       (mwalk ealt (+ d 1)) k)
	     (insn:pvcase _ _ _ alts ealt k) -> (begin (for-each (lambda (x) (walk x (+ d 1))) alts)
						       (mwalk ealt (+ d 1)) k)
	     ;; ... the rest just have one continuation
	     (insn:literal _ k)	     -> k
	     (insn:litcon _ _ k)     -> k
	     (insn:cexp _ _ _ _ k)   -> k
	     (insn:varref _ _ k)     -> k
	     (insn:varset _ _ _ k)   -> k
	     (insn:store _ _ _ _ k)  -> k
	     (insn:invoke _ _ _ k)   -> k
	     (insn:new-env _ _ k)    -> k
	     (insn:alloc _ _ k)	     -> k
	     (insn:push _ k)	     -> k
	     (insn:pop _ k)	     -> k
	     (insn:primop _ _ _ _ k) -> k
	     (insn:move _ _ k)	     -> k
	     )))
      (match k with
	(cont:k target free insn) -> (walk insn d)
	(cont:nil) -> #u)))
  (walk insn 0))

(define (make-insn-generator insn)
  (make-generator
   (lambda (consumer)
     (walk-insns
      (lambda (insn depth)
	(consumer (maybe:yes (:pair insn depth))))
      insn)
     (let loop ()
       (consumer (maybe:no))
       (loop)))))

;; could be for any generator?
(defmacro for-insns
  (for-insns vname insns body ...)
  -> (let (($ig (make-insn-generator insns)))
       (let loop ()
	 (match ($ig) with
	   (maybe:yes vname)
	   -> (begin body ... (loop))
	   (maybe:no) -> #u))))
