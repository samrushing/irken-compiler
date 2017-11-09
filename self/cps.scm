;; -*- Mode: Irken -*-

(include "self/nodes.scm")

;; XXX tag:bare appears to be unused, get rid of this datatype.
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
  (:ffi type type symbol (list int) cont)                       ;; <sig> <solved-type> <name> <args> <k>
  (:test int int insn insn cont)                                ;; <reg> <jump-number> <then> <else> <k>
  (:testcexp (list int) type string int insn insn cont)         ;; <regs> <sig> <template> <jump-number> <then> <else> <k>
  (:jump int int int (list int))                                ;; <reg> <target> <jump-number> <free>
  (:close symbol int insn cont)                                 ;; <name> <nfree> <body> <k>
  (:varref int int cont)                                        ;; <depth> <index> <k>
  (:varset int int int cont)                                    ;; <depth> <index> <reg> <k>
  (:new-env int bool (list type) cont)	                        ;; <size> <top?> <k>
  (:alloc tag int cont)                                         ;; <tag> <size> <k>
  (:store int int int int cont)                                 ;; <offset> <arg> <tuple> <i> <k>
  (:invoke (maybe symbol) int int cont)                         ;; <name> <closure> <args> <k>
  (:tail (maybe symbol) int int)                                ;; <name> <closure> <args>
  (:trcall int symbol (list int))                               ;; <depth> <name> <args>
  (:push int cont)                                              ;; <env>
  (:pop int cont)                                               ;; <result>
  (:primop symbol sexp type (list int) cont)                    ;; <name> <params> <args> <k>
  (:move int int cont)                                          ;; <var> <src> <k>
  (:fatbar int int insn insn cont)                              ;; <label> <jump-num> <alt0> <alt1> <k>
  (:fail int int (list int))                                    ;; <label> <npop> <free>
  (:nvcase int symbol (list symbol) int (list insn) (maybe insn) cont)      ;; <reg> <dt> <tags> <jump-num> <alts> <ealt> <k>
  (:pvcase int (list symbol) (list int) int (list insn) (maybe insn) cont)  ;; <reg> <tags> <arities> <jump-num> <alts> <ealt> <k>
  )

;; continuation
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

(define get-register-variables
  acc (cpsenv:nil)            -> acc
  acc (cpsenv:reg _ reg next) -> (get-register-variables (list:cons reg acc) next)
  acc (cpsenv:rib _ next)     -> (get-register-variables acc next)
  acc (cpsenv:fat _ next)     -> (get-register-variables acc next)
  )

(define lenv-top?
  (cpsenv:nil) -> #t
  _ -> #f
  )

(define (make-register-allocator)
  (match the-context.options.backend with
    (backend:bytecode)
    -> (let ((max-reg -1))
         (define (allocate free)
           (let loop ((i 0))
             (if (member? i free =)
                 (loop (+ i 1))
                 (begin (set! max-reg (max i max-reg)) i))))
         (define (get-max) max-reg)
         {alloc=allocate get-max=get-max}
         )
    _
    -> (let ((max-reg -1))
         (define (allocate free)
           (set! max-reg (+ max-reg 1))
           max-reg)
         (define (get-max) max-reg)
         {alloc=allocate get-max=get-max}
         )
    ))

(define (add-to-set reg set)
  (if (not (member? reg set =))
      (list:cons reg set)
      set))

(define (merge-sets a b)
  (let ((r b))
    (for-list ai a
      (if (not (member? ai b =))
          (PUSH r ai)))
    r))

;; perhaps name these cont/xxx could be confusing.
(define k/free
  (cont:k _ free _)   -> free
  (cont:nil) -> (list:nil))

(define k/target
  (cont:k target _ _) -> target
  (cont:nil) -> (error "k/target"))

(define k/insn
  (cont:k _ _ insn) -> insn
  (cont:nil) -> (error "k/insn"))

(define add-free-regs
  (cont:k target free k) regs -> (cont:k target (merge-sets free regs) k)
  (cont:nil) _		      -> (error "add-free-regs"))

(define (compile exp)

  (let ((current-funs '(top))
        (regalloc (make-register-allocator)))

    (define (set-flag! flag)
      (vars-set-flag! (car current-funs) flag))

    (define (cont free generator)
      (let ((reg (regalloc.alloc free)))
	(cont:k reg free (generator reg))))

    (define (dead free k)
      (cont:k -1 free k))

    (define (cps-error msg exp0)
      (printf (join "\n" (get-node-context exp (noderec->id exp0) 30)) "\n")
      (printf "node id=" (int (noderec->id exp0)) "\n")
      (error msg))

    (define (compile tail? exp lenv k)

      ;; override continuation when in tail position
      (if tail?
	  (set! k (cont (k/free k) gen-return)))

      (match (noderec->t exp) with
	(node:literal lit)		-> (c-literal lit (noderec->id exp) k)
	(node:sequence)			-> (c-sequence tail? (noderec->subs exp) lenv k)
	(node:if)			-> (c-conditional tail? exp lenv k)
	(node:function name formals)	-> (c-function name formals (noderec->id exp) (car (noderec->subs exp)) lenv k)
	(node:varref name)		-> (c-varref name lenv k)
	(node:varset name)		-> (c-varset name (car (noderec->subs exp)) lenv k)
	(node:cexp gens sig template)	-> (c-cexp sig template exp lenv k)
	(node:ffi gens sig name)	-> (c-ffi sig name exp lenv k)
	(node:call)			-> (c-call tail? exp lenv k)
	(node:primapp name params)	-> (c-primapp tail? name params exp lenv k)
	(node:nvcase 'nil tags arities) -> (c-pvcase tail? tags arities (noderec->subs exp) lenv k)
	(node:nvcase dt tags arities)	-> (c-nvcase tail? dt tags (noderec->subs exp) lenv k)
	(node:fix formals)		-> (c-let-splat tail? formals (noderec->subs exp) lenv k)
	(node:let formals)		-> (if (safe-for-let-reg exp formals)
					       (c-let-reg tail? formals (noderec->subs exp) lenv k)
					       (c-let-splat tail? formals (noderec->subs exp) lenv k))
	(node:subst _ _)		-> (impossible)
	)
      )

    (define (add-literal lit)
      (cmap/add the-context.literals lit)
      )

    ;; inlining often causes literals to be copied all over the place.
    ;;   we can detect this because their node id's are the same.  So keep
    ;;   a map from id->litindex so we can reference via litcon.

    (define (get-literal-index lit id)
      (match (tree/member the-context.literal-ids int-cmp id) with
	(maybe:yes v) -> v
	(maybe:no)
	-> (let ((index (add-literal lit)))
	     (tree/insert! the-context.literal-ids int-cmp id index)
	     index)))

    (define (get-symbol-index sym)
      (match (tree/member the-context.symbols symbol-index-cmp sym) with
        (maybe:yes index) -> index
        (maybe:no)
        -> (let ((string-index (add-literal (literal:string (symbol->string sym))))
        	 (symbol-index (add-literal (literal:symbol sym))))
             (tree/insert! the-context.symbols symbol-index-cmp sym symbol-index)
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
        ;; note: empty vector is an immediate.
        (literal:vector ())     -> (insn:literal lit k)
	(literal:vector args)	-> (begin
				     (scan-literals args)
				     (insn:litcon (get-literal-index lit id) 'vector k))
	;; problem: any literal without args should be encoded as an
	;;   immediate, and not show up in 'constructed'.  I think it
	;;   can be done like this:
	;; (literal:cons x y ()) -> (insn:literal lit k)
	;;(literal:cons 'bool s _) -> (insn:literal lit k)
        (literal:bool _)        -> (insn:literal lit k)
        (literal:cons _ _ ())   -> (insn:literal lit k)
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

    (define jump-counter (make-counter 0))

    ;; XXX consider redoing with fatbar?
    (define (c-conditional tail? exp lenv k)
      (let ((target (k/target k))
	    (free (k/free k))
	    (jump-num (jump-counter.inc)))
	(match (noderec->subs exp) with
	  (test then else)
	  -> (match (noderec->t test) the-context.options.backend with
	       (node:cexp _ sig template) (backend:c)
	       -> (c-simple-conditional tail? test then else sig template lenv k)
	       _ _
	       -> (compile
		   #f test lenv
		   (cont free
			 (lambda (reg)
			   (insn:test
			    reg
			    jump-num
			    (compile tail? then lenv (cont free (lambda (reg) (insn:jump reg target jump-num free))))
			    (compile tail? else lenv (cont free (lambda (reg) (insn:jump reg target jump-num free))))
			    k))
			 )))
	  _ -> (error1 "c-conditional" exp)
	  )))

    (define (c-simple-conditional tail? test then else sig template lenv k)
      (let ((free (k/free k))
	    (target (k/target k))
	    (jump-num (jump-counter.inc)))
	(define (finish regs)
	  ;; <regs> <sig> <template> <then> <else> <k>
	  (insn:testcexp
	   regs sig template
	   jump-num
	   (compile tail? then lenv (cont free (lambda (reg) (insn:jump reg target jump-num free))))
	   (compile tail? else lenv (cont free (lambda (reg) (insn:jump reg target jump-num free))))
	   k))
	(collect-primargs (noderec->subs test) lenv k finish)))

    (define extend-lenv
      () lenv -> lenv ;; don't extend with an empty rib
      fs lenv -> (cpsenv:rib fs lenv)
      )

    (define (c-function name formals id body lenv k)
      (set-flag! VFLAG-ALLOCATES)
      (PUSH current-funs name)
      (let ((regvars (get-register-variables '() lenv))
	    (r
	     (insn:close
	      name
	      (length regvars)
	      (compile #t
		       body
		       (extend-lenv formals lenv)
		       (cont regvars gen-return)
		       )
	      k)))
	(tree/insert! the-context.profile-funs symbol-index-cmp name {index=0 names=current-funs})
	(pop current-funs)
	r))

    (define search-rib
      name0 _ ()              -> (maybe:no)
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
	       -> (lambda (reg) (insn:move index reg k))
	       )))
	(compile #f exp lenv (cont (k/free k) kfun))))

    (define (c-primapp tail? name params exp lenv k)
      (let ((args (noderec->subs exp)))
	(match name with
	  '%fail    -> (c-fail tail? lenv k)
	  '%fatbar  -> (c-fatbar tail? args lenv k)
	  '%dtcon   -> (begin (if (> (length args) 0)
				 (set-flag! VFLAG-ALLOCATES))
			     (c-primargs args name params (noderec->type exp) lenv k))
	  '%vcon    -> (c-vcon params args lenv k)
	  '%rextend -> (c-record-literal exp lenv k)
	  '%raccess -> (let ((arg0 (nth args 0))
			     (sig (get-record-sig-sexp (noderec->type arg0))))
			 (c-primargs args '%record-get
				     (sexp params sig) ;; (field sig)
				     (noderec->type exp)
				     lenv k))
	  '%rset    -> (let ((arg0 (nth args 0))
			     (sig (get-record-sig-sexp (noderec->type arg0))))
			 (c-primargs args '%record-set
				     (sexp params sig) ;; (field sig)
				     (noderec->type exp)
				     lenv k))
	  '%cset    -> (let ((val (nth args 2))
			     (tval (noderec->type val))
			     (buffer (nth args 0)))
			 (match (noderec->type buffer) with
			   (type:pred 'buffer (tbase) _)
			   -> (let ((cast-type (arrow tbase (LIST tval))))
				;; we need both types in order to cast correctly
				(c-primargs args name params cast-type lenv k))
			   _ -> (impossible)))
	  ;; do-nothing prim used to verify exception types.
	  '%exn-raise  -> (compile tail? (first args) lenv k)
          ;; do-nothing prim to cast C types.
          '%c-cast     -> (compile tail? (first args) lenv k)
	  ;; note: discards first argument...
	  '%exn-handle -> (compile tail? (second args) lenv k)
	  _ -> (c-primargs args name params (noderec->type exp) lenv k))))

    (define (c-cexp sig template exp lenv k)
      ;;(print-string (format "c-cexp: sig = " (type-repr sig) " solved type = " (type-repr (noderec->type exp)) "\n"))
      (collect-primargs (noderec->subs exp) lenv k
			(lambda (regs)
			  (insn:cexp sig (noderec->type exp) template regs k))))

    (define (c-ffi sig name exp lenv k)
      (collect-primargs (noderec->subs exp) lenv k
			(lambda (regs)
			  (insn:ffi sig (noderec->type exp) name regs k))))

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
			     -> (> (noderec->size arg_a) (noderec->size arg_b))))
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
			      (cont (merge-sets (k/free k) regs)
				    (lambda (reg) (collect-primargs* tl (cons reg regs) perm lenv k ck))))
	))

    (define (c-primargs args op parm type lenv k)
      (collect-primargs args lenv k
			(lambda (regs) (insn:primop op parm type regs k))))

    (define (safe-for-let-reg exp names)
      (and (not the-context.options.noletreg)
	   (node-get-flag exp NFLAG-LEAF)
	   (< (length names) 5)
	   (not (some?
		 (lambda (name)
		   (vars-get-flag name VFLAG-FREEREF))
		 names))))

    (define (safe-for-tr-call exp fun)
      (match fun with
	(node:varref name)
	-> (and (node-get-flag exp NFLAG-RECURSIVE)
		(eq? name (car current-funs))
		(not (vars-get-flag (car current-funs) VFLAG-ESCAPES))
		)
	_ -> #f))

    (define (c-trcall depth name args lenv k)
      ;; NOTE: this means tail-calls do not have a guaranteed argument eval order!
      (collect-primargs args lenv k
			(lambda (regs) (insn:trcall depth name regs))))

    (define (c-call tail? exp lenv k)
      (match (noderec->subs exp) with
	(fun . args)
	-> (if (and tail? (safe-for-tr-call exp (noderec->t fun)))
	       (let ((name (varref->name (noderec->t fun))))
		 (match (lexical-address name 0 lenv) with
		   (:reg _) -> (error "c-call function in register?")
		   (:pair depth _) -> (c-trcall depth name args lenv k)
		   (:top depth _) -> (c-trcall depth name args lenv k)
		   ))
	       (let ((gen-invoke (if tail? gen-tail gen-invoke))
		     (name (match (noderec->t fun) with
			     (node:varref name)
			     -> (if (vars-get-flag name VFLAG-FUNCTION)
				    (maybe:yes name)
				    (maybe:no))
			     _ -> (maybe:no)))
		     (free (k/free k)))
		 (define (make-call args-reg)
		   (compile #f fun lenv
			    (cont
			     (if (= args-reg -1)
				 free
				 (cons args-reg free))
			     (lambda (closure-reg)
			       (gen-invoke name closure-reg args-reg k)))))
		 (if (> (length args) 0)
		     (compile-args args lenv (cont (k/free k) make-call))
		     (make-call -1))))
	() -> (error "c-call: no function?")
	))

    (define (compile-args args lenv k)
      (set-flag! VFLAG-ALLOCATES)
      (match args with
	() -> (insn:new-env 0 (lenv-top? lenv) '() k)
	_  -> (let ((nargs (length args))
		    (target (k/target k))
		    (free (k/free k))
		    (types (map (lambda (x) (noderec->type x)) args)))
		(insn:new-env
		 nargs
		 (lenv-top? lenv)
		 types
		 (cont:k target free
			 (compile-store-args 0 1 args target
                                             (add-to-set target free) lenv k))))
	))

    (define (compile-store-args i offset args tuple-reg free-regs lenv k)
      (compile
       #f (car args) lenv
       (cont free-regs
	     (lambda (arg-reg)
	       (insn:store
		offset arg-reg tuple-reg i
		(if (null? (cdr args)) ;; was this the last argument?
		    (cont:k -1 free-regs (k/insn k)) ;; avoid bogus target for <store>
		    (dead
		     free-regs
		     (compile-store-args (+ i 1) offset (cdr args) tuple-reg free-regs lenv k))))))))

    (define (c-let-reg tail? formals subs lenv k)
      (for-each (lambda (f) (vars-set-flag! f VFLAG-REG)) formals)
      (define (loop names inits lenv regs)
	(if (= 0 (length names))
	    ;; note: the last 'init' is the body
	    ;; build a new version of the continuation with <regs> listed as free regs.
            ;;(compile tail? (car inits) lenv (add-free-regs k regs))
            (compile tail? (car inits) lenv
                     (cont (merge-sets regs (k/free k))
                           (lambda (reg)
                             (insn:move reg -1 k))))
	    (compile #f
		     (car inits)
		     lenv
		     (cont
		      (merge-sets regs (k/free k))
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
	    (types (map (lambda (x) (noderec->type x)) inits))
	    (nargs (length formals))
	    (free (k/free k))
	    (k-body (dead free
			  (compile tail? body (extend-lenv formals lenv)
				   (cont (k/free k) (lambda (reg) (insn:pop reg k)))))))
	(set-flag! VFLAG-ALLOCATES)
	(insn:new-env
	 nargs
	 (lenv-top? lenv)
	 types
	 (cont free
	       (lambda (tuple-reg)
		 (insn:push
		  tuple-reg
		  (dead free
			(compile-store-args 0 1 inits tuple-reg
                                            (add-to-set tuple-reg free)
					    (extend-lenv formals lenv)
					    k-body))))))))

    (define (c-nvcase tail? dtname alt-formals subs lenv k)
      (let ((free (k/free k))
	    (jump-num (jump-counter.inc)))
	;; nvcase subs = <value>, <else-clause>, <alt0>, ...
	(match (alist/lookup the-context.datatypes dtname) with
	  (maybe:no) -> (error1 "no such datatype" dtname)
	  (maybe:yes dt)
	  -> (let ((value (nth subs 0))
		   (eclause (nth subs 1))
		   (alts (cdr (cdr subs))))
	       ;; previously we made a single jump continuation and re-used it for each alt.
	       ;;  this worked (in the C backend) because the separate blocks in each case
	       ;;  introduced a new namespace.  with llvm we cannot re-use names at all, so we
	       ;;  generate a fresh continuation/target for each alt.
	       (define (make-jump-k)
		 (cont free (lambda (reg) (insn:jump reg (k/target k) jump-num free))))
	       (define (finish test-reg)
		 (let ((alts (map (lambda (alt) (compile tail? alt lenv (make-jump-k))) alts))
		       (ealt1
			(if (not (= (dt.get-nalts) (length alts)))
			    (match (noderec->t eclause) with
			      (node:primapp '%match-error _) -> (cps-error "incomplete match" value)
			      ;; complete match, no ealt
			      (node:primapp '%complete-match _) -> (maybe:no)
			      ;; incomplete match with ealt
			      _ -> (maybe:yes (compile tail? eclause lenv (make-jump-k))))
			    ;; complete match, no ealt
			    (maybe:no))))
		   (insn:nvcase test-reg dtname alt-formals jump-num alts ealt1 k)))
	       (compile #f value lenv (cont free finish))))))

    (define match-error?
      (node:primapp '%match-error _) -> #t
      _ -> #f)

    (define (c-pvcase tail? alt-formals arities subs lenv k)
      ;; pvcase subs = <value>, <else-clause>, <alt0>, ...
      (let ((free (k/free k))
	    (jump-num (jump-counter.inc))
            (value (nth subs 0))
            (eclause (nth subs 1))
            (alts (cdr (cdr subs)))
            (has-else (not (match-error? (noderec->t eclause)))))
        (define (make-jump-k)
          (cont free (lambda (reg) (insn:jump reg (k/target k) jump-num free))))
        (define (finish test-reg)
          (let ((alts (map (lambda (alt) (compile tail? alt lenv (make-jump-k))) alts))
                (ealt (if has-else
                          (maybe:yes (compile tail? eclause lenv (make-jump-k)))
                          (maybe:no))))
            (insn:pvcase test-reg alt-formals arities jump-num alts ealt k)))
        (if (and (= (length alts) 1) (not has-else))
            (compile tail? (nth alts 0) lenv k)
            (compile #f value lenv (cont free finish)))))

    (define fatbar-counter (make-counter 0))

    (define (c-fatbar tail? subs lenv k)
      (let ((label (fatbar-counter.inc))
	    (lenv0 (cpsenv:fat label lenv))
	    (free (k/free k))
	    (target (k/target k))
	    (jump-num (jump-counter.inc))
            (jump-cont (cont free (lambda (reg) (insn:jump reg target jump-num free)))))
	(insn:fatbar
	 label
	 jump-num
	 (compile tail? (nth subs 0) lenv0 jump-cont)
	 (compile tail? (nth subs 1) lenv  jump-cont)
	 k)))

    (define (c-fail tail? lenv k)
      ;; lookup the closest surrounding fatbar label
      (let loop ((depth 0)
		 (lenv lenv))
	(match lenv with
	  (cpsenv:nil)		-> (error "%fail without fatbar?")
	  (cpsenv:rib _ lenv)	-> (loop (+ depth 1) lenv)
	  (cpsenv:reg _ _ lenv) -> (loop depth lenv)
	  (cpsenv:fat label _)	-> (insn:fail label depth (k/free k)))))

    (define (c-vcon params args lenv k)
      (match params with
	(sexp:list ((sexp:symbol label) _))
	-> (let ((tag (alist/get the-context.variant-labels label "unknown variant label?"))
		 (free (k/free k))
		 (nargs (length args))
		 (target (k/target k)))
	     (if (> nargs 0)
		 (set-flag! VFLAG-ALLOCATES))
	     ;; in python this was implemented as a %make-tuple primitive, which used
	     ;;   compile-primargs rather than using compile-store-rands.  The generated
	     ;;   code isn't much different, and may put less pressure on the registers.
	     (if (> nargs 0)
		 (insn:alloc (tag:uobj tag)
			     nargs
			     (cont:k target free
				     (compile-store-args
				      0 0 args target
                                      (add-to-set target free)
				      lenv k)))
		 (insn:alloc (tag:uobj tag) 0 k)))
	_ -> (error1 "bad %vcon params" params)))

    (define (c-record-literal exp lenv k)
      (let loop ((exp exp)
		 (fields '()))
        ;; (%rextend field0 (%rextend field1 (%rmake) ...)) => {field0=x field1=y}
	(match (noderec->t exp) with
	  (node:primapp '%rextend (sexp:symbol field)) ;; add another field
	  -> (match (noderec->subs exp) with
	       (exp0 val) -> (loop exp0 (list:cons (:pair field val) fields))
	       _	  -> (error1 "malformed %rextend" exp))
	  (node:primapp '%rmake _)
	  -> (let ((fields0 (sort ;; done - put the names in canonical order
			    (lambda (a b)
			      (match a b with
				(:pair f0 _) (:pair f1 _)
				-> (symbol<? f0 f1)))
			    fields))
		   (sig (map pair->first fields0))
		   (args (map pair->second fields0))
		   (tag (get-record-tag sig))
		   (free (k/free k))
		   (target (k/target k))
		   )
	       (insn:alloc (tag:uobj tag)
			   (length args)
			   ;; this is a hack.  the issue is that <k> already holds the target for
			   ;;   the allocation... compile-store-args is broken in that it assigns
			   ;;   a target for a <stor> [rather than a dead cont].
			   (cont:k target free
			           (compile-store-args
			            0 0 args target
                                    (add-to-set target free)
			            lenv k))
                           ))
	  _ -> (c-record-extension fields exp lenv k))))

    (define (c-record-extension fields exp lenv k)
      (error "c-record-extension: NYI"))

    (define (record-label-tag label)
      (let loop ((l the-context.labels))
	(match l with
	  ((:pair key val) . tl)
	  -> (if (eq? key label)
		 #u
		 (loop tl))
	  () -> (let ((index (length the-context.labels)))
		  (PUSH the-context.labels (:pair label index)))
	  )))

    (define (sig=? sig0 sig1)
      (and (= (length sig0) (length sig1))
	   (every2? eq? sig0 sig1)))

    (define (get-record-tag sig)
      (let loop ((l the-context.records))
	(match l with
	  ((:pair key val) . tl)
	  -> (if (sig=? key sig)
		 val
		 (loop tl))
	  ;; create a new entry
	  () -> (let ((index (length the-context.records)))
		  (for-each record-label-tag sig)
		  (PUSH the-context.records (:pair sig index))
		  index)
	  )))

    (define (gen-return reg)
      (insn:return reg))
    (define (gen-invoke name closure-reg args-reg k)
      (set-flag! VFLAG-ALLOCATES)
      (insn:invoke name closure-reg args-reg k))
    (define (gen-tail name closure-reg args-reg k)
      (insn:tail name closure-reg args-reg))

    (compile #t exp (cpsenv:nil) (cont:nil))
    ))

;; this never completes because of an issue with type<? - a certain complex type
;;   (an object type) ends up spinning forever.  may be related to non-canonical
;;   record types.  needs more investigation.

;; XXX update: this is almost certainly a problem with exponential growth
;;   when using a `<` function to do deep compares: each level of comparison
;;   makes *two* calls (to eliminate the equality case).
;;   magic<? may help with this problem, but the only true fix is to change
;;   *everything* to use 3-way comparisons.

(define (collect-all-types root)
  (let ((ng (make-node-generator root))
	(type-map (map-maker magic-cmp)))
    (let loop ()
      (match (ng) with
	(maybe:no) -> #u
	(maybe:yes (:tuple n d))
	-> (begin
	     (match (noderec->t n) with
	       (node:let formals)
	       -> (for-each
		   (lambda (x) (type-map::maybe-add (apply-subst (noderec->type x)) (noderec->id x)))
		   (reverse (cdr (reverse (noderec->subs n)))))
	       _ -> #u)
	     (loop)
	     )))
    (when the-context.options.debugtyping
       (printf "--- type-map ---\n")
       (let ((nitems 0))
	 (type-map::iterate
	  (lambda (k v)
	    (set! nitems (+ 1 nitems))))
	 (printf "#types=" (int nitems) "\n"))
       (type-map::iterate
	(lambda (k v)
	  (printf (lpad 10 (int v)) " " (type-repr k) "\n")))
       )
    type-map
    ))

(define (print-type-tree root)
  (for (n d x) (make-node-generator root)
       (indent d)
       (printf (type-repr (noderec->type n)) "\n")))

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

  ;; XXX note: this print code is a mess because it predates the format macro. TO BE FIXED!
  (define (ps x) (print x) (print-string " "))
  (define (ps2 x) (print-string x) (print-string " "))
  (match insn with
    (insn:return target)	    -> (begin (newline) (indent d) (ps2 "- ret") (print target))
    (insn:tail n c a)		    -> (print-line (lambda () (ps2 "tail") (ps n) (ps c) (ps a)) (cont:nil))
    (insn:trcall d n args)	    -> (print-line (lambda () (ps2 "trcall") (ps d) (ps n) (ps args)) (cont:nil))
    (insn:literal lit k)	    -> (print-line (lambda () (ps2 "lit") (ps2 (literal->string lit))) k)
    (insn:litcon i kind k)          -> (print-line (lambda () (ps2 "litcon") (ps i) (ps kind)) k)
    (insn:cexp sig typ tem args k)  -> (print-line (lambda () (ps2 "cexp") (ps2 (type-repr sig)) (ps2 (type-repr typ)) (ps tem) (ps args)) k)
    (insn:ffi sig typ name args k)  -> (print-line (lambda () (ps2 "ffi") (ps2 (type-repr sig)) (ps2 (type-repr typ)) (ps name) (ps args)) k)
    (insn:test reg jn then else k)  -> (print-line (lambda () (ps2 "test") (ps reg) (ps jn) (print-insn then (+ d 1)) (print-insn else (+ d 1))) k)
    (insn:jump reg trg jn f)	    -> (print-line (lambda () (ps2 "jmp") (ps reg) (ps trg) (ps jn) (ps f)) (cont:nil))
    (insn:close name nreg body k)   -> (print-line (lambda () (ps2 "close") (ps name) (ps nreg) (print-insn body (+ d 1))) k)
    (insn:varref d i k)		    -> (print-line (lambda () (ps2 "ref") (ps d) (ps i)) k)
    (insn:varset d i v k)	    -> (print-line (lambda () (ps2 "set") (ps d) (ps i) (ps v)) k)
    (insn:store o a t i k)	    -> (print-line (lambda () (ps2 "stor") (ps o) (ps a) (ps t) (ps i)) k)
    (insn:invoke n c a k)	    -> (print-line (lambda () (ps2 "invoke") (ps n) (ps c) (ps a)) k)
    (insn:new-env n top? types k)   -> (print-line (lambda () (ps2 "env") (ps n) (ps top?) (ps2 (format "(" (join type-repr " " types) ")"))) k)
    (insn:alloc tag size k)         -> (print-line (lambda () (ps2 "alloc") (ps tag) (ps size)) k)
    (insn:push r k)                 -> (print-line (lambda () (ps2 "push") (ps r)) k)
    (insn:pop r k)                  -> (print-line (lambda () (ps2 "pop") (ps r)) k)
    (insn:primop name p t args k)   -> (print-line (lambda () (ps2 "primop") (ps name) (ps2 (repr p)) (ps2 (type-repr t)) (ps args)) k)
    (insn:move var src k)           -> (print-line (lambda () (ps2 "move") (ps var) (ps src)) k)
    (insn:fatbar lab jn k0 k1 k)    -> (print-line (lambda () (ps2 "fatbar") (ps lab) (ps jn) (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1))) k)
    (insn:fail lab npop f)          -> (print-line (lambda () (ps2 "fail") (ps lab) (ps npop) (ps f)) (cont:nil))
    (insn:testcexp r s t jn k0 k1 k)
    -> (print-line
	(lambda ()
	  (ps2 "testcexp") (ps r) (ps2 (type-repr s)) (ps t) (ps jn) (ps jn)
	  (print-insn k0 (+ d 1)) (print-insn k1 (+ d 1)))
	k)
    (insn:nvcase tr dt labels jn alts ealt k)
    -> (print-line
	(lambda () (ps2 "nvcase")
		(ps tr) (ps dt) (ps labels) (ps jn)
		(for-each (lambda (insn) (print-insn insn (+ d 1))) alts)
		(mprint-insn ealt (+ d 1)))
	k)
    (insn:pvcase tr labels arities jn alts ealt k)
    -> (print-line
	(lambda () (ps2 "pvcase")
		(ps tr) (ps labels) (ps arities) (ps jn)
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
	     (insn:jump _ _ _ _)  -> (cont:nil)
	     (insn:fail _ _ _)    -> (cont:nil)
	     ;; these insns contain sub-bodies...
	     (insn:fatbar _ _ k0 k1 k)	       -> (begin (walk k0 (+ d 1)) (walk k1 (+ d 1)) k)
	     (insn:close _ nreg body k)	       -> (begin (walk body (+ d 1)) k)
	     (insn:test _ _ then else k)       -> (begin (walk then (+ d 1)) (walk else (+ d 1)) k)
	     (insn:testcexp _ _ _ _ k0 k1 k)   -> (begin (walk k0 (+ d 1)) (walk k1 (+ d 1)) k)
	     (insn:nvcase _ _ _ _ alts ealt k) -> (begin (for-each (lambda (x) (walk x (+ d 1))) alts)
							 (mwalk ealt (+ d 1)) k)
	     (insn:pvcase _ _ _ _ alts ealt k) -> (begin (for-each (lambda (x) (walk x (+ d 1))) alts)
							 (mwalk ealt (+ d 1)) k)
	     ;; ... the rest just have one continuation
	     (insn:literal _ k)	     -> k
	     (insn:litcon _ _ k)     -> k
	     (insn:cexp _ _ _ _ k)   -> k
	     (insn:ffi _ _ _ _ k)    -> k
	     (insn:varref _ _ k)     -> k
	     (insn:varset _ _ _ k)   -> k
	     (insn:store _ _ _ _ k)  -> k
	     (insn:invoke _ _ _ k)   -> k
	     (insn:new-env _ _ _ k)  -> k
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
;; XXX replace this with the 'for' macro.
(defmacro for-insns
  (for-insns vname insns body ...)
  -> (let (($ig (make-insn-generator insns)))
       (let loop ()
	 (match ($ig) with
	   (maybe:yes vname)
	   -> (begin body ... (loop))
	   (maybe:no) -> #u))))
