;; -*- Mode: Irken -*-

(require "self/nodes.scm")

;; continuation
(typealias cont {target=int free=(list int) insn=insn})

;; a sentinel/singleton to represent the null continuation.
(define null-cont {target=-1 free=(list:nil) insn=(insn:return -1)})
(define (null-cont? x) (eq? x null-cont))
(define (make-cont target free insn)
  {target=target free=free insn=insn}
  )

;; I'm using a cell to make the `free` sets attached to the `insn:jump`
;;   and `insn:fail` variants mutable.  (for `trim-free-regs`)

(typealias cell {val='a})
(define (make-cell val) {val=val})

;; If I were to do this again I would put `cont` in the front of each of these.

;; RTL instructions
(datatype insn
  ;; return <value-register>
  (:return int)
  ;; literal <value> <k>
  (:literal literal cont)
  ;; litcon <index> <value> <k>
  (:litcon int symbol cont)
  ;; cexp <sig> <solved-type> <template> <args> <k>
  (:cexp type type string (list int) cont)
  ;; ffi <sig> <solved-type> <name> <args> <k>
  (:ffi type type symbol (list int) cont)
  ;; test <reg> <jump-number> <then> <else> <k>
  (:test int int insn insn cont)
  ;; testcexp <regs> <sig> <template> <jump-number> <then> <else> <k>
  (:testcexp (list int) type string int insn insn cont)
  ;; jump <reg> <target> <jump-number> <free>
  (:jump int int int (cell (list int)))
  ;; close <name> <nfree> <body> <k>
  (:close symbol int insn cont)
  ;; varref <depth> <index> <k>
  (:varref int int cont)
  ;; varset <depth> <index> <reg> <k>
  (:varset int int int cont)
  ;; new-env <size> <top?> <k>
  (:new-env int bool (list type) cont)
  ;; alloc <tag> <size> <k>
  (:alloc int int cont)
  ;; store <offset> <arg> <tuple> <i> <k>
  (:store int int int int cont)
  ;; invoke <name> <closure> <args> <k>
  (:invoke (maybe symbol) int int cont)
  ;; tail <name> <closure> <args>
  (:tail (maybe symbol) int int)
  ;; trcall <depth> <name> <args>
  (:trcall int symbol (list int))
  ;; push <env>
  (:push int cont)
  ;; pop <result>
  (:pop int cont)
  ;; primop <name> <params> <args> <k>
  (:primop symbol sexp type (list int) cont)
  ;; move <var> <src> <k>
  (:move int int cont)
  ;; fatbar <label> <jump-num> <alt0> <alt1> <k>
  (:fatbar int int insn insn cont)
  ;; fail <label> <npop> <free>
  (:fail int int (cell (list int)))
  ;; nvcase <reg> <dt> <tags> <jump-num> <alts> <ealt> <k>
  (:nvcase int symbol (list symbol) int (list insn) (maybe insn) cont)
  ;; pvcase <reg> <tags> <arities> <jump-num> <alts> <ealt> <k>
  (:pvcase int (list symbol) (list int) int (list insn) (maybe insn) cont)
  ;; label <label-number> <next>
  (:label int insn)
  )

;; we use several kinds of environment 'ribs' during this phase
(datatype cpsenv
  (:nil) 		      ;; empty
  (:rib (list symbol) cpsenv) ;; variables
  (:reg symbol int cpsenv)    ;; variables-in-registers
  (:fat int cpsenv)	      ;; fatbar context
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

;; Note: this should match NREG in vm/irkvm.c.
(define *max-bytecode-registers* 20)

(define (make-register-allocator)
  (match the-context.options.backend with
    (backend:bytecode)
    -> (let ((max-reg -1))
         (define (allocate free)
           (let loop ((i 0))
             (if (member? i free =)
                 (loop (+ i 1))
                 (if (> i max-reg)
                     (begin (set! max-reg i)
                            (if (> i *max-bytecode-registers*)
                                (begin
                                  (printf "max reg of " (int i) " out of " (int *max-bytecode-registers*) "\n")
                                  (raise (:TooManyRegisters "bytecode: ran out of registers!")))
                                i))
                     i))))
         (define (get-max) max-reg)
         (define (reset) (set! max-reg -1))
         {alloc=allocate get-max=get-max reset=reset}
         )
    _
    -> (let ((max-reg -1))
         (define (allocate free)
           (set! max-reg (+ max-reg 1))
           max-reg)
         (define (get-max) max-reg)
         (define (reset) (set! max-reg -1))
         {alloc=allocate get-max=get-max reset=reset}
         )
    ))

(define (compile exp)

  (let ((current-funs '(top))
        (regalloc (make-register-allocator)))

    (define (add-to-set reg set)
      (if (not (member? reg set =))
          (list:cons reg set)
          set))

    (define (merge-sets a b)
      (let ((r b))
        (for-list ai a
          (if (not (member? ai b =))
              (push! r ai)))
        r))

    (define (set-flag! flag)
      (vars-set-flag! (car current-funs) flag))

    (define (cont free generator)
      (let ((reg (regalloc.alloc free)))
        (make-cont reg free (generator reg))))

    (define (label-cont k num)
      (make-cont k.target k.free (insn:label num k.insn)))

    (define (dead free k)
      (make-cont -1 free k))

    (define (cps-error msg exp0)
      (printf (join "\n" (get-node-context exp (noderec->id exp0) 30)) "\n")
      (printf "node id=" (int (noderec->id exp0)) "\n")
      (error msg))

    (define (compile tail? exp lenv k)

      ;; override continuation when in tail position
      (if tail?
	  (set! k (cont k.free gen-return)))

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
        (literal:record tag vals) -> (begin
                                       (scan-fields vals)
                                       (insn:litcon (get-literal-index lit id) 'record k))
        ;; note: literals without args are immediates.
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
	(exp . exps) -> (compile #f exp lenv (dead k.free (c-sequence tail? exps lenv k)))
	))

    (define jump-counter (make-counter 1))

    ;; XXX consider redoing with fatbar?
    (define (c-conditional tail? exp lenv k)
      (let ((jump-num (jump-counter.inc)))
	(match (noderec->subs exp) with
	  (test then else)
	  -> (match (noderec->t test) the-context.options.backend with
	       (node:cexp _ sig template) (backend:c)
	       -> (c-simple-conditional tail? test then else sig template lenv k)
	       _ _
	       -> (compile
		   #f test lenv
		   (cont k.free
			 (lambda (reg)
			   (insn:test
			    reg
			    jump-num
			    (compile tail? then lenv
                                     (cont k.free
                                           (lambda (reg)
                                             (insn:jump reg k.target jump-num (make-cell k.free)))))
			    (compile tail? else lenv
                                     (cont k.free
                                           (lambda (reg)
                                             (insn:jump reg k.target jump-num (make-cell k.free)))))
			    (label-cont k jump-num)))
			 )))
	  _ -> (error1 "c-conditional" exp)
	  )))

    (define (c-simple-conditional tail? test then else sig template lenv k)
      (let ((jump-num (jump-counter.inc)))
	(define (finish regs)
	  ;; <regs> <sig> <template> <then> <else> <k>
	  (insn:testcexp
	   regs sig template
	   jump-num
	   (compile tail? then lenv
                    (cont k.free (lambda (reg) (insn:jump reg k.target jump-num (make-cell k.free)))))
	   (compile tail? else lenv
                    (cont k.free (lambda (reg) (insn:jump reg k.target jump-num (make-cell k.free)))))
	   (label-cont k jump-num)))
	(collect-primargs (noderec->subs test) lenv k finish)))

    (define extend-lenv
      () lenv -> lenv ;; don't extend with an empty rib
      fs lenv -> (cpsenv:rib fs lenv)
      )

    (define (c-function name formals id body lenv k)
      (set-flag! VFLAG-ALLOCATES)
      (push! current-funs name)
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
	(pop! current-funs)
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
	(:reg r)            -> (insn:move r -1 k)
	(:pair depth index) -> (insn:varref depth index k)
	(:top _ index)      -> (insn:varref -1 index k)
	))

    (define (c-varset name exp lenv k)
      (let ((kfun
	     (match (lexical-address name 0 lenv) with
	       (:pair depth index) -> (lambda (reg) (insn:varset depth index reg k))
	       (:top _ index)      -> (lambda (reg) (insn:varset -1 index reg k))
	       (:reg index)        -> (lambda (reg) (insn:move index reg k))
	       )))
	(compile #f exp lenv (cont k.free kfun))))

    (define (get-cref-type arg)
      (match (noderec->type arg) with
        (type:pred 'cref (t1) _) -> t1
        _                        -> (impossible)
        ))

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
          ;; hack: the back end needs to know the type of the lval.
	  '%c-get-int -> (c-primargs args name params (get-cref-type (nth args 0)) lenv k)
	  '%c-set-int -> (c-primargs args name params (get-cref-type (nth args 0)) lenv k)
	  '%c-get-ptr -> (c-primargs args name params (get-cref-type (nth args 0)) lenv k)
	  '%c-set-ptr -> (c-primargs args name params (get-cref-type (nth args 0)) lenv k)
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
    ;;
    ;; NOTE: this is no longer done.  Because function calls are often inlined,
    ;;   it becomes impossible to predict the evaluation order of arguments.
    ;;   XXX remove the code here meant to handle the permuation of args.

    (define (collect-primargs args lenv k ck)
      (let ((args0 (map-range
		       i (length args)
		       (:pair (nth args i) i)))
	    ;; (args1 (sort (lambda (a b)
	    ;;     	   (match a b with
	    ;;     	     (:pair arg_a _) (:pair arg_b _)
	    ;;     	     -> (> (noderec->size arg_a) (noderec->size arg_b))))
	    ;;     	 args0))
            (args1 args0) ;; do not rearrange
	    (perm (map pair->second args1))
	    (args2 (map pair->first args1)))
	(collect-primargs* args2 '() perm lenv k ck)))

    (define (collect-primargs* args regs perm lenv k ck)
      (match args with
	()
        -> (let ((regs (reverse regs))
                 ;; undo the permutation of the arg regs
                 (perm-regs
                  (map-range
                      i (length perm)
                      (nth regs (index-eq i perm)))))
             (ck perm-regs))
	(hd . tl)
        -> (compile #f hd lenv
                    (cont (merge-sets k.free regs)
                          (lambda (reg)
                            (collect-primargs* tl (cons reg regs) perm lenv k ck))))
	))

    (define (c-primargs args op parm type lenv k)
      (collect-primargs args lenv k
			(lambda (regs) (insn:primop op parm type regs k))))

    (define (safe-for-let-reg exp names)
      (and (not the-context.options.noletreg)
	   (node-get-flag exp NFLAG-LEAF)
	   (< (length names) 15)
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
		     (mname (match (noderec->t fun) with
			     (node:varref name)
			     -> (if (vars-get-flag name VFLAG-FUNCTION)
				    (maybe:yes name)
				    (maybe:no))
			     _ -> (maybe:no))))
		 (define (make-call args-reg)
		   (compile #f fun lenv
			    (cont
			     (if (= args-reg -1)
				 k.free
				 (cons args-reg k.free))
			     (lambda (closure-reg)
			       (gen-invoke mname closure-reg args-reg k)))))
		 (if (> (length args) 0)
		     (compile-args args lenv (cont k.free make-call))
		     (make-call -1))))
	() -> (error "c-call: no function?")
	))

    (define (compile-args args lenv k)
      (set-flag! VFLAG-ALLOCATES)
      (match args with
	() -> (insn:new-env 0 (lenv-top? lenv) '() k)
	_  -> (let ((nargs (length args))
		    (types (map (lambda (x) (noderec->type x)) args)))
		(insn:new-env
		 nargs
		 (lenv-top? lenv)
		 types
                 (make-cont
                  k.target k.free
                  (compile-store-args
                   0 1 args k.target
                   (add-to-set k.target k.free) lenv k)))
                )))

    (define (compile-store-args i offset args tuple-reg free-regs lenv k)
      (compile
       #f (car args) lenv
       (cont free-regs
	     (lambda (arg-reg)
	       (insn:store
		offset arg-reg tuple-reg i
		(if (null? (cdr args)) ;; was this the last argument?
                    (dead free-regs k.insn) ;; avoid bogus target for <store>
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
                     (cont (merge-sets regs k.free)
                           (lambda (reg)
                             (insn:move reg -1 k))))
	    (compile #f
		     (car inits)
		     lenv
		     (cont
		      (merge-sets regs k.free)
		      (lambda (reg)
			(loop (cdr names)
			      (cdr inits)
			      (cpsenv:reg (car names) reg lenv)
			      (list:cons reg regs)))))))
      (loop formals subs lenv '()))

    (define (c-let-splat tail? formals subs lenv k)
      (let ((body (last subs)) ;; subs = (init0 init1 ... body)
	    (inits (butlast subs))
	    (types (map noderec->type inits))
	    (nargs (length formals))
	    (k-body (dead k.free
			  (compile tail? body (extend-lenv formals lenv)
				   (cont k.free (lambda (reg) (insn:pop reg k)))))))
	(set-flag! VFLAG-ALLOCATES)
	(insn:new-env
	 nargs
	 (lenv-top? lenv)
	 types
	 (cont k.free
	       (lambda (tuple-reg)
		 (insn:push
		  tuple-reg
		  (dead k.free
			(compile-store-args 0 1 inits tuple-reg
                                            (add-to-set tuple-reg k.free)
					    (extend-lenv formals lenv)
					    k-body))))))))

    (define (c-nvcase tail? dtname alt-formals subs lenv k)
      (let ((jump-num (jump-counter.inc)))
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
		 (cont k.free (lambda (reg) (insn:jump reg k.target jump-num (make-cell k.free)))))
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
		   (insn:nvcase test-reg dtname alt-formals jump-num alts ealt1 (label-cont k jump-num))))
	       (compile #f value lenv (cont k.free finish))))))

    (define match-error?
      (node:primapp '%match-error _) -> #t
      _ -> #f)

    (define (c-pvcase tail? alt-formals arities subs lenv k)
      ;; pvcase subs = <value>, <else-clause>, <alt0>, ...
      (let ((jump-num (jump-counter.inc))
            (value (nth subs 0))
            (eclause (nth subs 1))
            (alts (cdr (cdr subs)))
            (has-else (not (match-error? (noderec->t eclause)))))
        (define (make-jump-k)
          (cont k.free (lambda (reg) (insn:jump reg k.target jump-num (make-cell k.free)))))
        (define (finish test-reg)
          (let ((alts (map (lambda (alt) (compile tail? alt lenv (make-jump-k))) alts))
                (ealt (if has-else
                          (maybe:yes (compile tail? eclause lenv (make-jump-k)))
                          (maybe:no))))
            (insn:pvcase test-reg alt-formals arities jump-num alts ealt (label-cont k jump-num))))
        (if (and (= (length alts) 1) (not has-else))
            (compile tail? (nth alts 0) lenv k)
            (compile #f value lenv (cont k.free finish)))))

    (define (c-fatbar tail? subs lenv k)
      (let ((label (jump-counter.inc))
	    (lenv0 (cpsenv:fat label lenv))
	    (jump-num (jump-counter.inc))
            (jump-cont (cont k.free (lambda (reg) (insn:jump reg k.target jump-num (make-cell k.free)))))
            (ktest (compile tail? (nth subs 0) lenv0 jump-cont))
            (kfail (insn:label label (compile tail? (nth subs 1) lenv  jump-cont))))
	(insn:fatbar label jump-num ktest kfail (label-cont k jump-num))))

    (define (c-fail tail? lenv k)
      ;; lookup the closest surrounding fatbar label
      (let loop ((depth 0)
		 (lenv lenv))
	(match lenv with
	  (cpsenv:nil)		-> (error "%fail without fatbar?")
	  (cpsenv:rib _ lenv)	-> (loop (+ depth 1) lenv)
	  (cpsenv:reg _ _ lenv) -> (loop depth lenv)
	  (cpsenv:fat label _)	-> (insn:fail label depth (make-cell k.free)))))

    (define (c-vcon params args lenv k)
      (match params with
	(sexp:list ((sexp:symbol label) _))
	-> (let ((tag (alist/get the-context.variant-labels label "unknown variant label?"))
		 (nargs (length args)))
	     (if (> nargs 0)
		 (set-flag! VFLAG-ALLOCATES))
	     ;; in python this was implemented as a %make-tuple primitive, which used
	     ;;   compile-primargs rather than using compile-store-rands.  The generated
	     ;;   code isn't much different, and may put less pressure on the registers.
	     (if (> nargs 0)
		 (insn:alloc
                  tag nargs
                  (make-cont
                   k.target k.free
                   (compile-store-args
                    0 0 args k.target
                    (add-to-set k.target k.free)
                    lenv k)
                   ))
		 (insn:alloc tag 0 k)))
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
		   (tag (get-record-tag sig)))
	       (insn:alloc
                tag
                (length args)
                ;; this is a hack.  the issue is that <k> already holds the target for
                ;;   the allocation... compile-store-args is broken in that it assigns
                ;;   a target for a <stor> [rather than a dead cont].
                (make-cont
                 k.target k.free
                 (compile-store-args
                  0 0 args k.target
                  (add-to-set k.target k.free)
                  lenv k))
                ))
	  _ -> (c-record-extension fields exp lenv k))))

    (define (c-record-extension fields exp lenv k)
      (error "c-record-extension: NYI"))

    (define (record-label-tag label)
      (cmap/add the-context.labels label))

    ;; Note: record *literals* are discovered and recorded in nodes.scm,
    ;;  but expressions that *build* records are not caught until this phase.
    (define (get-record-tag sig)
      (for-each record-label-tag sig)
      (cmap/add the-context.records sig))

    (define (gen-return reg)
      (insn:return reg))
    (define (gen-invoke name closure-reg args-reg k)
      (set-flag! VFLAG-ALLOCATES)
      (insn:invoke name closure-reg args-reg k))
    (define (gen-tail name closure-reg args-reg k)
      (insn:tail name closure-reg args-reg))

    (tree/insert! the-context.profile-funs symbol-index-cmp 'toplevel {index=0 names=current-funs})

    (try
     (compile #t exp (cpsenv:nil) null-cont)
     except
     (:TooManyRegisters _)
     -> (begin
          (printf "too many registers in expression: \n")
          (pp-node exp)
          (error "too many registers."))
     )
    ))

;; --------- literal processing ----------

(define (add-literal lit)
  (cmap/add the-context.literals lit))

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
                   (literal:record _ args) -> (scan-fields args)
                   _ -> 0)
                 (scan-literals tl)))

(define (scan-fields fields)
  (scan-literals
   (map (lambda (f)
          (match f with
            (litfield:t _ val) -> val))
        fields)))

;; --------- type information ----------

;; this never completes because of an issue with type<? - a certain complex type
;;   (an object type) ends up spinning forever.  may be related to non-canonical
;;   record types.  needs more investigation.

;; XXX update: this is almost certainly a problem with exponential growth
;;   when using a `<` function to do deep compares: each level of comparison
;;   makes *two* calls (to eliminate the equality case).
;;   magic<? may help with this problem, but the only true fix is to change
;;   *everything* to use 3-way comparisons.
;;
;; XXX of course the 3-way thing was done aeons ago. revisit this!

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

;; --------- printing ----------

(define (fintlist args)
  (format "(" (join int->string " " args) ")"))

  (define (mname msym)
    (match msym with
      (maybe:yes name) -> (symbol->string name)
      (maybe:no)       -> "lambda"
      ))


(define print-insn
  (insn:return result)             -> (printf "ret " (int result))
  (insn:tail n c args)             -> (printf "tail " (maybe n symbol->string "lambda") " " (int c) " " (int args))
  (insn:trcall d name args)        -> (printf "trcall " (int d) " " (sym name) " " (fintlist args))
  (insn:literal lit k)             -> (printf "lit " (literal->string lit))
  (insn:litcon i kind k)           -> (printf "litcon " (int i) " " (sym kind))
  (insn:cexp sig typ tem args k)   -> (printf "cexp sig:" (type-repr sig) " type:" (type-repr typ) " "
                                              (string tem) " args:" (fintlist args))
  (insn:ffi sig typ name args k)   -> (printf "ffi " (type-repr sig) " " (type-repr typ) " " (sym name) " " (fintlist args))
  (insn:test reg jn then else k)   -> (printf "test " (int reg) "  L" (int jn))
  (insn:jump reg trg jn free)      -> (printf "jmp " (int reg) " trg:" (int trg) " L" (int jn) " free:" (fintlist free.val))
  (insn:close name nreg body k)    -> (printf "close " (sym name) " nreg:" (int nreg))
  (insn:varref d i k)              -> (printf "ref " (int d) "," (int i))
  (insn:varset d i v k)            -> (printf "set " (int d) "," (int i) " val:" (int v))
  (insn:store o a t i k)           -> (printf "stor off:" (int o) " arg:" (int a) " tup:" (int t) " idx:" (int i))
  (insn:invoke n c a k)            -> (printf "invoke " (maybe n symbol->string "lambda") " cl:" (int c) " args:" (int a))
  (insn:new-env n top? types k)    -> (printf "env n:" (int n) " top?:" (bool top?) " " (join type-repr " " types))
  (insn:alloc tag size k)          -> (printf "alloc tag:" (int tag) " size:" (int size))
  (insn:push r k)                  -> (printf "push " (int r))
  (insn:pop r k)                   -> (printf "pop " (int r))
  (insn:primop name p t args k)    -> (printf "prim " (sym name) " " (repr p) " " (type-repr t) " " (fintlist args))
  (insn:move var src k)            -> (printf "mov dst:" (int var) " src:" (int src))
  (insn:fatbar lab jn k0 k1 k)     -> (printf "fatbar fail L" (int lab) " jump L" (int jn))
  (insn:fail lab npop f)           -> (printf "fail L" (int lab) " npop:" (int npop) " free:" (fintlist f.val))
  (insn:testcexp r s t jn k0 k1 k) -> (printf "testcexp " (fintlist r) " " (type-repr s) " " (string t) " L" (int jn))
  (insn:nvcase tr dt labels jn alts ealt k)
  -> (printf "nvcase " (int tr) " " (sym dt) " (" (join symbol->string " " labels) ") L" (int jn))
  (insn:pvcase tr labels arities jn alts ealt k)
  -> (printf "pvcase " (int tr) " (" (join symbol->string " " labels) ") arities:" (fintlist arities) " L" (int jn))
  (insn:label label next)          -> (printf "L" (int label) ":")
  )

(define (print-cps insn d)

  (match insn with
    (insn:label label next)
    -> (begin
         (printf "\nL" (int label) ":")
         (set! insn next))
    _ -> #u
    )

  (let ((k (insn->cont insn)))
    (printf "\n" (repeat d " ") (if (<= k.target 0) "-" (int->string k.target)) " ")
    (print-insn insn)
    ;; print sub-expressions
    (for-list sub (insn->subexps insn)
      (print-cps sub (+ 1 d)))
    (when (not (null-cont? k))
      (print-cps k.insn d))
    ))

;; what registers are directly referenced by this insn?
(define insn->refs
  (insn:return result)             -> (LIST result)
  (insn:tail _ c a)                -> (LIST c a)
  (insn:trcall _ _ args)           -> args
  (insn:cexp _ _ _ args _)         -> args
  (insn:ffi _ _ _ args _)          -> args
  (insn:test reg _ _ _ _)          -> (LIST reg)
  (insn:jump reg trg _ _)          -> (LIST reg trg)
  (insn:varset _ _ v _)            -> (LIST v)
  (insn:store _ a t _ _)           -> (LIST a t)
  (insn:invoke _ c a _)            -> (LIST c a)
  (insn:push r _)                  -> (LIST r)
  (insn:pop r _)                   -> (LIST r)
  (insn:primop _ _ _ args _)       -> args
  (insn:move var src _)            -> (LIST var src)
  (insn:testcexp args _ _ _ _ _ _) -> args
  (insn:nvcase tr _ _ _ _ _ _)     -> (LIST tr)
  (insn:pvcase tr _ _ _ _ _ _)     -> (LIST tr)
  _                                -> (list:nil)
  )

;; sub-expressions of this insn. these only occur in branching insns (and `close`).
(define insn->subexps
  (insn:test reg jn then else k)                 -> (LIST then else)
  (insn:testcexp r s t jn k0 k1 k)               -> (LIST k0 k1)
  (insn:close name nreg body k)                  -> (LIST body)
  (insn:fatbar lab jn k0 k1 k)                   -> (LIST k0 k1)
  (insn:nvcase tr dt labels jn alts ealt k)      -> (if-maybe alt ealt (list:cons alt alts) alts)
  (insn:pvcase tr labels arities jn alts ealt k) -> (if-maybe alt ealt (list:cons alt alts) alts)
  _                                              -> (list:nil)
  )

(define insn->cont
  (insn:fatbar _ _ _ _ k)       -> k
  (insn:close _ _ _ k)          -> k
  (insn:test _ _ _ _ k)         -> k
  (insn:testcexp _ _ _ _ _ _ k) -> k
  (insn:nvcase _ _ _ _ _ _ k)   -> k
  (insn:pvcase _ _ _ _ _ _ k)   -> k
  (insn:literal _ k)            -> k
  (insn:litcon _ _ k)           -> k
  (insn:cexp _ _ _ _ k)         -> k
  (insn:ffi _ _ _ _ k)          -> k
  (insn:varref _ _ k)           -> k
  (insn:varset _ _ _ k)         -> k
  (insn:store _ _ _ _ k)        -> k
  (insn:invoke _ _ _ k)         -> k
  (insn:new-env _ _ _ k)        -> k
  (insn:alloc _ _ k)            -> k
  (insn:push _ k)               -> k
  (insn:pop _ k)                -> k
  (insn:primop _ _ _ _ k)       -> k
  (insn:move _ _ k)             -> k
  (insn:label _ next)           -> (insn->cont next)
  _                             -> null-cont
  )

;; goal: trim the list of free registers/conts to avoid passing any
;;   unreferenced registers to fail/jump continuations.
;; this is use-def for cps registers.
;; we want to walk the tree from the leaves up.
;; we get the free references from the sub-expressions & continuation.
;; when we see a definition, we remove it from the map.
;; when we see a list of free vars, we look at the map and remove
;;   any that are not referenced.

(define (trim-free-regs insn)

  (let ((jumps (tree/empty)))

    (define (in-refset? refset n)
      (set/member? refset int-cmp n))

    (define (trim-free refset free)
      (filter (lambda (x) (in-refset? refset x)) free))

    ;; these three insns discard/ignore any continuation, so they represent
    ;;  leaf nodes in the use/free/ref set.
    (define no-cont?
      (insn:return _)     -> #t
      (insn:tail _ _ _)   -> #t
      (insn:trcall _ _ _) -> #t
      _                   -> #f
      )

    (define (W insn refset)
      ;; special handling for labels.
      (match insn with
        (insn:label num next)
        -> (let ((refset (W* next refset)))
             (tree/insert! jumps int-cmp num refset)
             refset)
        _ -> (W* insn refset)
        ))

    (define (W* insn refset)
      (let ((cont (insn->cont insn)))
        ;; build the free set of the continuation.
        (when (not (null-cont? cont))
          (set! refset (W cont.insn (if (no-cont? cont.insn) (set/empty) refset))))
        ;; what relationship does that have to the subs?
        ;; merge with the refsets of sub-expressions.
        (for-list sub (reverse (insn->subexps insn))
          (set! refset (set/union int-cmp (W sub refset) refset))
          )
        ;; add any refs of this insn.
        (for-list ref (insn->refs insn)
          (when (> ref 0)
            (set/add! refset int-cmp ref)))
        ;; trim the continuation's free set.
        (when (not (null-cont? cont))
          (set! cont.free (trim-free refset cont.free)))
        ;; remove the target.
        (when (> cont.target 0)
          (set/delete! refset int-cmp cont.target))
        ;; fetch refset for jump & fail from our table.
        (match insn with
          (insn:jump reg trg jn free)
          -> (begin
               (set! refset (tree/get jumps int-cmp jn))
               (set! free.val (trim-free refset free.val))
               )
          (insn:fail label npop free)
          -> (begin
               (set! refset (tree/get jumps int-cmp label))
               (set! free.val (trim-free refset free.val))
               )
          _ -> #u
          )
        ;;(printf (int cont.target) " " (join int->string "," (set->list refset)) "\n")
        refset
        ))
    (W insn (set/empty))
    #u
    )
  )

(define (walk-insns p insn)

  (define (walk insn d)
    (p insn d)
    (match insn with
      (insn:label label next) -> (walk next d)
      _ -> (begin
             (for-list sub (insn->subexps insn)
               (walk sub (+ d 1)))
             (let ((cont (insn->cont insn)))
               (when (not (null-cont? cont))
                 (walk cont.insn d)
                 )))
      ))
  (walk insn 0)
  )

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
