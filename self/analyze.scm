;; -*- Mode: Irken -*-

;;; where to store properties?
;;; Some properties belong to the node itself.  For example, a RECURSIVE
;;;   flag can apply to a particular call, but not all of them.
;;; Other properties belong to a variable or a function - for example a
;;;   function can also be RECURSIVE.  But the property of escaping belongs
;;;   to the function/variable, not to a particular node - although you could
;;;   fake it by attaching it to the definition of the function/variable.
;;;
;; XXX consider combining these passes

(define (find-recursion exp)

  (define (walk exp fenv)
    (match (noderec->t exp) with
      (node:call)
      -> (let ((rator (nth (noderec->subs exp) 0)))
	   (match (noderec->t rator) with
	     (node:varref name)
	     -> (let ((var (vars-get-var name)))
		  (cond ((member-eq? name fenv)
			 (set! var.flags (bit-set var.flags VFLAG-RECURSIVE))
			 (node-set-flag! exp NFLAG-RECURSIVE)))
		  (set! var.calls (+ 1 var.calls)))
	     _ -> #u
	     ))
      (node:function name _)
      -> (begin
	   (PUSH fenv name)
	   (tree/insert! the-context.funs symbol-index-cmp name exp)
	   (vars-set-flag! name VFLAG-FUNCTION))
      ;; a convenient place to detect this.
      (node:primapp name _)
      -> (if (or (eq? name '%getcc) (eq? name '%putcc))
	     (vars-set-flag! (car fenv) VFLAG-GETCC))
      _ -> #u)
    (for-each (lambda (x) (walk x fenv)) (noderec->subs exp)))

  (walk exp '()))

(define (find-tail node)

  (define walk-sequence
    tail? (exp)        -> (walk tail? exp)
    tail? (hd . tl)    -> (begin (walk #f hd) (walk-sequence tail? tl))
    tail? ()           -> (error "empty sequence")
    )

  (define (walk-list tail? exps)
    (for-each (lambda (x) (walk tail? x)) exps))

  (define (walk-let tail? exps)
    (let ((rsubs (reverse exps))
	  (body (car rsubs))
	  (inits (cdr rsubs)))
      (walk tail? body)
      (walk-list #f inits)
      ))

  (define (walk-primapp tail? exp)
    (match (noderec->t exp) with
      ;; these are special-case w.r.t. tail status
      (node:primapp '%fatbar _)      -> (walk-list tail? (noderec->subs exp))
      (node:primapp '%exn-raise _)   -> (walk tail? (first (noderec->subs exp)))
      (node:primapp '%exn-handle _)  -> (walk tail? (second (noderec->subs exp)))
      _ -> (walk-list #f (noderec->subs exp))
      ))

  (define (walk tail? exp)
    (if tail?
	(node-set-flag! exp NFLAG-TAIL))
    (let ((subs (noderec->subs exp)))
      (match (noderec->t exp) with
	(node:sequence)      -> (walk-sequence tail? subs)
	(node:if)            -> (match subs with
				  (test then else)
				  -> (begin (walk #f test)
					    (walk tail? then)
					    (walk tail? else))
				  _ -> (error "malformed conditional"))
	(node:function _ _)  -> (walk #t (car subs))
	(node:varset _)      -> (walk #f (car subs))
	(node:cexp _ _ _)    -> (walk-list #f subs)
	(node:call)          -> (walk-list #f subs)
	(node:primapp _ _)   -> (walk-primapp tail? exp)
	(node:nvcase _ _ _)  -> (match subs with
				  (val eclause . alts)
				  -> (begin (walk #f val)
					    (walk tail? eclause)
					    (walk-list tail? alts))
				  _ -> (error "malformed nvcase"))
	(node:fix _)         -> (walk-let tail? subs)
	(node:let _)         -> (walk-let tail? subs)
	(node:subst _ _)     -> #u
	_                    -> #u
	)))
  (walk #t node)
  )

(define (find-leaves node)
  (define (search exp)
    (let ((leaf?
	   (match (noderec->t exp) with
	     (node:call) -> (node-get-flag exp NFLAG-TAIL)
	     _ -> #t)))
      (for-each
       (lambda (sub)
	 (let ((sub-leaf? (search sub)))
	   (if sub-leaf? (node-set-flag! sub NFLAG-LEAF))
	   (set! leaf? (and leaf? sub-leaf?))))
       (noderec->subs exp))
      leaf?))
  (if (search node)
      (node-set-flag! node NFLAG-LEAF)))


(define (find-refs node)

  (define (add-ref name)
    (let ((var (vars-get-var name)))
      (set! var.refs (+ 1 var.refs))))

  (define (add-set name)
    (let ((var (vars-get-var name)))
      (set! var.sets (+ 1 var.sets))))

  (define (walk node)
    (match (noderec->t node) with
      (node:varref name) -> (add-ref name)
      (node:varset name) -> (add-set name)
      _ -> #u)
    (for-each walk (noderec->subs node)))

  (walk node))

(define (find-free-refs node)

  (define (maybe-free name locals)
    (if (not (member-eq? name locals))
	(vars-set-flag! name VFLAG-FREEREF)))

  (define (search node locals)
    (match (noderec->t node) with
      ;; these two binding constructs extend the environment...
      (node:fix names)		-> (set! locals (append names locals))
      (node:let names)		-> (set! locals (append names locals))
      ;; but only this one adds a boundary layer
      (node:function _ formals) -> (set! locals formals)
      ;; ... and here we search the environment.
      (node:varref name)	-> (maybe-free name locals)
      (node:varset name)	-> (maybe-free name locals)
      _				-> #u)
    (for-each (lambda (x) (search x locals)) (noderec->subs node)))

  (search node (list:nil))
  )

(define (symbol-add-suffix sym suffix)
  (string->symbol (format (sym sym) suffix)))

(define inline-threshold 13)

(define (do-inlining root)

  (let ((inline-counter (make-counter 0))
	(rename-counter (make-counter 0))
	(multiplier (tree/empty))
	)

    (define (set-multiplier name calls)
      ;; when we inline <name>, each function that it calls must have its call-count
      ;;  raised by a factor of <calls>.
      (let ((g the-context.dep-graph))
	(match (g::get name) with
	  (maybe:yes deps)
	  -> (deps::iterate
	      (lambda (dep)
		(match (tree/member multiplier symbol-index-cmp dep) with
		  (maybe:no) -> (tree/insert! multiplier symbol-index-cmp dep calls)
		  (maybe:yes _) -> #u)))
	  (maybe:no) -> #u)))

    ;; XXX no protection against infinite aliases

    (define (follow-aliases fenv name)
      (match (tree/member fenv symbol-index-cmp name) with
	(maybe:no) -> (maybe:no)
	(maybe:yes fun)
	-> (match (noderec->t fun) with
	     (node:varref name0)
	     -> (follow-aliases fenv name0)
	     _ -> (maybe:yes (:pair name fun)))))

    (define (get-fun-calls name calls)
      (match (tree/member multiplier symbol-index-cmp name) with
	(maybe:yes num) -> (* num calls)
	(maybe:no) -> calls))

    (define (inline node fenv)

      (let/cc return

	  (match (noderec->t node) with
	    (node:fix names)
	    -> (for-range
		   i (length names)
		   (tree/insert! fenv symbol-index-cmp (nth names i) (nth (noderec->subs node) i)))

	    (node:call)
	    -> (match (noderec->subs node) with
		 () -> (impossible)
		 ((noderec:t {t=(node:varref name) ...}) . rands)
		 -> (match (follow-aliases fenv name) with
		      (maybe:no) -> #u
		      (maybe:yes (:pair name fun))
		      -> (let ((var (vars-get-var name))
			       (escapes (bit-get var.flags VFLAG-ESCAPES))
			       (recursive (bit-get var.flags VFLAG-RECURSIVE))
			       (getputcc (bit-get var.flags VFLAG-GETCC))
			       ;; this will spin unwanted extra copies
			       ;;(recursive (node-get-flag node NFLAG-RECURSIVE))
			       (calls (get-fun-calls name var.calls)))
                           ;; (printf "testing " (sym name) " calls " (int calls)
                           ;;                       " escapes " (bool escapes) " recursive " (bool recursive) "\n")
			   (cond ((and (function? fun)
				       (not (eq? (string-ref (symbol->string name) 0) #\^))
				       (not getputcc) ;; don't inline functions that use getcc/putcc
				       (> calls 0)
				       (and (or (<= (noderec->size fun) inline-threshold)
						(and (= calls 1) (not escapes)))
					    (not recursive)))
				  (if (> calls 1)
				      (set-multiplier name calls))
				  ;; (printf "inline: " (sym name) " calls " (int calls)" escapes " (bool escapes) " recursive " (bool recursive) "\n")
				  (let ((r (inline-application fun rands)))
				    ;; record the new variables...
				    (add-vars r)
				    (return (inline r fenv)))))))
		 ;; always inline ((lambda (x) ...) ...)
		 ((noderec:t {t=(node:function name formals) ...}) . rands)
		 -> (let ((r (inline-application (car (noderec->subs node)) rands)))
		      ;; (printf "inlined lambda: final size = " (int r.size) "\n")
		      (add-vars r)
		      (return (inline r fenv)))
		 _ -> #u)
	    _ -> #u)

	(set-node-subs! node (map (lambda (x) (inline x fenv)) (noderec->subs node)))
	node
	))

    (define (instantiate fun)
      (let ((new-vars '())
	    (suffix (format "_i" (int (inline-counter.inc))))
	    (body (nth (noderec->subs fun) 0)))

	(define (append-suffix sym)
	  (string->symbol (format (symbol->string sym) suffix)))

	(define (rename node lenv)

	  (define (get-new-name name)
	    (let ((name0 (append-suffix name)))
	      (set! new-vars (list:cons name0 new-vars))
	      (set! lenv (list:cons name lenv))
	      name0))

	  (define (get-new-names names)
	    (map get-new-name names))

	  ;; start with a copy of this node.
	  (set! node (node-copy node))
	  (match (noderec->t node) with
	    (node:let names)		 -> (set-node-t! node (node:let (get-new-names names)))
	    (node:fix names)		 -> (set-node-t! node (node:fix (get-new-names names)))
	    (node:function name formals) -> (set-node-t! node (node:function (get-new-name name) (get-new-names formals)))
	    (node:varref name)		 -> (if (member-eq? name lenv)
						(set-node-t! node (node:varref (append-suffix name))))
	    (node:varset name)		 -> (if (member-eq? name lenv)
						(set-node-t! node (node:varset (append-suffix name))))
	    _ -> #u)
	  (set-node-subs! node (map (lambda (x) (rename x lenv)) (noderec->subs node)))
	  node)

	(rename body '())
	))

    (define (safe-nvget-inline rands)
      (match rands with
	((noderec:t {t=(node:primapp '%nvget _) subs=((noderec:t {t=(node:varref name) ...}) . _)  ...}) . _)
	-> (let ((var (vars-get-var name)))
	     (= 0 var.sets))
	_ -> #f))

    (define (inline-application fun rands)
      (let ((simple '())
	    (complex '())
	    (n (length rands)))
	(match (noderec->t fun) with
	  (node:function name formals)
	  -> (cond ((not (= n (length formals))) (error1 "inline: bad arity" name))
		   (else
		    ;;(printf "inlining function " (sym name) " has " (int n) " formals\n")
		    (for-range
			i n
			(let ((formal (nth formals i))
			      (fvar (vars-get-var formal))
			      (rand (nth rands i)))
			  (if (> fvar.sets 0)
			      (PUSH complex i) ;; if a formal is assigned to, it must go into a let.
			      (match (noderec->t rand) with
				(node:literal _) -> (PUSH simple i)
				(node:varref arg)
				-> (let ((avar (vars-get-var arg)))
				     ;;(printf "formal: " (sym formal) " avar.sets=" (int avar.sets) " fvar.sets=" (int fvar.sets) "\n")
				     (if (> avar.sets 0)
					 (PUSH complex i)
					 (PUSH simple i)))
				_ -> (if (and (= 1 fvar.refs) (safe-nvget-inline rands))
					 (PUSH simple i)
					 (PUSH complex i))))))
		    ;;(print-string "   simple, complex=") (print simple) (printn complex) (newline)
		    (let ((body (instantiate fun)) ;; alpha converted copy of the function
			  (substs
			   (if (not (null? simple))
			       (map (lambda (i) (:pair (nth formals i) (nth rands i))) simple)
			       '())))
		      (if (eq? complex (list:nil))
			  ;; simple - substitute arguments directly
			  (substitute body substs)
			  ;; complex - bind args into (let ...), then inline body
			  ;; generate new names for complex args
			  (let ((names '())
				(inits '())
				(nc (length complex)))
			    (for-each
			     (lambda (i)
			       (let ((name (symbol-add-suffix
					    (nth formals i)
					    (format "_i" (int (rename-counter.inc))))))
				 (PUSH names name)
				 (PUSH inits (nth rands i))
				 (PUSH substs (:pair (nth formals i) (node/varref name)))
				 ))
			     (reverse complex))
			    ;;(print-string "substs = ") (printn substs)
			    (let ((body (substitute body substs)))
			      (node/let (reverse names) (reverse inits) body)))
			  ))))
	  _ -> (error1 "inline-application - inlining non-function?" fun)
	  )))

    (define (substitute body substs)

      (define (lookup name)
	(let loop ((substs substs))
	  (match substs with
	    () -> (maybe:no)
	    ((:pair from to) . tl)
	    -> (if (eq? name from)
		   (maybe:yes to)
		   (loop tl)))))

      (define (walk node)
	(let ((node0
	       (match (noderec->t node) with
		 (node:varref name)
		 -> (match (lookup name) with
		      (maybe:yes val) -> val
		      (maybe:no) -> node)
		 (node:varset name)
		 -> (match (lookup name) with
		      (maybe:yes val) -> (node/varset (varref->name (noderec->t val)) (car (noderec->subs node)))
		      (maybe:no) -> node)
		 _ -> node)))
	  (set-node-subs! node0 (map walk (noderec->subs node0)))
	  node0))

      (walk body))

    ;; body of do-inlining
    (if the-context.options.noinline
	root
	(inline root (tree/empty))
    )))

(define (escape-analysis root)

  (let ((escaping-funs '()))

    ;; for each variable, we need to know if it might potentially
    ;;  escape.  a variable 'escapes' when it is referenced while free
    ;;  inside a function that escapes (i.e., any function that is
    ;;  varref'd outside of the operator position).

    (define (fun-escapes name)
      (vars-set-flag! name VFLAG-ESCAPES)
      (PUSH escaping-funs name))

    (define (find-escaping-functions node parent)
      (match (noderec->t node) with
	(node:function name _)
	-> (match (noderec->t parent) with
	     (node:fix _) -> #u
	     ;; any function defined outside a fix (i.e., a lambda) is by
	     ;;   definition an escaping function - because we always reduce
	     ;;   ((lambda ...) ...) to (let ...)
	     _ -> (fun-escapes name))
	(node:varref name)
	-> (if (vars-get-flag name VFLAG-FUNCTION)
	       (match (noderec->t parent) with
		 (node:call)
		 ;; any function referenced in a non-rator position
		 -> (if (not (eq? (first (noderec->subs parent)) node))
			(fun-escapes name))
		 _ -> (fun-escapes name)))
	_ -> #u)
      (for-each (lambda (x) (find-escaping-functions x node)) (noderec->subs node)))

    (define (maybe-var-escapes name lenv)
      ;;(printf "maybe-var-escapes: " (sym name) "\n")
      (if (not (member-eq? name lenv))
	  ;; reference to a free variable. flag it as escaping.
	  (vars-set-flag! name VFLAG-ESCAPES)))

    ;; XXX make sure we still need to know if particular variables escape.

    ;; within each escaping function, we search for escaping variables.
    (define (find-escaping-variables node lenv)
      (match (noderec->t node) with
	;; the three binding constructs extend the environment...
	(node:function _ formals) -> (set! lenv (append formals lenv))
	(node:fix names)	  -> (set! lenv (append names lenv))
	(node:let names)	  -> (set! lenv (append names lenv))
	;; ... and here we search the environment.
	(node:varref name)	  -> (maybe-var-escapes name lenv)
	(node:varset name)	  -> (maybe-var-escapes name lenv)
	_ -> #u)
      (for-each (lambda (x) (find-escaping-variables x lenv)) (noderec->subs node)))

    ;; first we identify escaping functions
    (find-escaping-functions root (node/literal (literal:int 0)))
    (for-each
     (lambda (name)
       ;;(printf "searching escaping fun " (sym name) "\n")
       (let ((fun (match (tree/member the-context.funs symbol-index-cmp name) with
		    (maybe:yes fun) -> fun
		    (maybe:no) -> (error1 "find-escaping-funs: failed lookup" name))))
       (find-escaping-variables fun '())))
     escaping-funs)
    ))

;; simple cascading optimizations - these only work from the
;; outside-in, not the inside-out, so we make repeated passes
;; until we don't get any more.

(define simpleopt-hits 0)

(define (do-simple-optimizations node)
  (set! simpleopt-hits 0)
  (let loop ((r (simpleopt node)))
    (printf "simpleopt: " (int simpleopt-hits) "\n")
    (cond ((> simpleopt-hits 0)
	   (set! simpleopt-hits 0)
	   (loop (simpleopt r)))
	  (else r))))

(define simpleopt
  (noderec:t node) ->
  ;; early exit means we've rewritten the node, and will recur without
  ;;   the default processing at the end.
  (let/cc return

    ;; assume we'll hit an optimization, undo it down below if not.
    (set! simpleopt-hits (+ 1 simpleopt-hits))
    (match node.t with

      (node:fix ())
      ;; empty fix
      -> (return (simpleopt (first node.subs)))

      (node:fix names0)
      -> (match (unpack-fix node.subs) with
	   ;; body of fix is another fix...
	   (:fixsubs (noderec:t {t=(node:fix names1) subs=subs1 ...}) inits0)
	   -> (match (unpack-fix subs1) with
		(:fixsubs body1 inits1)
		-> (return
		    (simpleopt
		     (node/fix (append names0 names1)
			       (append inits0 inits1)
			       body1))))
	   _ -> #u)

      (node:let ())
      ;; empty let
      -> (return (simpleopt (first node.subs)))

      (node:let names0)
      -> (match (unpack-fix node.subs) with
	   ;; body of let is another let...
	   (:fixsubs (noderec:t {t=(node:let names1) subs=subs1 ...}) inits0)
	   -> (match (unpack-fix subs1) with
		(:fixsubs body1 inits1)
		-> (return
		    (simpleopt
		     (node/let (append names0 names1)
			       (append inits0 inits1)
			       body1))))
	   (:fixsubs body0 inits0)
	   ;; search for any let in the inits
	   -> (let ((n (length inits0)))
		(for-range
		    i n
		    (match (nth inits0 i) with
		      (noderec:t {t=(node:let names1) subs=subs1 ...})
		      -> (match (unpack-fix subs1) with
			   (:fixsubs body1 inits1)
			   -> (return
			       (simpleopt
				;; (let (a b (c (let (d e) <body1>)) f g) <body0>)
				;;   => (let (a b d e (c <body1>) f g) <body0>)
				;; Note: this looks like it makes <d> and <e> visible where they shouldn't be,
				;;   but access to those variables in <body0> will have already been caught
				;;   during earlier phases of the compiler.
				(node/let (append (append (slice names0 0 i) names1) (slice names0 i n))
					  (append (append (slice inits0 0 i) inits1) (list:cons body1 (slice inits0 (+ i 1) n)))
					  body0))))
		      _ -> #u))
		#u))

      (node:if)
      -> (match node.subs with
	   ;; (if #t a b) => a
	   ((noderec:t {t=(node:literal (literal:cons 'bool b _)) ...}) then else)
	   -> (if (eq? b 'true) (return (simpleopt then)) (return (simpleopt else)))
	   _ -> #u)

      (node:sequence)
      -> (if (= 1 (length node.subs))
	     ;; single-item sequence
	     (return (simpleopt (nth node.subs 0)))
	     ;; sequence within sequence
	     (let ((subs0 node.subs)
		   (n (length subs0)))
	       (for-range
		   i n
		   (match (nth subs0 i) with
		     (noderec:t {t=(node:sequence) subs=subs1 ...})
		     -> (return
			 (simpleopt
			  (node/sequence
			   (append (append (slice subs0 0 i) subs1) (slice subs0 (+ 1 i) n)))))
		     _ -> #u))
	       #u))

      _ -> #u)
    (set! simpleopt-hits (- simpleopt-hits 1))
    ;; if we get here, there was no early exit, so just recurse
    ;;  onto the sub-expressions
    (let ((new-subs (map simpleopt node.subs)))
      (set! node.subs new-subs)
      (noderec:t node)
      )))

(datatype fatopt
  (:cons symbol (list symbol) fatopt) ;; name tags
  (:nil)
  )

;; When we know that a particular nvcase represents an exhaustive/complete match,
;;   either replace the nvcase with its body (when only one alt remains), or
;;   flag the else clause as complete to the rest of the compiler.
;;
;; this was translated pretty straight from python, it should maybe
;;   be re-done.
;;
;; XXX the more I think about this, the more this seems like there should
;;   be an easier way.  Ideally, this capability would be built into the
;;   match compiler itself.
;;

(define (optimize-nvcase root)

  (define (lookup name0 fat-env)
    (let loop ((result '())
	       (rib fat-env))
      (match rib with
	(fatopt:nil) -> result
	(fatopt:cons name1 tags rest)
	-> (if (eq? name0 name1)
	       (loop (append result tags) rest)
	       (loop result rest)))))

  (define (get-datatype name)
    (match (alist/lookup the-context.datatypes name) with
      (maybe:no) -> (error1 "no such datatype" name)
      (maybe:yes dt) -> dt))

  (define (is-match/fail? node)
    (match node.t with
      (node:primapp '%match-error _) -> #t
      (node:primapp '%fail _)        -> #t
      _ -> #f))

  (define (search exp0 fat-env)
    (let ((exp (noderec->rec exp0))
	  (fatbar? #f))
      (match exp.t with
	(node:primapp '%fatbar _) -> (set! fatbar? #t)
	(node:nvcase 'nil tags arities) -> #u ;; pvcase
	(node:nvcase dtname tags arities)
	-> (let ((val (car exp.subs)))
	     (match (noderec->t val) with
	       (node:varref name)
	       -> (let ((dt (get-datatype dtname))
			(nalts (dt.get-nalts)))
		    (if (< (length tags) nalts)
			;; this nvcase is not exhaustive.  but have we already looked at
			;; the others?
			(let ((already (lookup name fat-env)))
			  ;;(printf "--- already " (int (length already)) " tags " (int (length tags)) " nalts " (int nalts) "\n")
			  (if (= (+ (length already) (length tags)) nalts)
			      ;; note: nvcase subs = <value> <else-clause> <alt0> ... <altn>
			      (cond ((= (length tags) 1)
				     ;; remove the nvcase, leave only its body...
				     ;;(printf "removed nvcase.\n")
				     (assert (= 3 (length exp.subs)))
				     (assert (is-match/fail? (noderec->rec (nth exp.subs 1))))
				     (set! exp (noderec->rec (nth exp.subs 2)))
				     )
				    (else
				     ;; disable the %match-error/%fail
				     ;; I think we can do this by simply replacing %match-error
				     ;;   with %complete-match (or any other symbol) to tell cps.scm
				     ;;   to ignore it.  Need to write a test to expose this branch!
				     (let ((eclause (noderec->rec (nth exp.subs 1))))
				       (assert (is-match/fail? eclause))
				       (set! eclause.t (node:primapp '%complete-match (sexp:bool #f)))
				       ;;(raise (:NotTestedError))
				       )
				     ))
			      ;; not exhaustive, extend fat-env with these new tags
			      (set! fat-env (fatopt:cons name tags fat-env))
			      ))
			;; this nvcase is exhaustive, no need to extend fat-env
			#u))
	       ;; probably a manually-constructed nvcase
	       _ -> (warning "non-standard nvcase expression.")
	       ))
	_ -> #u)
      ;; fatbar is tricky here, because it can represent a sequence of tests,
      ;;   but in such a way that the test performed is *not* a direct ancestor
      ;;   of later tests... therefore when maintaining fat_env, we treat fatbar
      ;;   specially, and preserve the interior version of fat_env for the second
      ;;   test.  [theoretically this hack could be avoided if we had a variant of
      ;;   fatbar that correctly maintained the parent/child relationship between
      ;;   earlier and later tests... but this would require code duplication, the
      ;;   elimination of which is the whole *purpose* of fatbar]
      (let ((new-subs '()))
	(for-each
	 (lambda (sub)
	   (let-values (((new-sub fat-env2) (search sub fat-env)))
	     (if fatbar?
		 ;; if we are in a fatbar, preserve the value of fat-env for the second branch.
		 (set! fat-env fat-env2))
	     (PUSH new-subs new-sub)))
	 exp.subs)
	(set! exp.subs (reverse new-subs))
	(set! exp.size (sum-size exp.subs))
	(:tuple (noderec:t exp) fat-env)
	)))
  (printf "starting optimize-nvcase...\n")
  (search root (fatopt:nil))
  (printf "done with optimize-nvcase.\n")
  )

(define (remove-onearmed-nvcase root)

  ;; remove nvcase for datatypes with only one alt.
  ;;  [this is now more common since we removed recursive types].

  (define (get-datatype name)
    (match (alist/lookup the-context.datatypes name) with
      (maybe:no) -> (error1 "no such datatype" name)
      (maybe:yes dt) -> dt))

  (define (search exp)
    (match (noderec->t exp) with
      (node:nvcase 'nil _ _) -> #u	;; pvcase
      (node:nvcase dtname tags arities)
      -> (let ((dt (get-datatype dtname)))
	   (if (= 1 (dt.get-nalts))	;; the one-armed man
	       (set! exp (nth (noderec->subs exp) 2))
	       #u))
      _ -> #u)
    (set-node-subs! exp (map search (noderec->subs exp)))
    exp
    )
  
  (search root)
  )

(define removed-count 0)

(define (do-trim top)
  (let ((g the-context.dep-graph)
	(seen (symbol-set-maker '())))

    ;;(print-string "do-trim:\n")
    ;;(print-graph g)

    (define (walk name)
      (match (g::get name) with
	(maybe:no) -> #u ;; not a function / no deps
	(maybe:yes deps)
	-> (begin
	     (seen::add name)
	     (for-each
	      (lambda (dep)
		(if (not (seen::in dep))
		    (walk dep)))
	      (deps::get)))))

    (define (trim node)
      (let ((node0
	     (match (noderec->t node) with
	       (node:fix names)
	       -> (let ((n (length names))
			(inits (noderec->subs node))
			(remove '()))
		    (for-range
			i n
			(if (not (seen::in (nth names i)))
			    (PUSH remove i)))
		    (if (null? remove)
			node
			(let ((new-names '())
			      (new-inits '()))
			  (for-range
			      i n
			      (cond ((not (member-eq? i remove))
				     (PUSH new-names (nth names i))
				     (PUSH new-inits (nth inits i)))))
			  (node/fix (reverse new-names)
				    (reverse new-inits)
				    (last inits))
			  )))
	       _ -> node)))
	(set-node-subs! node0 (map trim (noderec->subs node0)))
	node0))

    (walk 'top)
    (trim top)
    ))

(define (analyze exp)
  ;; clear the variable table
  (set! the-context.vars (tree/empty))
  (set! the-context.funs (tree/empty))
  ;; rebuild it
  (build-vars exp)
  (find-recursion exp)
  (find-refs exp)
  (escape-analysis exp)
  )

(define (do-one-round node)
  (analyze node)
  ;;(print-vars)
  (build-dependency-graph node)
  ;;(print-graph the-context.dep-graph)
  ;; trim, simple, inline, simple
  (do-simple-optimizations
   (do-inlining
    (do-simple-optimizations
     (do-trim node)))))
