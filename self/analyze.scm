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

(define (find-recursion exp context)

  (define (walk exp fenv)
    (match exp.t with
      (node:call)
      -> (let ((rator (nth exp.subs 0)))
	   (match rator.t with
	     (node:varref name)
	     -> (let ((var (vars-get-var context name)))
		  (cond ((member-eq? name fenv)
			 (set! var.flags (bit-set var.flags VFLAG-RECURSIVE))
			 (node-set-flag! exp NFLAG-RECURSIVE)))
		  (set! var.calls (+ 1 var.calls)))
	     _ -> #u
	     ))
      (node:function name _)
      -> (begin
	   (PUSH fenv name)
	   (set! context.funs (tree/insert context.funs symbol-index<? name exp))
	   (vars-set-flag! context name VFLAG-FUNCTION))
      ;; a convenient place to detect this.
      (node:primapp name _)
      -> (if (or (eq? name '%getcc) (eq? name '%putcc))
	     (vars-set-flag! context (car fenv) VFLAG-GETCC))
      _ -> #u)
    (for-each (lambda (x) (walk x fenv)) exp.subs))
  
  (walk exp '()))

(define (find-leaves node)
  (define (search exp)
    (let ((leaf?
	   (match exp.t with
	     (node:call) -> (node-get-flag exp NFLAG-RECURSIVE)
	     _ -> #t)))
      (for-each
       (lambda (sub)
	 (let ((sub-leaf? (search sub)))
	   (if sub-leaf? (node-set-flag! sub NFLAG-LEAF))
	   (set! leaf? (and leaf? sub-leaf?))))
       exp.subs)
      leaf?))
  (if (search node)
      (node-set-flag! node NFLAG-LEAF)))

(define (find-refs node context)

  (define (add-ref name)
    (let ((var (vars-get-var context name)))
      (set! var.refs (+ 1 var.refs))))

  (define (add-set name)
    (let ((var (vars-get-var context name)))
      (set! var.sets (+ 1 var.sets))))

  (define (walk node)
    (match node.t with
      (node:varref name) -> (add-ref name)
      (node:varset name) -> (add-set name)
      _ -> #u)
    (for-each walk node.subs))

  (walk node))

(define (symbol-add-suffix sym suffix)
  (string->symbol (format (symbol->string sym) suffix)))

(define inline-threshold 13)

(define (do-inlining root context)

  (let ((inline-counter (make-counter 0))
	(rename-counter (make-counter 0))
	(multiplier (tree/empty))
	)

    (define (set-multiplier name calls)
      ;; when we inline <name>, each function that it calls must have its call-count
      ;;  raised by a factor of <calls>.
      (let ((g context.dep-graph))
	(match (g::get name) with
	  (maybe:yes deps)
	  -> (deps::iterate
	      (lambda (dep)
		(match (tree/member multiplier symbol-index<? dep) with
		  (maybe:no) -> (tree/insert! multiplier symbol-index<? dep calls)
		  (maybe:yes _) -> #u)))
	  (maybe:no) -> #u)))

    ;; XXX no protection against infinite aliases

    (define (follow-aliases fenv name)
      (match (tree/member fenv symbol-index<? name) with
	(maybe:no) -> (maybe:no)
	(maybe:yes fun)
	-> (match fun.t with
	     (node:varref name0)
	     -> (follow-aliases fenv name0)
	     _ -> (maybe:yes (:pair name fun)))))

    (define (get-fun-calls name calls)
      (match (tree/member multiplier symbol-index<? name) with
	(maybe:yes num) -> (* num calls)
	(maybe:no) -> calls))

    (define (inline node fenv)

      (let/cc return

	  (match node.t with
	    (node:fix names)
	    -> (for-range
		   i (length names)
		   (tree/insert! fenv symbol-index<? (nth names i) (nth node.subs i)))

	    (node:call)
	    -> (match node.subs with
		 () -> (impossible)
		 ({t=(node:varref name) ...} . rands)
		 -> (match (follow-aliases fenv name) with
		      (maybe:no) -> #u
		      (maybe:yes (:pair name fun))
		      -> (let ((var (vars-get-var context name))
			       (escapes (bit-get var.flags VFLAG-ESCAPES))
			       (recursive (bit-get var.flags VFLAG-RECURSIVE))
			       (getputcc (bit-get var.flags VFLAG-GETCC))
			       ;; this will spin unwanted extra copies
			       ;;(recursive (node-get-flag node NFLAG-RECURSIVE))
			       (calls (get-fun-calls name var.calls)))
;;                            (print-string (format "testing " (sym name) " calls " (int calls)
;;                                                  " escapes " (bool escapes) " recursive " (bool recursive) "\n"))
			   (cond ((and (function? fun)
				       (not (eq? (string-ref (symbol->string name) 0) #\^))
				       (not getputcc) ;; don't inline functions that use getcc/putcc
				       (> calls 0)
				       (and (or (<= fun.size inline-threshold)
						(and (= calls 1) (not escapes)))
					    (not recursive)))
				  (if (> calls 1)
				      (set-multiplier name calls))
;; 				  (print-string (format "inline: " (sym name) " calls " (int calls)" escapes " (bool escapes) " recursive " (bool recursive) "\n"))
				  (let ((r (inline-application fun rands)))
				    ;; record the new variables...
				    (add-vars r context)
				    (return (inline r fenv)))))))
		 ;; always inline ((lambda (x) ...) ...)
		 ({t=(node:function name formals) ...} . rands)
		 -> (let ((r (inline-application (car node.subs) rands)))
;; 		      (print-string (format "inlined lambda: final size = " (int r.size) "\n"))
		      (add-vars r context)
		      (return (inline r fenv)))
		 _ -> #u)
	    _ -> #u)
    
	(set! node.subs (map (lambda (x) (inline x fenv)) node.subs))
	node
	))

    (define (instantiate fun)
      (let ((new-vars '())
	    (suffix (format "_i" (int (inline-counter.inc))))
	    (body (nth fun.subs 0)))

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
	  (match node.t with
	    (node:let names)		 -> (set! node.t (node:let (get-new-names names)))
	    (node:fix names)		 -> (set! node.t (node:fix (get-new-names names)))
	    (node:function name formals) -> (set! node.t (node:function (get-new-name name) (get-new-names formals)))
	    (node:varref name)		 -> (if (member-eq? name lenv)
						(set! node.t (node:varref (append-suffix name))))
	    (node:varset name)		 -> (if (member-eq? name lenv)
						(set! node.t (node:varset (append-suffix name))))
	    _ -> #u)
	  (set! node.subs (map (lambda (x) (rename x lenv)) node.subs))
	  node)

	(rename body '())
	))

    (define (safe-nvget-inline rands)
      (match rands with
	({t=(node:primapp '%nvget _) subs=({t=(node:varref name) ...} . _)  ...} . _)
	-> (let ((var (vars-get-var context name)))
	     (= 0 var.sets))
	_ -> #f))

    (define (inline-application fun rands)
      (let ((simple '())
	    (complex '())
	    (n (length rands)))
	(match fun.t with
	  (node:function name formals)
	  -> (cond ((not (= n (length formals))) (error1 "inline: bad arity" name))
		   (else
		    ;;(print-string (format "inlining function " (sym name) " has " (int n) " formals\n"))
		    (for-range
			i n
			(let ((formal (nth formals i))
			      (fvar (vars-get-var context formal))
			      (rand (nth rands i)))
			  (if (> fvar.sets 0)
			      (PUSH complex i) ;; if a formal is assigned to, it must go into a let.
			      (match rand.t with
				(node:literal _) -> (PUSH simple i)
				(node:varref arg)
				-> (let ((avar (vars-get-var context arg)))
				     ;;(print-string (format "formal: " (sym formal) " avar.sets=" (int avar.sets) " fvar.sets=" (int fvar.sets) "\n"))
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
	       (match node.t with
		 (node:varref name)
		 -> (match (lookup name) with
		      (maybe:yes val) -> val
		      (maybe:no) -> node)
		 (node:varset name)
		 -> (match (lookup name) with
		      (maybe:yes val) -> (node/varset (varref->name val.t) (car node.subs))
		      (maybe:no) -> node)
		 _ -> node)))
	  (set! node0.subs (map walk node0.subs))
	  node0))
    
      (walk body))

    ;; body of do-inlining
    (inline root (tree/empty))
    ))

(define (escape-analysis root context)

  (let ((escaping-funs '()))

    ;; for each variable, we need to know if it might potentially
    ;;  escape.  a variable 'escapes' when it is referenced while free
    ;;  inside a function that escapes (i.e., any function that is
    ;;  varref'd outside of the operator position).
  
    (define (fun-escapes name)
      (vars-set-flag! context name VFLAG-ESCAPES)
      (PUSH escaping-funs name))

    (define (find-escaping-functions node parent)
      (match node.t with
	(node:function name _)
	-> (match parent.t with
	     (node:fix _) -> #u
	     ;; any function defined outside a fix (i.e., a lambda) is by
	     ;;   definition an escaping function - because we always reduce
	     ;;   ((lambda ...) ...) to (let ...)
	     _ -> (fun-escapes name))
	(node:varref name)
	-> (if (vars-get-flag context name VFLAG-FUNCTION)
	       (match parent.t with
		 (node:call)
		 ;; any function referenced in a non-rator position
		 -> (if (not (eq? (first parent.subs) node))
			(fun-escapes name))
		 _ -> (fun-escapes name)))
	_ -> #u)
      (for-each (lambda (x) (find-escaping-functions x node)) node.subs))

    (define (maybe-var-escapes name lenv)
      ;;(print-string (format "maybe-var-escapes: " (sym name) "\n"))
      (if (not (member-eq? name lenv))
	  ;; reference to a free variable. flag it as escaping.
	  (vars-set-flag! context name VFLAG-ESCAPES)))

    ;; XXX make sure we still need to know if particular variables escape.

    ;; within each escaping function, we search for escaping variables.
    (define (find-escaping-variables node lenv)
      (match node.t with
	;; the three binding constructs extend the environment...
	(node:function _ formals) -> (set! lenv (append formals lenv))
	(node:fix names)	  -> (set! lenv (append names lenv))
	(node:let names)	  -> (set! lenv (append names lenv))
	;; ... and here we search the environment.
	(node:varref name)	  -> (maybe-var-escapes name lenv)
	(node:varset name)	  -> (maybe-var-escapes name lenv)
	_ -> #u)
      (for-each (lambda (x) (find-escaping-variables x lenv)) node.subs))

    ;; first we identify escaping functions
    (find-escaping-functions root (node/literal (literal:int 0)))
    (for-each
     (lambda (name)
       ;;(print-string (format "searching escaping fun " (sym name) "\n"))
       (let ((fun (match (tree/member context.funs symbol-index<? name) with
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
    (print-string (format "simpleopt: " (int simpleopt-hits) "\n"))
    (cond ((> simpleopt-hits 0)
	   (set! simpleopt-hits 0)
	   (loop (simpleopt r)))
	  (else r))))

(define (simpleopt node)
  
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
	   (:fixsubs {t=(node:fix names1) subs=subs1 ...} inits0)
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
	   (:fixsubs {t=(node:let names1) subs=subs1 ...} inits0)
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
		      {t=(node:let names1) subs=subs1 ...}
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
	   ({t=(node:literal (literal:cons 'bool b _)) ...} then else)
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
		     {t=(node:sequence) subs=subs1 ...}
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
      node
      )))

(define removed-count 0)

(define (do-trim top context)
  (let ((g context.dep-graph)
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
	     (match node.t with
	       (node:fix names)
	       -> (let ((n (length names))
			(inits node.subs)
			(remove '()))
		    (for-range
			i n
			(if (not (seen::in (nth names i)))
			    (PUSH remove i)))
		    (if (null? remove)
			node
			(let ((new-names '())
			      (new-inits '())
			      ;; XXX remove when happy, this var only for the print
			      (trimmed (map (lambda (i) (nth names i)) remove)))
			  ;;(print-string (format "trimming: " (join symbol->string ", " trimmed) "\n"))
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
	(set! node0.subs (map trim node0.subs))
	node0))

    (walk 'top)
    (trim top)
    ))

(define (analyze exp context)
  ;; clear the variable table
  (set! context.vars (tree/empty))
  (set! context.funs (tree/empty))
  ;; rebuild it
  (build-vars exp context)
  (find-recursion exp context)
  (find-refs exp context)
  (escape-analysis exp context)
  )

(define (do-one-round node context)
  (analyze node context)
  ;;(print-vars context)
  (build-dependency-graph node context)
  ;;(print-graph context.dep-graph)
  ;; trim, simple, inline, simple
  (do-simple-optimizations
   (do-inlining
    (do-simple-optimizations
     (do-trim node context))
    context)))
    
