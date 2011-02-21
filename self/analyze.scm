;; -*- Mode: Irken -*-

;;; where to store properties?
;;; Some properties belong to the node itself.  For example, a RECURSIVE
;;;   flag can apply to a particular call, but not all of them.
;;; Other properties belong to a variable or a function - for example a
;;;   function can also be RECURSIVE.  But the property of escaping belongs
;;;   to the function/variable, not to a particular node - although you could
;;;   fake it by attaching it to the definition of the function/variable.
;;;
;;; The cost of accessing these flags could be fairly high, since
;;;   we've chosen to use a red-black tree to store them, and that
;;;   involves symbol (and thus string) comparisons. In an old-style
;;;   lisp, we could throw things onto the property list of each
;;;   symbol.  We don't have or want that, but maybe it might make
;;;   sense to reproduce it in some way?  Rather than using 'symbols'
;;;   to hold names, we make a new symbol-like object to which we can
;;;   attach data?

(define (analyze exp context)
  ;; clear the variable table
  (set! context.vars (tree:empty))
  ;; rebuild it
  (build-vars exp context)
  (find-recursion exp context)
  (find-refs exp context)
  (escape-analysis exp context))

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
	   (vars-set-flag! context name VFLAG-FUNCTION))
      _ -> #u)
    (for-each (lambda (x) (walk x fenv)) exp.subs))
  
  (walk exp '()))


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
	(rename-counter (make-counter 0)))

    (define (set-multiplier name calls)
      ;; XXX NYI
      #u)

    (define (inline node fenv)

      (let/cc return

	  (match node.t with
	    (node:fix names)
	    -> (for-range
		   i (length names)
		   (set! fenv (alist:entry (nth names i)
					   (nth node.subs i)
					   fenv)))

	    (node:call)
	    -> (match node.subs with
		 () -> (impossible)
		 ({t=(node:varref name) ...} . rands)
		 -> (let ((fun (alist/get fenv name "inline"))
			  (var (vars-get-var context name))
			  (escapes (bit-get var.flags VFLAG-ESCAPES))
			  (recursive (bit-get var.flags VFLAG-RECURSIVE))
			  (calls var.calls))
		      (print-string (format "testing " (sym name) " calls " (int calls) " escapes " (bool escapes) " recursive " (bool recursive) "\n"))
		      (cond ((and (not (eq? (string-ref (symbol->string name) 0) #\^))
				  (> calls 0)
				  (and (or (<= fun.size inline-threshold)
					   (and (= calls 1) (not escapes)))
				       (not recursive)))
			     (if (> calls 1)
				 (set-multiplier name calls))
			     ;; XXX re-walk ('sneaky')
			     ;;(return (inline (inline-application fun rands) fenv))
			     (let ((r (inline-application fun rands)))
			       (print-string "inlined, looks like this:")
			       (pp-node r 4) (newline)
			       (return r)))
			    (else #u)))
		 ;;({t=(node:function name formals) ...})
		 ;; _ -> node))
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

	  (define (get-new-names names)
	    (let ((names0 (map append-suffix names)))
	      (set! new-vars (append names0 new-vars))
	      (set! lenv (append names lenv))
	      names0))

	  ;; start with a copy of this node.
	  (set! node (node-copy node))
	  (match node.t with
	    (node:let names) -> (set! node.t (node:let (get-new-names names)))
	    (node:fix names) -> (set! node.t (node:fix (get-new-names names)))
	    ;; XXX should we rename the function too?
	    (node:function name formals) -> (set! node.t (node:function name (get-new-names formals)))
	    (node:varref name) -> (if (member-eq? name lenv)
				      (set! node.t (node:varref (append-suffix name))))
	    (node:varset name) -> (if (member-eq? name lenv)
				      (set! node.t (node:varset (append-suffix name))))
	    _ -> #u)
	  (set! node.subs (map (lambda (x) (rename x lenv)) node.subs))
	  node)

	(rename body '())
	))

    (define (safe-nvget-inline rands)
      (match rands with
	({t=(node:primapp '%nvget' _) subs=({t=(node:varref name) ...} . _)  ...} . _)
	-> (let ((var (vars-get-var context name)))
	     (= 0 var.sets))
	_ -> #f))

    (define (inline-application fun rands)
      (let ((simple '())
	    (complex '())
	    (n (length rands))
	    (body (instantiate fun))) ;; alpha converted copy of the function
	(match fun.t with
	  (node:function name formals)
	  -> (cond ((not (= n (length formals))) (error1 "inline: bad arity" fun))
		   (else (for-range
			     i n
			     (let ((formal (nth formals i))
				   (fvar (vars-get-var context formal))
				   (rand (nth rands i)))
			       (match rand.t with
				 (node:literal _) -> (PUSH simple i)
				 (node:varref arg)
				 -> (let ((avar (vars-get-var context arg)))
				      (if (or (> avar.sets 0) (> fvar.sets 0))
					  (PUSH complex i)
					  (PUSH simple i)))
				 _ -> (if (and (= 1 fvar.refs) (safe-nvget-inline rands))
					  (PUSH simple i)
					  (PUSH complex i)))))
			 (print-string "name=") (printn name)
			 (print-string "simple=") (printn simple)
			 (print-string "complex=") (printn complex)
			 (let ((substs
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
				  complex)
				 (let ((body (substitute body substs)))
				   (print-string "substituted body:")
				   (pp-node body 4) (newline)
				   (node/let (reverse names) (reverse inits) body)))
			       ))))
	  _ -> (error "inline-application")
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
		      (maybe:yes val) -> (node/varset name val)
		      (maybe:no) -> node)
		 _ -> node)))
	  (set! node0.subs (map walk node0.subs))
	  node0))
    
      (walk body))

    ;; body of do-inlining
    (inline root (alist:nil))
    ))

(define (escape-analysis root context)

  (let ((escaping-funs '()))

    ;; for each variable, we need to know if it might potentially
    ;;  escape.  a variable 'escapes' when it is referenced while free
    ;;  inside a function that escapes (i.e., any function that is
    ;;  varref'd outside of the operator position).
  
    (define (fun-escapes name node)
      (vars-set-flag! context name VFLAG-ESCAPES)
      (PUSH escaping-funs node))

    (define (find-escaping-functions node parent)
      (match node.t with
	(node:function name _)
	-> (match parent.t with
	     (node:fix _) -> #u
	     ;; any function defined outside a fix (i.e., a lambda) is by
	     ;;   definition an escaping function - because we always reduce
	     ;;   ((lambda ...) ...) to (let ...)
	     _ -> (fun-escapes name node))
	(node:varref name)
	-> (if (vars-get-flag context name VFLAG-FUNCTION)
	       (match parent.t with
		 (node:call)
		 ;; any function referenced in a non-rator position
		 -> (if (not (eq? (first parent.subs) node))
			(fun-escapes name node))
		 _ -> (fun-escapes name node)))
	_ -> #u)
      (for-each (lambda (x) (find-escaping-functions x node)) node.subs))

    (define (maybe-var-escapes name lenv)
      (if (not (member-eq? name lenv))
	  ;; reference to a free variable. flag it as escaping.
	  (vars-set-flag! context name VFLAG-ESCAPES)))

    ;; XXX make sure we still need to know if particular variables escape.

    ;; within each escaping function, we search for escaping variables.
    (define (find-escaping-variables node lenv)
      (match node.t with
	;; the three binding constructs extend the environment...
	(node:function _ formals) -> (set! lenv (append formals lenv))
	(node:fix names) -> (set! lenv (append names lenv))
	(node:let names) -> (set! lenv (append names lenv))
	;; ... and here we search the environment.
	(node:varref name) -> (maybe-var-escapes name lenv)
	(node:varset name) -> (maybe-var-escapes name lenv)
	_ -> #u)
      (for-each (lambda (x) (find-escaping-variables x lenv)) node.subs))

    ;; first we identify escaping functions
    (find-escaping-functions root (node/literal (literal:int 0)))
    (for-each
     (lambda (fun)
       (find-escaping-variables fun '()))
     escaping-funs)
    ))


;; ok, what all is done inside analyze.py?
;;  find aliases
;;  optimize nvcase
;;  transforms
;;  recursion
;;  find applications / tree-walk
;;  escape analysis
;;  call graph
;;  inlining
;;  again: transform, find-applications
;;  pruning
;;  again: escape analysis
;;  find leaves

;; what transforms are done?
;; primapp: only &vcase
;; conditional: optimized #t/#f case
;; let_splat:
;;   coalesce
;;   (let (x <init>) x) => <init>
;;   (let (x (let ((a _) (b _) ...) ...)))
;; varref: aliases
;; fix: coalesce
;; sequence: coalesce
;; pvcase: this is the only difficult one
;;   change %vcase prims into a single node
;; 
;; It would be nice if we could let the macro system
;;   handle the transforms, but we can't: the transforms
;;   are applied again after inlining.  Maybe if we applied
;;   inlining *before* turning into nodes?  That'd be a pretty
;;   big design change though.

;; what's needed for inlining?
;; replace
;; instantiate
;; substitute
;; count fun calls
;; count assignments
;; inline multiplier

(define (replace-nodes p node)
  (define (replace n)
    (let ((n0 (p n))
	  (new-subs (map replace n0.subs)))
      (set! n0.subs new-subs)
    n0))
  (replace node))

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
	   ({t=(node:literal (literal:bool b)) ...} then else)
	   -> (if b (return (simpleopt then)) (return (simpleopt else)))
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

(define (do-trim node context)
  (set! removed-count 0)
  (analyze node context)
  (let loop ((r (trim-unused node context)))
    (print-string (format "trim-unused: " (int removed-count) "\n"))
    (cond ((> removed-count 0)
	   (set! removed-count 0)
	   (analyze r context)
	   (loop (trim-unused r context)))
	  (else r))))

(define (trim-unused exp context)
  
  (define (walk exp)
    (let/cc return
      (set! removed-count (+ removed-count 1))
      (match exp.t with
	(node:fix names)
	-> (let ((n (length names))
		 (inits exp.subs)
		 (remove '()))
	     (for-range
		 i n
		 (let ((name (nth names i))
		       (var (vars-get-var context name)))
		   (if (= 0 var.refs)
		       (PUSH remove i))))
	     (if (null? remove)
		 #u
		 (let ((new-names '())
		       (new-inits '())
		       (trimmed (map (lambda (i) (nth names i)) remove)))
		   (print-string (format "trimming: " (join symbol->string ", " trimmed) "\n"))
		   (for-range
		       i n
		       (cond ((not (member-eq? i remove))
			      (PUSH new-names (nth names i))
			      (PUSH new-inits (nth inits i)))))
		   (return (node/fix (reverse new-names)
				     (reverse new-inits)
				     (walk (last inits))))
		   )))
	_ -> #u)
      (set! removed-count (- removed-count 1))
      (set! exp.subs (map walk exp.subs))
      exp))

  (walk exp))
	
      
	     
		 
		  

  