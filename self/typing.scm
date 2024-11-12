;; -*- Mode: Irken -*-

(require "self/graph.scm")
(require "self/types.scm")

;; This uses a relatively inexpensive form of an 'occurs check'.
;;   Since apply-subst is used all over the place (including in the
;;   printing of errors), this gets better coverage than an explicit
;;   'occurs check' in the unifier.

;; If you see INFINITE-TYPE in an error message, you have either
;;   inadvertently created one via a bug, or you're trying to do
;;   something not supported but the compiler.

;; XXX can this benefit from memoization?
;;     answer: not really.
;;     we mostly care about it for apply-subst-to-program, but
;;     the cost of wholesale apply-subst to the entire program is
;;     immense (twice the time & space of typing itself!), and not
;;     really needed.  calling apply-subst on demand in the CPS phase
;;     (via `get-node-type`) seems to be easy & sufficient.

(define (apply-subst t)
  (define (p t)
    (let ((t (type-find t))
	  (trec (type->trec t)))
      (if trec.pending
	  (pred 'INFINITE-TYPE '())
	  (match t with
	    (type:tvar _ _) -> t
	    (type:pred name subs _)
	    -> (begin
		 (set! trec.pending #t)
		 (let ((r (pred name (map p subs))))
		   (set! trec.pending #f)
		   r))
	    ))))
  (p t))

(define (get-node-type n)
  (apply-subst (noderec->type n)))

;; --------- tenv API ---------

;; Type Environment: originally a linked list of environments,
;;  we have added two optimizations:
;;  1) use a functional map rather than an alist (speeds lookups by name)
;;  2) we pre-compute the set of all free tvars to speed up the occurs check.

(define (tenv/make)
  {free=(tree:empty) env=(tree:empty)})

(define (tenv/lookup tenv name)
  (tree/member tenv.env symbol-index-cmp name))

(define (tenv/extend tenv name scheme)
  (let ((free0 (scheme/free scheme))
        (free1 tenv.free))
    ;; merge into tenv.free
    (tree/inorder
     (lambda (k v)
       (set! free1 (tree/insert free1 magic-cmp k #f)))
     free0)
    {free=free1 env=(tree/insert tenv.env symbol-index-cmp name scheme)}))

(define (tenv/dump tenv)
  (print-string " tenv= {\n")
  (tree/inorder
   (lambda (k v)
     (printf "   " (sym k) " " (scheme-repr v) "\n"))
   tenv.env)
  (printf " -- free = ")
  (tree/inorder (lambda (k _) (printf (type-repr k) " ")) tenv.free)
  (printf "}\n")
  )

(define (occurs-free-in-tenv tvar tenv)
  (match (tree/member tenv.free magic-cmp tvar) with
    (maybe:yes _) -> #t
    (maybe:no)    -> #f
    ))

(defmacro tenv/extend!
  (tenv-extend! tenv name scheme) -> (set! tenv (tenv/extend tenv name scheme))
  )

(define (tenv/iterate p tenv)
  (tree/inorder p tenv.env))

;; collect all the free tvars in a type scheme
(define scheme/free
  (:scheme gens type)
  -> (let ((free (tree:empty)))
       (let walk ((type type))
         (match type with
           (type:pred _ args _)
           -> (let loop ((args args))
                (match args with
                  ()           -> #u
                  (arg . args) -> (begin (walk arg) (loop args))))
           _ -> (if (not (member-eq? type gens))
                    (tree/insert! free magic-cmp type #f))
           ))
       free))

;; ----------------------------

(define scheme-repr
  (:scheme gens type)
  -> (format "forall(" (join type-repr "," gens) ")." (type-repr (apply-subst type))))

(define (remember-variant-label label)
  (match (alist/lookup the-context.variant-labels label) with
    (maybe:yes _) -> #u
    (maybe:no) -> (let ((index (alist/length the-context.variant-labels)))
		    (alist/push the-context.variant-labels label index))))

(define (type-program root-node)

  (define (unify exp t0 t1)

    (define (print-type-error t0 t1)
      (printf (join "\n" (get-node-context root-node (noderec->id exp) 30)) "\n")
      (printf "node id=" (int (noderec->id exp)) "\n")
      (let ((ut0 (apply-subst t0))
	    (ut1 (apply-subst t1)))
	(printf "\nType Error:\n\t" (type-repr ut0) "\n\t" (type-repr ut1) "\n")
        (printf "\n  " (src-repr (noderec->src exp) the-context.options.src-limit) "\n")))

    (define (type-error t0 t1)
      (print-type-error t0 t1)
      (error "type error"))

    (define (U t0 t1)
      (when the-context.options.debugtyping
	    (printf "    ----U " (type-repr t0) " -- " (type-repr t1) "\n"))
      (let/cc return
	  (let ((u (type-find t0))
		(v (type-find t1)))
	    (when the-context.options.debugtyping
		  (printf "    ----: " (type-repr u) " -- " (type-repr v) "\n"))
	    (if (not (eq? u v))
		(begin
		  (match u v with
		    (type:tvar _ _) _ -> #u
		    _ (type:tvar _ _) -> #u
		    (type:pred pu su _) (type:pred pv sv _)
		    -> (match pu pv with
			 'rlabel _   -> (return (U-row u v))
			 _ 'rlabel   -> (return (U-row v u))
			 'rdefault _ -> (return (U-row u v))
			 _ 'rdefault -> (return (U-row v u))
			 _ _ -> (if (or (not (eq? pu pv))
					(not (= (length su) (length sv))))
				    (type-error t0 t1)
				    #u)))
		  (type-union u v)
		  (match u v with
		    (type:pred _ su _) (type:pred _ sv _) -> (for-each2 U su sv)
		    _ _ -> #u))
		))))

    (define (pred=? sym0 sym1)
      (eq? sym0 sym1))

    (define label=?
      ;; labels are represented with nullary predicates, compare via symbol
      (type:pred na _ _) (type:pred nb _ _) -> (eq? na nb)
      _ _ -> #f)

    (define (U-row u v)
      (match u v with
        ;; u and v are both rlabel
        (type:pred 'rlabel (l0 t0 d0) _) (type:pred 'rlabel (l1 t1 d1) _)
        -> (cond ((label=? l0 l1)
                  ;; identical labels, unify presence.
                  (U t0 t1)
                  (U d0 d1))
                 (else
                  ;; distinct head labels, C-MUTATE-LL
                  (let ((x (new-tvar)))
                    (U d0 (rlabel l1 t1 x))
                    (U d1 (rlabel l0 t0 x)))))
	;; u is rlabel, v is not
	(type:pred 'rlabel (l0 t0 d0) _) (type:pred p1 s1 _)
	-> (cond ((eq? p1 'rdefault)
		  ;; C-MUTATE-DL
		  (U (car s1) t0)
		  (U v d0))
		 (else
		  ;; some other predicate
		  ;; S-MUTATE-GL
		  (let ((n (length s1))
			(tvars0 (map-range i n (new-tvar)))
			(tvars1 (map-range i n (new-tvar))))
		    (U (pred p1 tvars0) t0)
		    (U (pred p1 tvars1) d0)
		    (for-range i n
                      (U (nth s1 i) (rlabel l0 (nth tvars0 i) (nth tvars1 i)))
                      ))))
	;; both are rdefault
	(type:pred 'rdefault (t0) _) (type:pred 'rdefault (t1) _)
	-> (U t0 t1)
	;; ensure that u is the rdefault/δ
	_ (type:pred 'rdefault (t1) _)
	-> (U-row v u)
	;; u is rdefault, v is some other predicate, S-MUTATE-GD
	(type:pred 'rdefault (t0) _) (type:pred p1 s1 _)
	-> (let ((n (length s1))
		 (tvars (map-range i n (new-tvar))))
	     (U t0 (pred p1 tvars))
	     (for-range i n
			(U (nth s1 i) (rdefault (nth tvars i)))))
	;; anything else is an error
	_ _ -> (type-error u v)
	))

    (U t0 t1)
    )

  (define (other-error message node)
    (printf (bold message) "\n")
    (printf "\n  " (src-repr (noderec->src node) the-context.options.src-limit) "\n")
    (raise (:TypingError message))
    )

  (define (instantiate-type-scheme gens type)
    ;; map from gens->fresh
    (when the-context.options.debugtyping
	  (printf "gens= " (join type-repr ", " gens) "\n")
	  (printf "type= " (type-repr type) "\n"))
    (let ((fresh
	   (foldr
	    (lambda (gen al)
	      (match gen with
		(type:tvar id _) -> (alist:entry id (new-tvar) al)
		_ -> (error "instantiate-type-scheme")))
	    (alist:nil)
	    gens
	    )))
      ;; walk the type, replacing each member of <gen> with its fresh tvar.
      (let walk ((t type))
	(match t with
	  (type:pred name args _) -> (pred name (map walk args))
	  (type:tvar id _)        -> (match (alist/lookup fresh id) with
				       (maybe:yes tv) -> tv
				       (maybe:no) -> t)))))

  (define (build-type-scheme type tenv)
    (let ((gens (set/empty)))
      (define (find-generic-tvars t)
	(match t with
	  (type:tvar _ _)
          -> (if (not (occurs-free-in-tenv t tenv)) (set/add! gens magic-cmp t))
	  (type:pred _ args _)
          -> (for-each find-generic-tvars args)))
      (let ((type (apply-subst type)))
	(find-generic-tvars type)
	(when the-context.options.debugtyping
          (printf "build-type-scheme type=" (type-repr type)
                  " gens = " (join type-repr "," (set->list gens))
                  "\n"))
	(:scheme (set->list gens) type))))

  (define (type-of* exp tenv)
    (match (noderec->t exp) with
      (node:literal lit)	    -> (type-of-literal lit exp tenv)
      (node:cexp gens sig _)	    -> (type-of-cexp gens sig exp tenv)
      (node:ffi name sig)           -> (type-of-ffi name sig exp tenv)
      (node:if)			    -> (type-of-conditional exp tenv)
      (node:sequence)		    -> (type-of-sequence (noderec->subs exp) tenv)
      (node:function name formals)  -> (type-of-function name formals (car (noderec->subs exp)) (noderec->type exp) tenv)
      (node:varref name)	    -> (type-of-varref name exp tenv)
      (node:varset name)	    -> (type-of-varset name exp tenv)
      (node:call)		    -> (type-of-call exp tenv)
      (node:fix names)		    -> (type-of-fix names exp tenv)
      (node:let names)		    -> (type-of-let names exp tenv)
      (node:primapp name parms)	    -> (type-of-primapp exp name parms (noderec->subs exp) tenv)
      (node:nvcase dt tags arities) -> (type-of-vcase dt tags arities exp tenv)
      (node:subst _ _)              -> (impossible)
      ))

  (define (type-of exp tenv)
    (when the-context.options.debugtyping
      (printf "( type-of* " (int (noderec->id exp)) " " (format-node-type (noderec->t exp)) " :\n"))
    (let ((texp0 (noderec->type exp))
          (texp1 (type-of* exp tenv)))
      (set-node-type! exp texp1)
      (when the-context.options.debugtyping
        (printf "  type-of* " (int (noderec->id exp)) " " (type-repr (apply-subst texp1)) ")\n"))
      texp1))

  ;; (define (type-of exp tenv)
  ;;   (try
  ;;    (type-of* exp tenv)
  ;;    except
  ;;    (:TypingError msg)
  ;;    -> (begin
  ;;         (printf (join "\n" (get-node-context root-node (noderec->id exp) 30)) "\n")
  ;;         (error msg)
  ;;         ))

  (define (type-of-literal lit exp tenv)
    (match lit with
      (literal:string _) -> string-type
      (literal:int _)	 -> int-type
      (literal:char _)	 -> char-type
      (literal:bool _)   -> bool-type
      (literal:undef)	 -> undefined-type
      (literal:symbol _) -> symbol-type
      (literal:sexp _)   -> sexp-type
      (literal:cons dt v l)
      -> (match (get-dtcon-scheme dt v (length l)) with
           (:scheme gens type)
           -> (match (instantiate-type-scheme gens type) with
                (type:pred 'arrow (result-type . arg-types) _)
                -> (let ((nargs0 (length arg-types))
                         (nargs1 (length l)))
                     (when (not (= nargs0 nargs1))
                       (other-error "Bad Arity" exp))
                     (for-range i (length arg-types)
                       (let ((tx (type-of-literal (nth l i) exp tenv)))
                         (unify exp tx (nth arg-types i))))
                     result-type)
                x -> (error1 "strange constructor scheme" x)))
      (literal:vector l)
      -> (let ((tv (new-tvar)))
           (for-list x l
             (unify exp tv (type-of-literal x exp tenv)))
           (pred 'vector (list tv)))
      (literal:record tag fl)
      -> (let loop ((t (rdefault (rabs)))
                    (fl fl))
           (match fl with
             () -> (rproduct t)
             ((litfield:t name val) . rest)
             -> (loop (rlabel (make-label name) (rpre (type-of-literal val exp tenv)) t) rest)
             ))
      ))

  (define (get-dtcon-scheme dt v arity)
    (if (eq? dt 'nil)
        (get-vcon-scheme v arity)
        ((%%attr (alist/get the-context.datatypes dt "no such datatype")
                 get-alt-scheme) v)))

  ;; XXX duplicates code from %vcon below.
  (define (get-vcon-scheme label arity)
    (let ((plabel (make-label label)))
      (remember-variant-label label)
      (match arity with
        ;; ∀X.() → Σ(l:pre (Π());X)
        0 -> (:scheme (list T0) (arrow (rsum (rlabel plabel (rpre (pred 'product '())) T0)) '()))
        ;; ∀XY.X → Σ(l:pre X;Y)
        1 -> (:scheme (list T0 T1) (arrow (rsum (rlabel plabel (rpre T0) T1)) (list T0)))
        ;; ∀ABCD.Π(A,B,C) → Σ(l:pre (Π(A,B,C));D)
        _ -> (let ((tdflt (new-tvar))
                   (targs (n-tvars arity)))
               (:scheme (list:cons tdflt targs)
                        (arrow (rsum (rlabel plabel (rpre (pred 'product targs)) tdflt))
                               targs))))))

  ;; HACK: remove a raw predicate if present
  (define unraw
    (type:pred 'raw (arg) _) -> arg
    t -> t)

  ;; HACK: frob signature if bogus-bytecode-cexp.
  (define (maybe-frob-cexp-sig pargs)
    (if (eq? the-context.options.backend (backend:bytecode))
        (map ffi-type-frob-int pargs)
        pargs))

  (define (type-of-cexp gens sig exp tenv)
    (match (instantiate-type-scheme gens sig) with
      (type:pred 'arrow pargs _)
      -> (if (not (= (- (length pargs) 1) (length (noderec->subs exp))))
             (error1 "wrong number of args to cexp" exp)
             (match (maybe-frob-cexp-sig pargs) with
               () -> (error1 "malformed arrow type" sig)
               (result-type . parg-types)
               -> (let ((arg-types (map (lambda (x) (type-of x tenv)) (noderec->subs exp))))
                    (for-list2 a b parg-types arg-types
                      (unify exp (unraw a) b))
                    result-type
                    )))
      type -> type))

  (define (type-of-ffi name sig exp tenv)
    (match sig with
      (csig:fun _ frtype fargtypes var?)
      -> (let ((args (noderec->subs exp))
               (argtypes (map (lambda (x) (type-of x tenv)) (noderec->subs exp)))
               (nfixed (length fargtypes)))
           ;; (printf "type-of-ffi " (sym name)
           ;;         " csig " (csig-repr sig)
           ;;         " argtypes (" (join type-repr " " argtypes)
           ;;         ") nfixed=" (int nfixed)
           ;;         "\n")
           ;; %%ffi has 3 extra arguments in bytecode mode.
           (if (eq? the-context.options.backend (backend:bytecode))
               (set! argtypes (drop argtypes 3)))
           ;; unify the <n> fixed (formal) args
           (for-range i nfixed
             (let ((type0 (ctype->irken-type (frob-int-arg (nth fargtypes i))))
                   (type1 (nth argtypes i)))
               (unify exp type0 type1)))
           ;; compute the type of the extra args (though we unify against no formal arg types)
           (for-range i (- (length argtypes) nfixed)
             (let ((arg (nth args (+ nfixed i))))
               ;; (printf "typing extra arg at position " (int (+ nfixed i)) " of type " (type-repr (noderec->type arg)) "\n")
               ;; (pp-node arg)
               (type-of (nth args (+ nfixed i)) tenv)
               ;; (printf " now has type " (type-repr (noderec->type arg)) "\n")
               ))
           (ffi-type-frob-int (ctype->irken-type frtype)))
      (csig:obj _ obtype)
      -> (pred 'cref (list (ffi-type-frob-int (ctype->irken-type obtype))))
      ))

  (define (type-of-conditional exp tenv)
    (match (map (lambda (x) (type-of x tenv)) (noderec->subs exp)) with
      (tif tthen telse)
      -> (begin
	   (unify exp tif bool-type)
	   (unify exp tthen telse)
	   telse)
      _ -> (error1 "malformed conditional" exp)
      ))

  (define (type-of-sequence exps tenv)
    (let loop ((l exps))
      (match l with
	()	  -> (error "empty sequence?")
	(one)	  -> (type-of one tenv)
	(hd . tl) -> (begin
		       ;; ignore all but the last
		       (type-of hd tenv)
		       (loop tl)))))

  ;; XXX TODO - handle user-supplied types (on formals?)
  (define (optional-type formal tenv)
    (new-tvar))

  (define (type-of-function name formals body sig tenv)
    (when the-context.options.debugtyping
      (printf "type-of-function " (bold (sym name)) "\n"))
    (let ((arg-types '()))
      (for-list formal formals
        (let ((type (optional-type formal tenv)))
          (push! arg-types type)
          (tenv/extend! tenv formal (:scheme '() type))))
      (let ((btype (type-of body tenv))
            (ftype (arrow btype (reverse arg-types))))
        (if (no-type? sig)
            ftype
            (begin (unify body ftype sig) sig)))
      ))

  (define (apply-tenv name tenv)
    ;; (print-string "apply-tenv: ") (printn name)
    ;; (tenv/dump tenv)
    (match (tenv/lookup tenv name) with
      ;;(maybe:no) -> (error1 "apply-tenv: unbound variable" name)
      (maybe:no)
      -> (begin
           (tenv/dump tenv)
           (error1 "apply-tenv: unbound variable" name))
      (maybe:yes (:scheme gens type))
      -> (instantiate-type-scheme gens type)
      ;; -> (let ((r (instantiate-type-scheme gens type)))
      ;;      (printf "apply-tenv: => " (type-repr (apply-subst r)) "\n")
      ;;      r
      ;;      )
      ))

  (define (type-of-varref name exp tenv)
    ;; (let ((t (apply-tenv name tenv)))
    ;;   (printf "varref: " (sym name) " type = " (type-repr (apply-subst t)) "\n")
    ;;   t))
    (apply-tenv name tenv)
    )

  (define (type-of-varset name exp tenv)
    ;; XXX implement the no-generalize rule for vars that are assigned.
    ;;     [i.e. the value restriction]
    (let ((val (car (noderec->subs exp)))
	  (t0 (apply-tenv name tenv))
	  (t1 (type-of val tenv)))
      (unify exp t0 t1)
      undefined-type
      ))

  (define (type-of-call exp tenv)
    (match (map (lambda (x) (type-of x tenv)) (noderec->subs exp)) with
      (rator-type . rand-types)
      -> (let ((result-type (new-tvar)))
	   (unify exp rator-type (arrow result-type rand-types))
	   result-type)
      () -> (error "empty call?")
      ))

  (define (type-of-fix names exp tenv)
    ;; reorder fix into dependency order
    (match (reorder-fix names (noderec->subs exp) the-context.scc-graph) with
      (:reordered names0 inits0 body partition)
      -> (let ((n (length names0))
	       (names (list->vector names0))
	       (inits (list->vector inits0))
	       (init-tvars (list->vector (map-range i n (new-tvar))))
	       (init-types (make-vector n no-type))
	       )
	   ;;(printf "reordered: (" (join symbol->string " " names0) "\n")
	   (for-list part partition
             ;; build temp tenv for typing the inits
             (let ((temp-tenv
                    (foldr
                     (lambda (i tenv0)
                       (tenv/extend tenv0 names[i] (:scheme '() init-tvars[i])))
                     tenv (reverse part))))
               ;; type each init in temp-tenv
               (for-list i part
                 (let ((ti (type-of inits[i] temp-tenv)))
                   (unify inits[i] ti init-tvars[i])
                   ;;(set! init-types[i] (apply-subst ti)))
                   (set! init-types[i] ti)
                   ))
               ;; now extend the environment with type schemes instead
               (for-list i part
                 (let ((scheme (build-type-scheme init-types[i] tenv)))
                   (tenv/extend! tenv names[i] scheme)))
               part))
	   ;; type the body in the new polymorphic environment
	   (type-of body tenv))))

  (define (type-of-let names exp tenv)
    (let ((n (length names))
	  (inits (noderec->subs exp)))
      (for-range
	  i n
	  (let ((init (nth inits i))
		(name (nth names i))
		(ta (type-of init tenv)))
	    ;; XXX user-supplied type
	    ;; extend environment
            (tenv/extend! tenv name (:scheme '() ta))))
      ;; type body in the new env
      (type-of (nth inits n) tenv)))

  (define (type-of-vcase dt tags arities exp tenv)
    (if (eq? dt 'nil)
	(type-of-pvcase tags arities exp tenv)
	(type-of-nvcase dt tags exp tenv)))

  (define (type-of-nvcase dt tags exp tenv)
    (let ((dt (alist/get the-context.datatypes dt "no such datatype"))
	  (subs (noderec->subs exp))
	  ;; use match for these!?
	  (value (nth subs 0))
	  (else-exp (nth subs 1))
	  (alts (cdr (cdr subs)))
	  (tval (type-of value tenv))
	  (dt-scheme (dt.get-scheme))
	  (tv (new-tvar)))
      (match dt-scheme with
	(:scheme tvars type)
	-> (if (null? tvars)
	       (unify exp tval type)
	       (let ((type0 (instantiate-type-scheme tvars type)))
		 (unify exp tval type0))))
      ;; each alt has the same type
      (for-list alt alts
        (unify alt tv (type-of alt tenv)))
      ;; this will work even when else-exp is a dummy %%match-error
      (unify else-exp tv (type-of else-exp tenv))
      tv))

  (define (type-of-pvcase tags arities exp tenv)
    (match (noderec->subs exp) with
      (value else-exp . alts)
      -> (let ((tv-exp (new-tvar))
	       (else? (match (noderec->t else-exp) with
			(node:primapp '%fail _) -> #f
			(node:primapp '%match-error _) -> #f
			_ -> #t))
	       (row (if else? (new-tvar) (rdefault (rabs)))))
	   (for-range
	       i (length tags)
	       (let ((alt (nth alts i))
		     (tag (nth tags i))
		     (arity (nth arities i))
		     (argvars (n-tvars arity)))
		 (set! row (rlabel (make-label tag)
				   (rpre (if (= arity 1)
					     (car argvars)
					     (pred 'product argvars)))
				   row))
		 ;; each alt must have the same type
		 (unify alt tv-exp (type-of alt tenv))))
	   (if else?
	       (unify else-exp tv-exp (type-of else-exp tenv)))
	   ;; the value must have the row type determined
	   ;;   by the set of polyvariant alternatives
	   (unify value (rsum row) (type-of value tenv))
	   ;; this is the type of the entire expression
	   tv-exp
	   )
      _ -> (error1 "malformed pvcase" exp)
      ))

  ;; these are used in type schemes in lookup-primap.  Since they are generalized
  ;;  over, it's ok to use the same tvar repeatedly.
  (define T0 (new-tvar))
  (define T1 (new-tvar))
  (define T2 (new-tvar))

  (define n-tvars
    0 -> '()
    n -> (list:cons (new-tvar) (n-tvars (- n 1))))

  (define (prim-error name)
    (error1 "bad parameters to primop" name))

  ;; this function acts like a lookup table for the type signatures of primitives.
  ;; it is a function because some of the prims have parameters that affect their
  ;;   type signatures in a way that requires them to be generated on the fly -
  ;;   for example accessing a record field requires a row type containing a label
  ;;   for the field.

  ;; XXX need a little DSL for type schemes here, building them by hand is ugly/clumsy.

  (define (lookup-primapp name params)
    (match name with
      '%fatbar	    -> (:scheme (list T0) (arrow T0 (list T0 T0)))
      '%fail	    -> (:scheme (list T0) (arrow T0 '()))
      '%match-error -> (:scheme (list T0) (arrow T0 '()))
      '%make-vector -> (:scheme (list T0) (arrow (pred 'vector (list T0)) (list int-type T0)))
      '%array-ref   -> (:scheme (list T0) (arrow T0 (list (pred 'vector (list T0)) int-type)))
      '%array-set   -> (:scheme (list T0) (arrow undefined-type (list (pred 'vector (list T0)) int-type T0)))
      '%rmake       -> (:scheme '() (arrow (rproduct (rdefault (rabs))) '()))
      '%ensure-heap -> (:scheme '() (arrow undefined-type (list int-type)))
      '%rextend     -> (match params with
                         (sexp:symbol label _)
                         -> (let ((plabel (make-label label)))
                              ;; forall('a,'b).({lab=(abs) 'a} 'b -> {lab=(pre 'b) 'a})
                              (:scheme (list T0 T1)
                                       (arrow (rproduct (rlabel plabel (rpre T1) T0))
                                              (list
                                               (rproduct (rlabel plabel (rabs) T0))
                                               T1)))
                              )
                         _ -> (prim-error name))
      '%raccess     -> (match params with
			 (sexp:symbol label _)
			 -> (:scheme (list T0 T1)
				     (arrow T0
					    (list (rproduct (rlabel (make-label label)
								    (rpre T0)
								    T1)))))
			 _ -> (prim-error name))
      '%rset        -> (match params with
			 (sexp:symbol label _)
			 -> (:scheme (list T0 T1)
				     (arrow undefined-type
					    (list (rproduct (rlabel (make-label label)
								    (rpre T0)
								    T1))
						  T0)))
			 _ -> (prim-error name))
      '%dtcon       -> (match params with
			 ;; e.g. list := nil | cons X list
			 ;; %dtcon/list/cons := ∀X.(X,list(X)) → list(X)
			 (sexp:cons dtname altname)
			 -> (match (alist/lookup the-context.datatypes dtname) with
			      (maybe:no) -> (error1 "lookup-primapp: no such datatype" dtname)
			      (maybe:yes dt) -> (dt.get-alt-scheme altname))
			 _ -> (prim-error name))
      '%vcon        -> (match params with
			 (sexp:list ((sexp:symbol label _) (sexp:int arity)))
			 -> (let ((plabel (make-label label)))
			      (remember-variant-label label)
			      (match arity with
				;; ∀X.() → Σ(l:pre (Π());X)
				0 -> (:scheme (list T0) (arrow (rsum (rlabel plabel (rpre (pred 'product '())) T0)) '()))
				;; ∀XY.X → Σ(l:pre X;Y)
				1 -> (:scheme (list T0 T1) (arrow (rsum (rlabel plabel (rpre T0) T1)) (list T0)))
				;; ∀ABCD.Π(A,B,C) → Σ(l:pre (Π(A,B,C));D)
				_ -> (let ((tdflt (new-tvar))
					   (targs (n-tvars arity)))
				       (:scheme (list:cons tdflt targs)
						(arrow (rsum (rlabel plabel (rpre (pred 'product targs)) tdflt))
						       targs)))))
			 _ -> (prim-error name))
      '%nvget       -> (match params with
			 (sexp:list ((sexp:cons dtname altname) (sexp:int index) (sexp:int arity)))
			 -> (if (eq? dtname 'nil)
				;; polymorphic variant
				(let ((argvars (n-tvars arity))
				      (tdflt (new-tvar))
				      (plabel (make-label altname))
				      (vtype (rsum (rlabel plabel
							   (rpre (if (> arity 1)
								     (pred 'product argvars)
								     (car argvars)))
							   tdflt))))
				  (:scheme (list:cons tdflt argvars)
					   ;; e.g., to pick the second arg:
					   ;; ∀0123. Σ(l:pre (0,1,2);3) → 1
					   (arrow (nth argvars index) (list vtype))))
				;; normal variant
				(match (alist/lookup the-context.datatypes dtname) with
				  (maybe:no) -> (error1 "lookup-primapp: no such datatype" dtname)
				  (maybe:yes dt)
				  -> (let ((alt (dt.get altname))
					   (tvars (dt.get-tvars))
					   (dtscheme (pred dtname tvars)))
				       (:scheme tvars (arrow (nth alt.types index) (list dtscheme))))))
			 _ -> (prim-error name))
      '%callocate  -> (let ((type (parse-type params)))
			;; int -> (buffer <type>)
			(:scheme '() (arrow (pred 'buffer (list type)) (list int-type))))
      '%exit       -> (:scheme (list T0 T1) (arrow T0 (list T1)))
      ;; these both can be done with %%cexp, but we need to be able to detect their usage in order to
      ;;   disable inlining of functions that use them.
      '%getcc      -> (:scheme (list T0) (arrow (pred 'continuation (list T0)) (list)))
      '%putcc      -> (:scheme (list T0 T1) (arrow T1 (list (pred 'continuation (list T0)) T0)))
      ;; used in an nvcase else clause when the compiler knows the match is complete
      ;;  [i.e., no else clause is needed]
      '%complete-match -> (:scheme (list T0) (arrow T0 '()))
      ;; llvm prims
      '%llarith    -> (:scheme '() (arrow int-type (list int-type int-type)))
      '%llicmp     -> (:scheme '() (arrow bool-type (list int-type int-type)))
      '%lleq       -> (:scheme (list T0) (arrow bool-type (list T0 T0)))
      '%malloc -> (:scheme '() (arrow (pred 'cref
                                            (list (pred 'array (list (parse-type params)))))
                                      (list int-type)))
      '%halloc -> (:scheme '() (arrow (pred 'cref
                                            (list (pred 'array (list (parse-type params)))))
                                      (list int-type)))
      '%free   -> (:scheme (list T0) (arrow undefined-type (list (pred 'cref (list T0)))))
      '%c-aref -> (:scheme (list T0) (arrow (pred 'cref (list T0))
                                            (list (pred 'cref (list (pred 'array (list T0)))) int-type)))
      ;; forall(t0).cref(*(t0)) -> cref(t0)
      '%c-get-ptr -> (:scheme (list T0)
                              (arrow (pred 'cref (list T0))
                                     (list (pred 'cref (list (pred '* (list T0)))))))
      ;; forall(t0).cref(*(t0)),cref(t0) -> undefined
      '%c-set-ptr -> (:scheme (list T0)
                              (arrow undefined-type
                                     (list (pred 'cref (list (pred '* (list T0))))
                                           (pred 'cref (list T0)))))
      '%c-get-int -> (:scheme (list T0) (arrow int-type (list (pred 'cref (list T0)))))
      '%c-set-int -> (:scheme (list T0) (arrow undefined-type (list (pred 'cref (list T0)) int-type)))
      '%c-sref    -> (get-sref-scheme params)
      '%cref->string -> (:scheme '() (arrow string-type (list (pred 'cref (list (pred 'char '()))) int-type)))
      '%string->cref -> (:scheme '() (arrow (pred 'cref (list (pred 'char '()))) (list string-type)))
      '%c-cast       -> (:scheme (list T0)
                                 (arrow (pred 'cref (list (parse-type params)))
                                        (list (pred 'cref (list T0)))))
      '%c-sizeof     -> (:scheme '() (arrow int-type '()))
      '%cref->int    -> (:scheme (list T0) (arrow int-type (list (pred 'cref (list T0)))))
      '%int->cref    -> (:scheme (list T0) (arrow (pred 'cref (list T0)) (list int-type)))
      ;; for calling internal functions of the form `(object, object, ...) -> object`
      '%llvm-call    -> (make-llvm-scheme params)
      ;; for fetching irken objects in external symbols
      '%llvm-get     -> (make-llvm-scheme params)
      ;; -------------------- FFI --------------------
      _ -> (error1 "lookup-primapp" name)))

  (define (ffi-type-frob-int type)
    (match type with
      ;; promote all c int types to irken ints.
      (type:pred kind _ _) -> (if (member-eq? kind c-int-types)
                                  int-type
                                  type)
      _ -> type
      ))

  (define get-sref
    ;; structname.field0.field1.field2 ...
    (sexp:attr (sexp:symbol sname _) fname)
    -> (:tuple (lookup-field fname (lookup-struct-fields sname)) sname)
    (sexp:attr sub fname)
    -> (let (((ref sname) (get-sref sub)))
         (:tuple (cref-field fname ref) sname))
    x -> (error1 "get-sref: malformed struct/union reference" (repr x))
    )

  (define (get-sref-scheme refexp)
    (let (((ref sname) (get-sref refexp))
          (ftype (ctype->irken-type* ref.ctype)))
      (:scheme (list T0) (arrow (pred 'cref (list ftype))
                                (list (pred 'cref
                                            (list (pred 'struct
                                                        (list (pred sname '()))))))))
      ))

  ;; for typing purposes, all int-like args (and results) become 'int'.
  ;; XXX this would be the place to handle other immediate types like char, #t/#f, etc.
  (define frob-int-arg
    (ctype:int _ _) -> (ctype:int (cint:int) #t)
    x               -> x
    )

  ;; XXX do we need to scan the resulting type for tvars to add to gens?
  (define lookup-ffi-scheme
    (sexp:symbol name _)
    -> (match (ffi-info.sigs::get name) with
         (maybe:yes (csig:fun _ rtype argtypes var?))
         -> (:scheme '() (arrow (ffi-type-frob-int (ctype->irken-type rtype))
                                (map ctype->irken-type (map frob-int-arg argtypes))))
         (maybe:yes (csig:obj _ obtype))
         -> (:scheme '() (arrow (pred 'cref (list (ffi-type-frob-int (ctype->irken-type obtype)))) '()))
         (maybe:no)
         -> (error1 "lookup-ffi-scheme: unknown name" name))
    x -> (error1 "lookup-ffi-scheme: malformed" (repr x))
    )

  (define make-llvm-scheme
    ;; after the sig are optional metadata
    (sexp:list ((sexp:string _) sig . _)) -> (parse-cexp-sig sig)
    x -> (error1 "make-llvm-scheme: malformed" (repr x))
    )

  ;; each exception is stored in a global table along with a tvar
  ;;  that will unify with each use.
  (define (get-exn-type name)
    (match (alist/lookup the-context.exceptions name) with
      (maybe:yes tvar) -> tvar
      (maybe:no)
      -> (let ((tvar (new-tvar)))
	   (alist/push the-context.exceptions name tvar)
	   tvar)))

  ;; given an exception row type for <exp> look up its name in the global
  ;;   table and unify each element of the sum.

  (define (unify-exception-types exp row tenv)
    ;;(printf " ------- " (type-repr row) " -- " (type-repr (apply-subst (type-of exp tenv))) "\n")
    (match row with
      (type:pred 'rlabel ((type:pred exn-name _ _) exn-type _) _)
      -> (let ((global-type (get-exn-type exn-name)))
	   (unify exp global-type exn-type))
      ;; XXX do we need to assert that row is a row type?
      (type:tvar _ _)
      -> #u
      _ -> (error1 "unify-exception-types: bad type" (type-repr row))
      ))

  ;; unify the label from this row with the global table
  (define (type-of-raise val tenv)
    (let ((val-type (apply-subst (type-of val tenv))))
      (match val-type with
	(type:pred 'rsum (row) _)
	-> (begin
	     (unify-exception-types val row tenv)
	     val-type)
        (type:tvar _ _)
        ;; XXX do we need to assert that row is a row type?
        -> val-type
	_ -> (begin
	       (printf "bad exception type:\n")
	       (pp-node val) (newline)
	       (error1 "bad exception type:" val)))))

  ;; unify each label from this row with the global table
  (define (type-of-handle exn-val exn-match tenv)
    (let ((match-type (type-of exn-match tenv))
	  (val-type (apply-subst (type-of exn-val tenv))))
      ;;(printf "type-of-handle: " (type-repr (apply-subst val-type)) "\n")
      (match val-type with
	(type:pred 'rsum (row) _)
	-> (begin
	     (unify-exception-types exn-match row tenv)
	     match-type)
        ;; XXX do we need to assert that row is a row type?
        (type:tvar _ _)
        -> val-type
	_ -> (error1 "unify-handlers: expected row sum type" (type-repr val-type)))))

  (define (type-of-primapp exp name params subs tenv)
    ;; special primapps
    (match name with
      ;; we need a map of exception => tvar, then cross-verify with each handle expression.
      '%exn-raise
      -> (match subs with
	   (exn-val)
	   -> (type-of-raise exn-val tenv)
	   _ -> (other-error "%exn-raise: bad arity" exp))
      '%exn-handle
      ;; %exn-handle is wrapped around the match expression of the exception handler
      -> (match subs with
	   (exn-val exn-match)
	   -> (type-of-handle exn-val exn-match tenv)
	   _ -> (other-error "%exn-handle: bad arity" exp))

      ;; normal primapps
      _ -> (match (lookup-primapp name params) with
	     (:scheme gens type)
	     -> (let ((itype (instantiate-type-scheme gens type)))
		  (when the-context.options.debugtyping
		  	(printf "type-of-primapp, name = " (sym name) " params= " (repr params) "\n")
		  	(printf "type-of-primapp, scheme = " (scheme-repr (lookup-primapp name params)) "\n")
		  	(printf "           instantiated = " (type-repr itype) "\n")
		  	)
		  (match itype with
		    ;; very similar to type-of-cexp
		    (type:pred 'arrow (result-type . arg-types) _)
		    -> (begin
			 (when (not (= (length arg-types) (length subs)))
                           (printf "\n\nwrong number of args to primapp '" (sym name) " params " (repr params) "'\n"
                                   "types: (" (join " " (map type-repr arg-types)) ")\n"
                                   "args: " (join " " (map repr (map noderec->sexp subs))) "\n")
                           (other-error "wrong number of args" exp))
			 (for-range
			     i (length arg-types)
			     (let ((arg (nth subs i))
				   (ta (type-of arg tenv))
				   (arg-type (nth arg-types i)))
			       (unify arg ta arg-type)))
			 result-type)
		    _ -> (other-error "type-of-primapp: expected arrow type" exp)
		    )))))

  ;;(build-graphs node)
  (type-of root-node (tenv/make))
  )

(define (build-graphs node)
  (let ((dep-graph (build-dependency-graph node))
        (strong (strongly dep-graph)))
    (set! the-context.dep-graph dep-graph) ;; needed by do-trim to find unused things
    (set! the-context.scc-graph strong)    ;; needed by type-of-fix for re-ordering
    (when the-context.options.debugtyping
      (printf "strongly-connected-components:\n")
      (printf "(" (join (lambda (x) (format "(" (join symbol->string " " x) ")")) " " strong) ")\n"))
    ;; (printf "SCC:\n")
    ;; (printn strong)
    ))

(define (dump-types n)

  (define (strip-alpha name)
    (let ((name0 (symbol->string name))
          (parts (string-split name0 #\_)))
      (let loop ((parts (reverse parts)))
        (if (all? digit? (string->list (car parts)))
            (loop (cdr parts))
            (format (join "_" (reverse parts)))))))

  (let ((funstack (list 0)))
    (for (node depth) (make-node-generator n)
      ;; normalize indent
      (match (noderec->t node) with
        (node:function name formals)
        -> (begin
             (if (> depth (car funstack))
                 (push! funstack depth)
                 (while (< depth (car funstack))
                   (pop! funstack)))
             (printf (rpad 20 (format (repeat (length funstack) "  ") (strip-alpha name)))
                     " : " (type-repr* (apply-subst (noderec->type node)) #t) "\n"))
        _ -> #u
        )
      )))
