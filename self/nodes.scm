;; -*- Mode: Irken -*-

(include "self/transform.scm")

(datatype literal
  (:string string)
  (:int int)
  (:char char)
  (:bool bool)
  (:undef)
  ;; vectors, records, etc...
  )

;; node type holds metadata related to the node,
;;  but sub-nodes are held with the record.
(datatype node
  (:varref symbol)
  (:varset symbol)
  (:literal literal)
  (:cexp (list type) type string) ;; generic-tvars type template
  (:nvcase symbol (list symbol))  ;; datatype alts
  (:sequence)
  (:if)
  (:function symbol (list symbol)) ;; name formals
  (:call)
  (:let (list symbol))
  (:fix (list symbol))
  (:subst symbol symbol)
  (:primapp symbol sexp) ;; name params
  )

(define node-counter (make-counter 0))

;; given a list of nodes, add up their sizes (+1)
(define (sum-size l)
  (fold (lambda (n acc) (+ n.s acc)) 1 l))

(define no-type (pred '? '()))

(define no-type?
  (type:pred '? () _) -> #t
  _ -> #f
  )

(define (node t subs)
  {t=t subs=subs s=(sum-size subs) id=(node-counter.inc) type=no-type}
  )

(define (node/varref name)
  (node (node:varref name) '()))

(define (node/varset name val)
  (node (node:varset name) (LIST val)))

(define (node/literal lit)
  (node (node:literal lit) '()))

(define (node/cexp gens type template args)
  (node (node:cexp gens type template) args))

(define (node/sequence subs)
  (node (node:sequence) subs))

(define (node/if test then else)
  (let ((nodes (LIST test then else)))
    (node (node:if) nodes)))

(define (node/function name formals body)
  (node (node:function name formals) (LIST body)))

(define (node/call rator rands)
  (let ((subs (list:cons rator rands)))
    (node (node:call) subs)))

(define (node/fix names inits body)
  (let ((subs (append inits (LIST body))))
    (node (node:fix names) subs)))

(define (node/let names inits body)
  (let ((subs (append inits (LIST body))))
    (node (node:let names) subs)))

(define (node/nvcase dt tags value alts else)
  (let ((subs (list:cons value (list:cons else alts))))
    (node (node:nvcase dt tags) subs)))

(define (node/subst from to body)
  (node (node:subst from to) (LIST body)))

(define (node/primapp name params args)
  (node (node:primapp name params) args))

(define literal->string
  (literal:string s) -> (format (char #\") s (char #\"))
  (literal:int n)    -> (format (int n))
  (literal:char ch)  -> (format (char #\#) (char #\\) (char ch)) ;; printable?
  (literal:bool b)   -> (format (bool b))
  (literal:undef)    -> (format "#u")
  )

(define (pp-node n d)
  (define PS print-string)
  (let ((tr (type-repr n.type)))
    (newline)
    (indent d)
    (PS (format (int n.s) " "))
    (match n.t with
      (node:varref name)	     -> (PS (format "varref " (sym name) " : " tr))
      (node:varset name)	     -> (PS (format "varset " (sym name) " : " tr))
      (node:literal lit)	     -> (PS (format "literal " (p literal->string lit) " : " tr))
      (node:cexp gens type template) -> (PS (format "cexp " template " : " tr))
      (node:sequence)		     -> (PS (format "sequence : " tr))
      (node:if)			     -> (PS (format "conditional : " tr))
      (node:call)		     -> (PS (format "call : " tr))
      (node:function name formals)   -> (PS (format "function " (sym name) " (" (join symbol->string " " formals) ") : " tr))
      (node:fix formals)	     -> (PS (format "fix (" (join symbol->string " " formals) ") : " tr))
      (node:nvcase dt tags)	     -> (PS (format "nvcase " (sym dt) "(" (join symbol->string " " tags) ") : " tr))
      (node:subst from to)	     -> (PS (format "subst " (sym from) "->" (sym to)))
      (node:primapp name params)     -> (PS (format "primapp " (sym name) " " (p repr params) " : " tr))
      (node:let formals)	     -> (PS (format "let (" (join symbol->string " " formals) ") : " tr))
      )
    (pp-nodes n.subs (+ 1 d))
    ))

(define (pp-nodes l d)
  (for-each (lambda (n) (pp-node n d)) l))

(define (get-formals l)
  (define p
    (sexp:symbol formal) acc -> (list:cons formal acc)
    _			 acc -> (error1 "malformed formal" l))
  (reverse (fold p '() l)))

(define (unpack-bindings bindings)
  (let loop ((l bindings)
	     (names '())
	     (inits '()))
    (match l with
      () -> (:pair (reverse names) (reverse inits))
      ((sexp:list ((sexp:symbol name) init)) . l)
      -> (loop l (list:cons name names) (list:cons init inits))
      _ -> (error1 "unpack-bindings" l)
      )))

(define (parse-cexp-sig sig)
  (let ((generic-tvars (alist-maker))
	(result (parse-type* sig generic-tvars)))
    (:scheme (generic-tvars::values) result)))

(define walk*
  (sexp:symbol s) -> (node/varref s)
  (sexp:string s) -> (node/literal (literal:string s))
  (sexp:int n)    -> (node/literal (literal:int n))
  (sexp:char c)   -> (node/literal (literal:char c))
  (sexp:bool b)   -> (node/literal (literal:bool b))
  (sexp:undef)    -> (node/literal (literal:undef))
  ;; (:vector (list sexp))
  ;; (:record (list field))
  ;; (:cons symbol symbol)
  ;; (:attr sexp symbol)
  (sexp:list l)
  -> (match l with
       ((sexp:symbol '%%cexp) sig (sexp:string template) . args)
       -> (let ((scheme (parse-cexp-sig sig)))
	    (match scheme with
	      (:scheme gens type)
	      -> (node/cexp gens type template (map walk args))))
       ((sexp:symbol '%nvcase) (sexp:symbol dt) val-exp (sexp:list tags) (sexp:list alts) ealt)
       -> (node/nvcase dt (map sexp->symbol tags) (walk val-exp) (map walk alts) (walk ealt))
       ((sexp:symbol 'begin) . exps)
       -> (node/sequence (map walk exps))
       ((sexp:symbol 'set!) (sexp:symbol name) arg)
       -> (node/varset name (walk arg))
       ((sexp:symbol 'quote) arg)
       -> (let ((lit (walk arg)))
	    (match lit.t with
	      (node:literal _) -> lit
	      _ -> (error1 "expected literal type" l)))
       ((sexp:symbol 'if) test then else)
       -> (node/if (walk test) (walk then) (walk else))
       ((sexp:symbol 'function) (sexp:symbol name) (sexp:list formals) body)
       -> (node/function name (get-formals formals) (walk body))
       ((sexp:symbol 'fix) (sexp:list names) (sexp:list inits) body)
       -> (node/fix (get-formals names) (map walk inits) (walk body))
       ((sexp:symbol 'let) (sexp:list bindings) body)
       -> (match (unpack-bindings bindings) with
	    (:pair names inits)
	    -> (node/let names (map walk inits) (walk body)))
       ((sexp:symbol 'letrec) (sexp:list bindings) body)
       -> (match (unpack-bindings bindings) with
	    (:pair names inits)
	    -> (node/fix names (map walk inits) (walk body)))
       ((sexp:symbol 'let_subst) (sexp:list ((sexp:symbol from) (sexp:symbol to))) body)
       -> (node/subst from to (walk body))
       (rator . rands)
       -> (match rator with
	    (sexp:symbol name)
	    -> (if (eq? (string-ref (symbol->string name) 0) #\%)
		   (node/primapp name (car rands) (map walk (cdr rands)))
		   (node/call (walk rator) (map walk rands)))
	    (sexp:cons dt alt)
	    -> (node/primapp '%dtcon rator (map walk rands))
	    _ -> (node/call (walk rator) (map walk rands)))
       _ -> (error1 "syntax error: " l)
       )
  x -> (error1 "syntax error 2: " x)
  )

(define (walk exp)
  (print-string "walking... ") (unread exp) (newline)
  (walk* exp))

(define (frob name num)
  (string->symbol (format (sym name) "_" (int num))))

(define (make-vardef name serial)
  (let ((frobbed (frob name serial)))
    {name=name name2=frobbed assigns='() refs='() serial=serial }
    ))

(define (make-var-map)
  (let ((map (tree:empty))
	(counter (make-counter 0)))
    (define (add sym)
      (let ((vd (make-vardef sym (counter.inc))))
	(set! map (tree/insert map symbol<? sym vd))
	vd))
    (define (lookup sym)
      (tree/member map symbol<? sym))
    (define (get) map)
    {add=add lookup=lookup get=get}
    ))

(define (rename-variables n)

  (let ((varmap (make-var-map)))

    (define (rename-all exps lenv)
      (for-each (lambda (exp) (rename exp lenv)) exps))
    
    (define (rename exp lenv)

      (define (lookup name)
	(let loop0 ((lenv lenv))
	  (match lenv with
	    ()		 -> (maybe:no)
	    (rib . next) -> (let loop1 ((l rib))
			      (match l with
				()	  -> (loop0 next)
				(vd . tl) -> (if (eq? name vd.name)
						 (maybe:yes vd)
						 (loop1 tl)))))))
      
      (match exp.t with
	(node:function name formals)
	-> (let ((rib (map varmap.add formals)))
	     (set! exp.t (node:function name (map (lambda (x) x.name2) rib)))
	     (rename-all exp.subs (list:cons rib lenv)))
	(node:varref name)
	-> (match (lookup name) with
	     (maybe:no) -> #u ;; can't rename it if we don't know what it is
	     (maybe:yes vd) -> (set! exp.t (node:varref vd.name2)))
	(node:varset name)
	-> (match (lookup name) with
	     (maybe:no) -> #u
	     (maybe:yes vd) -> (set! exp.t (node:varset vd.name2)))
	_ -> (rename-all exp.subs lenv)
	))

    (rename n '())
    ))

;; walk the node tree, applying subst nodes
(define (apply-substs exp)
  
  ;; could we do this more easily by flattening the environment and just using member?
  (define shadow
    names ()		-> (list:nil)
    names (pair . tail) -> (if (not (member-eq? pair.from names))
			       (list:cons pair (shadow names tail))
			       (shadow names tail)))

  (define lookup
    name ()	       -> name
    name (pair . tail) -> (if (eq? name pair.from)
			      pair.to
			      (lookup name tail)))

  (define (walk exp lenv)
    (let/cc return
	(match exp.t with
	  (node:fix formals)   -> (set! lenv (shadow formals lenv))
	  (node:let formals)   -> (set! lenv (shadow formals lenv))
	  ;; other binding expressions!
	  (node:subst from to) -> (begin
				    (print-string "      added entry\n")
				    (set! lenv (list:cons {from=from to=(lookup to lenv)} lenv))
				    (return (walk (car exp.subs) lenv)))
	  (node:varref name)   -> (set! exp.t (node:varref (lookup name lenv)))
	  (node:varset name)   -> (set! exp.t (node:varset (lookup name lenv)))
	  _ -> #u
	  )
      (set! exp.subs (map (lambda (x) (walk x lenv)) exp.subs))
      exp))

  (walk exp '())
  )

(define (test-nodes)
  (let ((context (make-context))
	(transform (transformer context))
	(exp0 (sexp:list (read-string "((lambda (x y) (+ x y)) 3 4)")))
	(exp1 (transform exp0))
	(node0 (walk exp1))
	(node1 (apply-substs node0))
	)
    (unread exp0) (newline)
    (unread exp1) (newline)
    (printn (walk exp1))
    (pp-node node1 0) (newline)
    (rename-variables node0)
    (pp-node node1 0) (newline)
    )
  )

;;(test-nodes)
