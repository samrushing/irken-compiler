;; -*- Mode: Irken -*-

(include "self/transform.scm")
(include "lib/counter.scm")

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
  (:cexp type string)
;  (:nvcase symbol (list symbol))
  (:sequence)
  (:if)
  (:function symbol (list symbol)) ;; name formals
  (:call)
  )

;; given a list of nodes, add up their sizes (+1)
(define (sum-size l)
  (fold (lambda (n acc) (+ n.s acc)) 1 l))

(define (node/varref name)
  {t=(node:varref name) subs='() s=1})

(define (node/varset name val)
  {t=(node:varset name) subs=(LIST val) s=(+ 1 val.s)})

(define (node/literal lit)
  {t=(node:literal lit) subs='() s=1})

(define (node/cexp type template args)
  {t=(node:cexp type template) subs=args s=(sum-size args)})

(define (node/sequence subs)
  {t=(node:sequence) subs=subs s=(sum-size subs)})

(define (node/if test then else)
  (let ((nodes (LIST test then else)))
    {t=(node:if) subs=nodes s=(sum-size nodes)}))

(define (node/function name formals body)
  {t=(node:function name formals) subs=(LIST body) s=(+ 1 body.s)})

(define (node/call rator rands)
  (let ((subs (list:cons rator rands)))
    {t=(node:call) subs=subs s=(sum-size subs)}))

(define indent
  0 -> #t
  n -> (begin (print-string "  ") (indent (- n 1))))

(define (pp-nodes l d)
  (for-each (lambda (n) (pp-node n d)) l))

(define (pp-node n d)
  (define PS print-string)
  (newline)
  (indent d)
  (print n.s)
  (PS " ")
  (match n.t with
    (node:varref name)		 -> (begin (PS "varref ") (print name))
    (node:varset name)		 -> (begin (PS "varset ") (print name) (pp-nodes n.subs (+ 1 d)))
    (node:literal lit)		 -> (begin (PS "literal ") (print lit))
    (node:cexp type template)    -> (begin (PS "cexp ") (PS template) (PS " ") (print-type type) (pp-nodes n.subs (+ 1 d)))
    (node:sequence)		 -> (begin (PS "sequence") (pp-nodes n.subs (+ 1 d)))
    (node:if)			 -> (begin (PS "conditional") (pp-nodes n.subs (+ 1 d)))
    (node:function name formals) -> (begin (PS "function ") (print name) (PS " ") (print formals) (pp-nodes n.subs (+ 1 d)))
    (node:call)			 -> (begin (PS "call") (pp-nodes n.subs (+ 1 d)))
    )
  )

(define (get-formals l)
  (define p
    (sexp:symbol formal) acc -> (list:cons formal acc)
    _			 acc -> (error1 "malformed formal" l))
  (reverse (fold p '() l)))

(define walk
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
       ((sexp:symbol '%%cexp) type (sexp:string template) . args)
       -> (node/cexp (parse-type type) template (map walk args))

;;       ((sexp:symbol '%nvcase) (sexp:symbol datatype) value (sexp:list alts) ealt)
;;       -> (match (split-alts alts) with
;;	     (:pair tags exps) -> (node:nvcase datatype (walk value) tags (map walk exps) (walk ealt)))

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

       (rator . rands)
       -> (node/call (walk rator) (map walk rands))

       _ -> (error1 "syntax error: " l)
       )
  x -> (error1 "syntax error 2: " x)
  )

(define (frob sym num)
  (string->symbol (string-append (symbol->string sym) "_" (int->string num))))

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
    (printn (varmap.get))
    ))

(define (test-nodes)
  (let ((context (make-context))
	(transform (transformer context))
	(exp0 (sexp:list (read-string "((lambda (x y) (+ x y)) 3 4)")))
	(exp1 (transform exp0))
	(node0 (walk exp1))
	)
    (unread exp0) (newline)
    (unread exp1) (newline)
    (printn (walk exp1))
    (pp-node node0 0) (newline)
    (rename-variables node0)
    (pp-node node0 0) (newline)
    )
  )

;;(test-nodes)
