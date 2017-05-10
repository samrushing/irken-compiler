;; -*- Mode: Irken -*-

(include "self/lisp_reader.scm")
(include "self/mbe.scm")
(include "self/types.scm")
(include "self/match.scm")

;; scan for datatypes, definitions, etc..
;; and do source-level transformations that can't be handled by the macro system.

(define (transformer)

  (define counter 0)

  (define (go exp)
    (let ((expanded
	   (match (splice exp) with
	     (sexp:list ())    -> (sexp:list '())
	     (sexp:list (one)) -> (expand one)
	     (sexp:list exps)  -> (expand-body exps)
	     _ -> (error1 "unexpected s-expression in transformer:" exp)
	     )))
      (wrap-with-constructors (splice expanded))
      ))

  ;; similar to UNQUOTE-SPLICING.
  ;; (x y ... (%splice a b c) z)
  ;; -> (x y ... a b c z)
  
  (define (splice-list forms acc)
    (match forms with
      () -> (reverse acc)
      ((sexp:list ((sexp:symbol '%splice) . forms0)) . forms1)
      -> (splice-list forms1 (foldr cons acc (reverse (map splice forms0))))
      (hd . tl)
      -> (splice-list tl (list:cons (splice hd) acc))
      ))
  
  (define splice
    (sexp:list forms)
    -> (sexp:list (splice-list forms '()))
    x -> x
    )

  (define (wrap-fix names inits body)
    (if (> (length names) 0)
	(sexp (sexp:symbol 'fix)
	      (sexp:list names)
	      (sexp:list inits)
	      body)
	body))

  (define (wrap-begin exps)
    (sexp1 'begin exps))

  (define (wrap-definitions defs exps)
    (let ((names '())
	  (inits '()))
      (for-each
       (lambda (x)
	 (match x with
	   (:pair name init)
	   -> (begin (PUSH names (sexp:symbol name))
		     (PUSH inits (expand init)))))
       defs)
      (wrap-fix (reverse names) (reverse inits) (expand (wrap-begin exps)))))

  (define (expand-body exps)
    (find-definitions (find-declarations (scan-for-meta exps)) wrap-definitions))

  (define (find-declarations exps)
    (define recur
      ()        acc -> (reverse acc)
      (hd . tl) acc
      -> (match hd with
	   (sexp:list ((sexp:symbol 'typealias) . dtl))
	   -> (begin (parse-typealias dtl) (recur tl acc))
	   (sexp:list ((sexp:symbol 'datatype) . dtl))
	   -> (begin (parse-datatype dtl) (recur tl acc))
	   (sexp:list ((sexp:symbol 'defmacro) . dtl))
	   -> (begin (parse-defmacro dtl) (recur tl acc))
	   _ -> (recur tl (list:cons hd acc))))
    (recur exps '()))

  ;; scan for forms that make compile-time decisions about how/what code to include.
  (define (scan-for-meta forms)
    (define loop
      acc () -> (reverse acc)
      acc ((sexp:list ((sexp:symbol '%backend) (sexp:symbol backend) . subs)) . tl)
      -> (if (eq? backend (backend->name the-context.options.backend))
             (loop (foldr cons acc (reverse (scan-for-meta subs))) tl)
             (loop acc tl))
      acc ((sexp:list ((sexp:symbol '%backend) (sexp:list backends) . subs)) . tl)
      -> (if (member? (sexp:symbol (backend->name the-context.options.backend)) backends sexp=?)
             (loop (foldr cons acc (reverse (scan-for-meta subs))) tl)
             (loop acc tl))
      acc ((sexp:list ((sexp:symbol 'include) (sexp:string path))) . tl)
      -> (loop (foldr cons acc (reverse (scan-for-meta (find-and-read-file path)))) tl)
      acc (hd . tl)
      -> (loop (list:cons hd acc) tl)
      )
    (loop '() forms))

  (define (find-definitions exps0 k)
    (define recur
      defs exps ()	-> (k (reverse defs) (reverse exps))
      defs exps (hd . tl) -> (match hd with
			       (sexp:list ((sexp:symbol 'define) . body))
			       -> (recur (list:cons (parse-define body) defs) exps tl)
			       exp -> (recur defs (list:cons exp exps) tl)
			       ))
    (recur '() '() exps0))

  (define expand-field
    (field:t name exp) -> (field:t name (expand exp)))

;;   (define (expand exp)
;;     (print-string (format "expanding: " (repr exp) "\n"))
;;     (let ((r (expand* exp)))
;;       (print-string (format "         = " (repr r) "\n"))
;;       r))

  (define (expand exp)
    (match exp with
      (sexp:symbol _)	   -> exp
      (sexp:string _)	   -> exp
      (sexp:char _)	   -> exp
      (sexp:bool _)	   -> exp
      (sexp:int _)	   -> exp
      (sexp:undef)	   -> exp
      (sexp:list l)	   -> (maybe-expand l)
      (sexp:vector rands)  -> (sexp:vector (map expand rands))
      (sexp:record fields) -> (sexp:record (map expand-field fields))
      (sexp:cons _ _)	   -> exp
      (sexp:attr exp sym)  -> (sexp:attr (expand exp) sym)
      ))

  (define (maybe-expand l)
    (match l with
      () -> (sexp:list '())
      (rator . rands)
      -> (match rator with
	   (sexp:symbol sym)
	   -> (match (alist/lookup transform-table sym) with
		(maybe:yes fun) 
                -> (fun rands)
		(maybe:no)	
                -> (match (alist/lookup the-context.macros sym) with
                     (maybe:yes macro) -> (expand (macro.apply (sexp:list l) the-context.options.debugmacroexpansion))
                     (maybe:no)	       -> (sexp:list (list:cons rator (map expand rands)))))
	   ;; automagically insert the <self> argument
	   ;; (ob.o.method args0 ...) => (ob.o.method ob args0 ...)
	   ;; XXX use something like __methods__ rather than 'o', duh.
	   (sexp:list ((sexp:symbol '%method) (sexp:symbol name) ob))
	   -> (sexp:list (append (LIST (sexp:attr (sexp:attr ob 'o) name) (sexp:attr ob 'self)) (map expand rands)))
	   _ -> (sexp:list (map expand l)))))

  (define expand-if
    (tst then)	    -> (sexp1 'if (LIST (expand tst) (expand then) (sexp:undef)))
    (tst then else) -> (sexp1 'if (LIST (expand tst) (expand then) (expand else)))
    x		    -> (error1 "malformed <if>" x)
    )

  (define expand-set!
    (lhs0 rhs0)
    -> (let ((lhs (expand lhs0)) ;; early expansion of lhs allows macros to expand to <attr> and <array-ref>
	     (rhs (expand rhs0)))
	 (match lhs with
	   (sexp:attr lhs attr)
	   -> (sexp1 '%rset (LIST (sexp:symbol attr) lhs rhs))
	   (sexp:list ((sexp:symbol '%array-ref) param lhs idx))
	   -> (sexp1 '%array-set (LIST param lhs idx rhs))
	   (sexp:symbol name)
	   -> (sexp1 'set! (LIST (sexp:symbol name) rhs))
	   x -> (error1 "malformed set!" x)
	   ))
    x -> (error1 "malformed set!" x))

  (define expand-begin
    ()	  -> (error "empty BEGIN")
    (one) -> (expand one)
    l     -> (sexp1 'begin (map expand l))
    )

  (define expand-let-splat
    ((sexp:list bindings) . body)
    -> (let ((bindings0
	      (map
	       (lambda (pair)
		 (match pair with
		   (sexp:list (var val))
		   -> (sexp var (expand val))
		   _ -> (error1 "malformed binding in LET-SPLAT" pair)))
	       bindings)))
	 (sexp1 'let-splat (LIST (sexp:list bindings0) (expand-body body))))
    x -> (error1 "malformed LET-SPLAT" x))

  ;; avoid macro-expanding let-subst bindings
  (define expand-let-subst
    (pair . body) -> (sexp1 'let-subst (LIST pair (expand-body body)))
    x -> (error1 "malformed LET-SUBST" x)
    )

  (define expand-lambda
    (formals . body) -> (exp-function (sexp:symbol 'lambda) formals (sexp:bool #f) (expand-body body))
    x		     -> (error1 "malformed LAMBDA" x))

  (define expand-function
    (name formals type . body) -> (exp-function name formals type (expand-body body))
    x			  -> (error1 "malformed FUNCTION" x))

  ;; --- handy sugar ---
  ;; (lambda (:pvar x y ...) body)
  ;; =>
  ;; (lambda (m) (vcase m ((:pvar x y ...) body)))

  (define (expand-pvar-lambda type pvar formals body)
    (let ((msym (sexp:symbol (new-match-var))))
      (expand 
       (sexp (sexp:symbol 'function)
	     (sexp:symbol 'lambda)
	     (sexp:list (LIST msym))
	     type
	     (sexp:list 
	      (LIST (sexp:symbol 'vcase)
		    msym
		    (sexp:list (LIST formals body))
		    ))
	     ))))

  (define (exp-function name formals type body)
    (match formals with
      (sexp:list ((sexp:cons 'nil pvar) . _))
      -> (expand-pvar-lambda type pvar formals body)
      _ -> (sexp1 'function (LIST name formals type body))
      ))

  ;; collect tag/alt pairs from vcase
  (define (split-alts pairs k)
    (let loop ((tags '())
	       (formals '())
	       (alts '())
	       (pairs pairs)
	       )
      (match pairs with
	() -> (k tags formals alts (maybe:no))
	;; (((:tag f0 f1 ...) body ...) ...)
	((sexp:list ((sexp:list ((sexp:cons _ tag) . args)) . code)) . pairs)
	 -> (loop (list:cons tag tags)
		  (list:cons args formals)
		  (list:cons (expand (sexp1 'begin code)) alts)
		  pairs)
	;; ((else body ...))
	((sexp:list ((sexp:symbol 'else) . else-code)))
	-> (k tags formals alts (maybe:yes (expand (sexp1 'begin else-code))))
	_ -> (begin (pp (car pairs) 80) (error1 "split-alts" pairs)))))

  ;; (nvcase type x
  ;;    ((<select0> <formal0> <formal1> ...) <body0>)
  ;;    ((<select1> <formal0> <formal1> ...) <body1>)
  ;;    ...
  ;;    [(else <body2>)]
  ;;    )
  ;; =>
  ;; (nvcase type x
  ;;    ((let ((f0 x.0) (f1 x.1) (f2 x.2)) <body0>) ...))
  ;;

  (define (make-nvget dt label index arity value)
    (sexp (sexp:symbol '%nvget)
	  (sexp (sexp:cons dt label) (sexp:int index) (sexp:int arity))
	  (sexp:symbol value)))

  (define expand-vcase
    ;; nvcase := (vcase <datatype> <value> . alts)
    ;; pvcase := (vcase <value> . alts)
    ((sexp:symbol dt) (sexp:symbol value) . alts) -> (expand-vcase* dt value alts)
    ((sexp:symbol value) . alts)		  -> (expand-vcase* 'nil value alts)
    x -> (error1 "expand-vcase" x))

  (define (expand-vcase* dt value alts)
    (split-alts
     alts
     (lambda (tags formals alts ealt?)
       (let ((arities '())
	     (alts0 '()))
	 (for-range
	     i (length alts)
	     (let ((alt-formals (nth formals i))
		   (arity (length alt-formals))
		   (tag (nth tags i))
		   (binds (let loop ((j 0)
				     (r '()))
			    (if (= j arity)
				(reverse r)
				(match (nth alt-formals j) with
				  (sexp:symbol '_) -> (loop (+ j 1) r)
				  formal -> (loop (+ j 1)
						  (list:cons
						   (sexp formal (make-nvget dt tag j arity value))
						   r)))))))
	       (PUSH arities arity)
	       (if (not (null? binds))
		   ;; (let ((f0 (%nvget (list:cons 0) value))
		   ;;       (f1 (%nvget (list:cons 1) value)))
		   ;;   body)
		   (PUSH alts0
			 (sexp:list
			  (append (LIST (sexp:symbol 'let) (sexp:list binds))
				  (LIST (nth alts i)))))
		   ;; body
		   (PUSH alts0 (nth alts i)))))
	 (sexp (sexp:symbol '%nvcase)
	       (sexp:symbol dt)
	       (sexp:symbol value)
	       (sexp:list (map sexp:symbol tags))
	       (sexp:list (map sexp:int (reverse arities)))
	       (sexp:list (map expand (reverse alts0)))
	       (match ealt? with
		 (maybe:no) -> match-error
		 (maybe:yes ealt) -> (expand ealt)))
	 ))))

  (define parse-defmacro
    ((sexp:symbol name) . exps)
    -> (let ((macro
		 (make-macro
		  name
		  (let loop ((exps exps))
		    (match exps with
		      () -> '()
		      (in-pat (sexp:symbol '->) out-pat . rest)
		      -> (list:cons (:pair in-pat out-pat) (loop rest))
		      _ -> (error1 "malformed macro definition:" exps))))))
	 (alist/push the-context.macros name macro))
    x -> (error1 "malformed macro definition:" x)
    )

  (define (make-datatype tvars name)

    (let ((alt-map (alist-maker))
	  (nalts 0))

      (define (get tag)
	(alt-map::get-err tag "no such alt in datatype"))

      (define (add alt)
	(alt-map::add alt.name alt)
	(set! alt.index nalts)
	(set! nalts (+ 1 nalts))
	)

      (define (iterate p)
	(alt-map::iterate p))

      (define (get-nalts) nalts)

      (define (get-scheme)
	(let ((tvars (get-tvars)))
	  (:scheme tvars (pred name tvars))))

      ;; XXXXXXX FIX ME XXXXXXXX
      ;;   tvars are reversed!
      ;; XXXXXXX FIX ME XXXXXXXX
      (define (get-tvars)
	(reverse (tvars::values)))

      (define (get-alt-scheme tag)
	(let ((alt (alt-map::get-err tag "no such alt in datatype"))
	      (tvars0 (get-tvars)))
	  ;; ok, this is a mistake: the order of the tvars *matters*!
	  (let ((dtscheme (pred name tvars0))
		(r (:scheme tvars0 (arrow dtscheme alt.types))))
 	    ;; (printf "get-alt-scheme dt=" (sym name) " tag=" (sym tag) " scheme=" (scheme-repr r) "\n")
	    r)))

      (define (to-sexp)
        (let ((tvar-cmap (cmap/make magic-cmp))
              (index 0)
              (r '()))
          (for-list tvar (get-tvars)
            (match tvar with
              (type:tvar id _) -> (cmap/add tvar-cmap id)
              _ -> (impossible)))
          (for-list alt (reverse (alt-map::values))
            (PUSH r (sexp (sym alt.name)
                          (int index)
                          (list (map (lambda (t) (type->sexp* tvar-cmap t)) alt.types))))
            (set! index (+ 1 index)))
          (sexp (sym name)
                (list (reverse r)))))

      { name=name
	get=get
	add=add
	iterate=iterate
	get-nalts=get-nalts
	get-scheme=get-scheme
	get-alt-scheme=get-alt-scheme
	get-tvars=get-tvars
        to-sexp=to-sexp
        }

      ))

  (define (wrap-with-constructors body)
    (let ((names '())
	  (constructors '()))
      (alist/iterate
       (lambda (name dt)
	 (dt.iterate
	  (lambda (tag alt)
	    (PUSH names (string->symbol (format (sym dt.name) ":" (sym tag))))
	    (PUSH constructors (make-constructor dt.name tag alt.arity)))))
       the-context.datatypes)
      (wrap-fix (map sexp:symbol names) constructors body)))

  (define (make-constructor dt tag arity)
    (let ((args (map-range i arity (sexp:symbol (string->symbol (format "arg" (int i)))))))
      (sexp (sexp:symbol 'function)
	    (sexp:symbol (string->symbol (format (sym dt) ":" (sym tag))))
	    (sexp:list args)
	    (sexp:bool #f)
	    (sexp:list (append (LIST (sexp:symbol '%dtcon) (sexp:cons dt tag)) args))
	    )))

  (define (make-alt tvars tag types)
    (let ((types (map (lambda (t) (parse-type* t tvars)) types))
	  (arity (length types)))
      {name=tag
       types=types
       arity=arity
       index=0}))

  (define (add-datatype name dt)
    (match (alist/lookup the-context.datatypes name) with
	   (maybe:yes _) -> (error1 "datatype already defined" name)
	   (maybe:no) -> (alist/push the-context.datatypes name dt)))

  (define parse-datatype
    ((sexp:symbol name) . subs)
    -> (let ((tvars (alist-maker))
	     (dt (make-datatype tvars name)))
	 (for-each
	  (lambda (sub)
	    (match sub with
	      (sexp:list ((sexp:cons 'nil tag) . types)) -> (dt.add (make-alt tvars tag types))
	      x						 -> (error1 "malformed alt in datatype" x)))
	  subs)
	 (add-datatype name dt)
	 )
    x -> (error1 "malformed datatype" x)
    )

  ;; XXX I think this approach is wrong... either
  ;;  1) we need to store these in *unparsed* form so when they are seen in the datatype
  ;;     alts their typevars are seen, or
  ;;  2) we have to bite the bullet and either add a new kind of datatype or extend the
  ;;     current datatype to support non-variant datatypes natively.
  ;;  I think we have to do #2... because otherwise we have no way to specify the typevars
  ;;    correctly, e.g.:
  ;; (define typealias thing1 {x=int y='a})
  ;; (datatype thing2
  ;;    (:one (thing1 int))
  ;;    (:two (thing1 bool)))
  ;; becomes impossible?
  (define parse-typealias
    ((sexp:symbol name) alias)
    -> (let ((tvars (alist-maker))
  	     (type (parse-type* alias tvars)))
         (alist/push the-context.aliases
		     name
		     (:scheme (reverse (tvars::values)) type)))
    ;; only predicate -> predicate mapping
    ;;((sexp:symbol name) (sexp:symbol alias))
    ;;-> (alist/push the-context.aliases name alias)
    x -> (error1 "malformed typealias" x))

  ;; pushes unparsed alias
  ;; (define parse-typealias
  ;;   ((sexp:symbol name) alias) -> (alist/push the-context.aliases name alias)
  ;;   x -> (error1 "malformed typealias" x))

  (define parse-define
    ;; (define name ...)
    ((sexp:symbol name) . body)
    -> (if (member? (sexp:symbol '->) body sexp=?)
	   ;; pattern-matching expression
	   (parse-pattern-matching-define name body (sexp:bool #f))
	   ;; normal definition
	   (parse-no-formals-define name body (maybe:no)))
    ;; (define (%typed name type) ...)
    ((sexp:list ((sexp:symbol '%typed) (sexp:symbol name) type)) . body)
    -> (parse-no-formals-define name body (maybe:yes type))
    ;; (define (%typed (name arg ...) type) ...)
    ((sexp:list ((sexp:symbol '%typed) (sexp:list ((sexp:symbol name) . formals)) type)) . body)
    -> (parse-normal-definition name formals body type)
    ;; (define name : type <body>)
    ;; ((sexp:list ((sexp:symbol '%typed) (sexp:symbol name) type)) . body)
    ;; -> (if (member? (sexp:symbol '->) body sexp=?)
    ;;        (parse-pattern-matching-define name body type)
    ;;        (error1 "type annotations of this form not yet implemented" name))
    ;; (define (name arg ...) ...)
    ((sexp:list ((sexp:symbol name) . formals)) . body)
    -> (parse-normal-definition name formals body (sexp:bool #f))
    x -> (error1 "malformed <define>" x))

  (define (parse-pattern-matching-define name body type)
    (match (compile-pattern expand '() body) with
      (:pair vars body0)
      -> (:pair name
		   (sexp (sexp:symbol 'function)
			 (sexp:symbol name)
			 (sexp:list (map sexp:symbol vars))
                         type
			 (expand body0)))))

  (define parse-no-formals-define
    ;; XXX should we run expand-body on body?
    name (body) (maybe:no)       -> (:pair name body)
    name (body) (maybe:yes type) -> (:pair name (sexp (sexp:symbol '%typed) type body))
    name _ _ -> (error1 "malformed definition" name)
    )

  (define (parse-normal-definition name formals body type)
    ;; (match type with
    ;;   (sexp:bool _) -> #u
    ;;   _ -> (begin
    ;;          (print-string (format "user type in define: " (repr type) "\n"))
    ;;          (print-string (format "  parsed: " (type-repr (parse-type type)) "\n"))))
    (:pair name (sexp (sexp:symbol 'function)
		      (sexp:symbol name)
		      (sexp:list formals)
		      type
		      ;; note: expand-body returns one sexp
		      (expand-body body))))

  (define (expand-match exps)
    (let loop ((vars '())
	       (inits '())
	       (el exps))
      (match el with
	((sexp:symbol 'with) . rules)
	-> (match (compile-pattern expand (reverse vars) rules) with
	     (:pair _ code)
	     -> (expand
		 (if (null? inits)
		     code
		     (sexp (sexp:symbol 'let) (sexp:list inits) code))))
	((sexp:symbol var) . el)
	-> (loop (list:cons var vars) inits el)
	(value . el)
	-> (let ((var (new-match-var)))
	     (loop (list:cons var vars) (list:cons (sexp (sexp:symbol var) value) inits) el))
	_ -> (error1 "malformed match expression" exps))))

  (define expand-cinclude
    ((sexp:string path))
    -> (begin
	 (PUSH the-context.cincludes path)
	 (sexp (sexp:symbol 'begin)))
    x -> (error1 "malformed <cinclude>" x)
    )

  (define expand-linclude
    ((sexp:string path))
    -> (begin
	 (PUSH the-context.lincludes path)
	 (sexp (sexp:symbol 'begin)))
    x -> (error1 "malformed <linclude>" x)
    )

  (define expand-cverbatim
    ((sexp:string path))
    -> (begin
	 (PUSH the-context.cverbatim path)
	 (sexp (sexp:symbol 'begin)))
    x -> (error1 "malformed <cverbatim>" x)
    )

  (define (expand-%nvcase l)
    ;; already expanded... investigate why this happens
    (sexp:list (list:cons (sexp:symbol '%nvcase) l)))

  (define expand-%%cexp
    (sig template . args)
    -> (sexp:list (append (LIST (sexp:symbol '%%cexp) sig template)
			  (map expand args)))
    x -> (error1 "malformed %%cexp" x))

  (define expand-%%ffi
    (name sig . args)
    -> (sexp:list (append (LIST (sexp:symbol '%%ffi) name sig)
			  (map expand args)))
    x -> (error1 "malformed %%ffi" x))

  (define (expand-%%sexp list)
    (sexp:list (cons (sexp:symbol '%%sexp) list)))

  (define backend->name
    (backend:c)        -> 'c
    (backend:llvm)     -> 'llvm
    (backend:bytecode) -> 'bytecode
    )

  (define expand-datatype->sexp
    ((sexp:symbol dtname))
    -> (match (alist/lookup the-context.datatypes dtname) with
         (maybe:yes dt) -> (sexp (sym '%%sexp) (dt.to-sexp))
         (maybe:no)     -> (error1 "unknown datatype" dtname))
    x -> (error1 "malformed datatype->sexp" x))

  (define expand-dtreflect
    ((sexp:symbol operator) (sexp:symbol dtname))
    -> (match (alist/lookup the-context.datatypes dtname) with
         (maybe:yes dt) -> (expand (sexp (sym operator) (dt.to-sexp)))
         (maybe:no)     -> (error1 "unknown datatype" dtname))
    x -> (error1 "malformed dtreflect" x))

  (define transform-table
     (alist/make
      ('if expand-if)
      ('set! expand-set!)
      ('begin expand-begin)
      ('lambda expand-lambda)
      ('function expand-function)
      ('vcase expand-vcase)
      ('let-splat expand-let-splat)
      ('let-subst expand-let-subst)
      ('match expand-match)
      ('cinclude expand-cinclude)
      ('local-include expand-linclude)
      ('cverbatim expand-cverbatim)
      ('%nvcase expand-%nvcase)
      ('%%cexp expand-%%cexp)
      ('%%ffi expand-%%ffi)
      ('%%sexp expand-%%sexp)
      ('datatype->sexp expand-datatype->sexp)
      ('dtreflect expand-dtreflect)
      ))

  go

  )

(define (prepend-standard-macros forms)
  (foldr list:cons forms
	 (find-and-read-file
	  the-context.standard-macros)))

(define (print-datatype dt)
  (printf "(forall (" (join type-repr " " (dt.get-tvars)) ")\n")
  (printf "  (datatype " (sym dt.name) "\n")
  (dt.iterate
   (lambda (tag alt)
     (printf "    (:" (sym tag) " " (join type-repr " " alt.types) ")\n")))
  (printf "    )\n")
  (printf "  )\n")
  )
