;; -*- Mode: Irken -*-

(include "self/lisp_reader.scm")
(include "self/mbe.scm")
(include "self/types.scm")
(include "self/match.scm")

;; scan for datatypes, definitions, etc..
;; and do transformations that can't be handled by the macro system.

(define (transformer)

  (define counter 0)

  (define (go exp)
    (let ((expanded
	   (match exp with
	     (sexp:list ())    -> (sexp:list '())
	     (sexp:list (one)) -> (expand one)
	     (sexp:list exps)  -> (expand-body (find-declarations exps))
	     _ -> (error1 "unexpected s-expression in transformer:" exp)
	     )))
      (wrap-with-constructors expanded)
      ))

  (define (wrap-fix names inits body)
    (if (> (length names) 0)
	(sexp (sexp:symbol 'fix)
	      (sexp:list names)
	      (sexp:list inits)
	      body)
	body))

  (define (wrap-begin exps)
    (sexp:list (list:cons (sexp:symbol 'begin) exps)))

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
    (find-definitions exps wrap-definitions))

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
		(maybe:yes fun) -> (fun rands)
		(maybe:no)	-> (match (alist/lookup the-context.macros sym) with
				     (maybe:yes macro) -> (expand (macro.apply (sexp:list l) the-context.options.debugmacroexpansion))
				     (maybe:no)	       -> (sexp:list (list:cons rator (map expand rands)))))
	   ;; automagically insert the <self> argument
	   ;; (ob.o.method args0 ...) => (ob.o.method ob args0 ...)
	   ;; XXX use something like __methods__ rather than 'o', duh.
	   (sexp:list ((sexp:symbol '%method) (sexp:symbol name) self))
	   -> (sexp:list (append (LIST (sexp:attr (sexp:attr self 'o) name) self) (map expand rands)))
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

  (define expand-lambda
    (formals . body) -> (exp-function (sexp:symbol 'lambda) formals (sexp:bool #f) (expand-body body))
    x		     -> (error1 "malformed LAMBDA" x))

  (define expand-function
    (name formals type . body) -> (exp-function name formals type (expand-body body))
    x			  -> (error1 "malformed FUNCTION" x))

  (define (exp-function name formals type body)
    (sexp1 'function (LIST name formals type body)))

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
	_ -> (begin (pp 0 (car pairs)) (error1 "split-alts" pairs)))))

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

      { name=name
	get=get
	add=add
	iterate=iterate
	get-nalts=get-nalts
	get-scheme=get-scheme
	get-alt-scheme=get-alt-scheme
	get-tvars=get-tvars }

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

  (define parse-typealias
    ;; for now, allow only predicate -> predicate mapping
    ((sexp:symbol name) (sexp:symbol alias))
    ;; -> (let ((tvars (alist-maker))
    ;; 	        (type (parse-type* alias tvars)))
    ;;      (alist/push the-context.aliases name (:scheme (reverse (tvars::values)) type)))
    -> (alist/push the-context.aliases name alias)
    x -> (error1 "malformed typealias" x))

  (define parse-define
    ;; (define name ...)
    ((sexp:symbol name) . body)
    -> (if (member? (sexp:symbol '->) body sexp=?)
	   ;; pattern-matching expression
	   (parse-pattern-matching-define name body)
	   ;; normal definition
	   (parse-no-formals-define name body))
    ;; (define (%typed (name arg ...) type) ...)
    ((sexp:list ((sexp:symbol '%typed) (sexp:list ((sexp:symbol name) . formals)) type)) . body)
    -> (parse-normal-definition name formals body type)
    ;; (define (name arg ...) ...)
    ((sexp:list ((sexp:symbol name) . formals)) . body)
    -> (parse-normal-definition name formals body (sexp:bool #f))
    x -> (error1 "malformed <define>" x))

  (define (parse-pattern-matching-define name body)
    (match (compile-pattern expand '() body) with
      (:pair vars body0)
      -> (:pair name
		   (sexp (sexp:symbol 'function)
			 (sexp:symbol name)
			 (sexp:list (map sexp:symbol vars))
			 (sexp:bool #f)
			 (expand body0)))))

  (define (parse-no-formals-define name body)
    (if (not (= 1 (length body)))
	(error1 "malformed definition" name)
	(:pair name (car body))))

  (define (parse-normal-definition name formals body type)
    (match type with
      (sexp:bool _) -> #u
      _ -> (begin
	     (print-string (format "user type in define: " (p repr type) "\n"))
	     (print-string (format "  parsed: " (p type-repr (parse-type type)) "\n"))))
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

  (define transform-table
     (alist/make
      ('if expand-if)
      ('set! expand-set!)
      ('begin expand-begin)
      ('lambda expand-lambda)
      ('function expand-function)
      ('vcase expand-vcase)
      ('let-splat expand-let-splat)
      ('match expand-match)
      ('cinclude expand-cinclude)
      ('local-include expand-linclude)
      ('cverbatim expand-cverbatim)
      ('%nvcase expand-%nvcase)
      ('%%cexp expand-%%cexp)
      ))

  go

  )

(define (prepend-standard-macros forms)
  (foldr list:cons forms
	 (find-and-read-file
	  the-context.standard-macros)))

(define (print-datatype dt)
  (print-string "(datatype ")
  (printn dt.name)
  (dt.iterate
   (lambda (tag alt)
     (print-string (format "  (:" (sym tag) " " (join type-repr " " alt.types) ")\n"))))
  (print-string "  )\n")
  )
