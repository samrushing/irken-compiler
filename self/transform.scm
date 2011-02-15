;; -*- Mode: Irken -*-

(include "self/lisp_reader.scm")
(include "self/mbe.scm")
(include "self/types.scm")
(include "self/context.scm")
(include "self/match.scm")

;; scan for datatypes, definitions, etc..
;; and do transformations that can't be handled by the macro system.

(define (transformer context)

  (define counter 0)

  (define (go exp)
    (print-string "go:")
    (pp 0 exp) (newline)
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
	(sexp:list (LIST (sexp:symbol 'fix)
			 (sexp:list names)
			 (sexp:list inits)
			 body))
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
	   (sexp:list ((sexp:symbol 'datatype) . dtl))
	   -> (begin (parse-datatype dtl) (recur tl acc))
	   (sexp:list ((sexp:symbol 'defmacro) . dtl))
	   -> (begin (parse-defmacro dtl) (recur tl acc))
	   _ -> (recur tl (list:cons hd acc))))
    (recur exps '()))
  
  (define (find-definitions exps k)
    (define recur
      defs exps ()	-> (k (reverse defs) (reverse exps))
      defs exps (hd . tl) -> (match hd with
			       (sexp:list ((sexp:symbol 'define) . body))
			       -> (recur (list:cons (parse-define body) defs) exps tl)
			       exp -> (recur defs (list:cons exp exps) tl)
			       ))
    (recur '() '() exps))

  (define expand-field
    (field:t name exp) -> (field:t name (expand exp)))

  (define (expand exp)
    (print-string "expanding... ") (unread exp) (newline)
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
		(maybe:no)	-> (match (alist/lookup context.macros sym) with
				     (maybe:yes macro) -> (begin (print-string " --- macro-expand --- \n") (expand (macro.apply (sexp:list l))))
				     (maybe:no)	       -> (sexp:list (list:cons rator (map expand rands)))))
	   _ -> (sexp:list (map expand l)))))

  (define expand-if
    (tst then)	    -> (sexp1 'if (LIST (expand tst) (expand then) (sexp:undef)))
    (tst then else) -> (sexp1 'if (LIST (expand tst) (expand then) (expand else)))
    x		    -> (error1 "malformed <if>" x)
    )

  (define expand-set!
    ((sexp:attr lhs attr) val)
    -> (sexp1 '%%record-set (LIST (sexp:symbol attr) (expand lhs) (expand val)))
    ((sexp:list ((sexp:symbol '%%array-ref) lhs idx)) val)
    -> (sexp1 '%%array-set (LIST (expand lhs) (expand val) (expand idx)))
    exp
    -> (sexp:list (list:cons (sexp:symbol 'set!) exp))
    )

  (define expand-begin
    ()	  -> (error "empty BEGIN")
    (one) -> (expand one)
    l     -> (sexp1 'begin (map expand l))
    )

  (define expand-quote
    (one) -> (build-literal one #t #f)
    x     -> (error1 "bad args to QUOTE" x))

  (define expand-literal
    (one) -> (build-literal one #f #f)
    x     -> (error1 "bad args to LITERAL" x))

  (define expand-backquote
    (one) -> (build-literal one #t #t)
    x     -> (error1 "bad args to BACKQUOTE" x))

  (define expand-lambda
    (formals . body) -> (exp-function (sexp:symbol 'lambda) formals (expand (sexp1 'begin body)))
    x		     -> (error1 "malformed LAMBDA" x))

  (define expand-function
    (name formals . body) -> (exp-function name formals (expand (sexp1 'begin body)))
    x			  -> (error1 "malformed FUNCTION" x))

  (define (exp-function name formals body)
    (sexp1 'function (LIST name formals body)))

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
	_ -> (begin (unread (car pairs)) (error1 "split-alts" pairs)))))

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
  
  (define (make-nvget dt label index value)
    (sexp:list (LIST (sexp:symbol '%nvget)
		     (sexp:list (LIST (sexp:cons dt label) (sexp:int index)))
		     value)))

  (define expand-vcase
    ((sexp:symbol dt) value . alts)
    -> (split-alts
	alts
	(lambda (tags formals alts ealt?)
	  (print-string "expand-vcase:\n")
	  (printn tags)
	  (printn formals)
	  (for-each (lambda (x) (unread x) (newline)) alts)
	  (printn ealt?)
	  (let ((alts0
		 (map-range
		     i (length alts)
		     (let ((alt-formals (nth formals i))
			   (tag (nth tags i))
			   (binds (map-range
				      j (length alt-formals)
				      ;; (f0 (%nvget (list:cons 0) value))
				      (sexp:list
				       (LIST (nth alt-formals j)
					     (make-nvget dt tag j value)
					     )))))
		       (if (not (null? binds))
			   ;; (let ((f0 (%nvget (list:cons 0) value))
			   ;;       (f1 (%nvget (list:cons 1) value)))
			   ;;   body)
			   (sexp:list
			    (append (LIST (sexp:symbol 'let) (sexp:list binds))
				    (LIST (nth alts i))))
			   ;; body
			   (nth alts i))))))
	    (for-each (lambda (x) (unread x) (newline)) alts0)
	    (sexp1 '%nvcase
		   (LIST (sexp:symbol dt)
			 (expand value)
			 (sexp:list (map sexp:symbol tags))
			 (sexp:list alts0)
			 (match ealt? with
			   (maybe:no) -> match-error
			   (maybe:yes ealt) -> ealt)))
	  )))
    x -> (error1 "expand-vcase" x))

  (define (build-literal ob as-list? backquote?)
    #u)

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
	 (alist/push context.macros name macro))
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
	(let ((tvars (tvars::values)))
	  (:scheme tvars (pred name tvars))))

      (define (get-tvars)
	(tvars::values))

      (define (get-alt-scheme tag)
	(let ((alt (alt-map::get-err tag "no such alt in datatype")))
	  (print-string "get-alt-scheme: tag=") (printn tag)
	  (print-string "get-alt-scheme: keys=") (printn (tvars::keys))
	  (print-string "get-alt-scheme: values=") (printn (tvars::values))	  
	  (print-string "get-alt-scheme: types=") (printn alt.types)
	  (let ((dtscheme (pred name (tvars::values))))
	    (:scheme (tvars::values) (arrow dtscheme alt.types)))))

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
       context.datatypes)
      (wrap-fix (map sexp:symbol names) constructors body)))

  (define (make-constructor dt tag arity)
    (let ((args (map-range i arity (sexp:symbol (string->symbol (format "arg" (int i)))))))
      (sexp:list (LIST (sexp:symbol 'function)
		       (sexp:symbol (string->symbol (format (sym dt) ":" (sym tag))))
		       (sexp:list args)
		       (sexp:list (list:cons (sexp:cons dt tag) args))))))

  (define (make-alt tvars tag types)
    (let ((types (map (lambda (t) (parse-type* t tvars)) types))
	  (arity (length types)))
      {name=tag
       types=types
       arity=arity
       index=0}))

  (define parse-datatype
    ((sexp:symbol name) . subs)
    -> (let ((tvars (alist-maker))
	     (dt (make-datatype tvars name)))
	 (print-string "parsing datatype " ) (print name) (newline)
	 (for-each
	  (lambda (sub)
	    (print-string "sub=\n") (pp 0 sub) (newline)
	    (match sub with
	      (sexp:list ((sexp:cons 'nil tag) . types)) -> (dt.add (make-alt tvars tag types))
	      x						 -> (error1 "malformed alt in datatype" x)))
	  (reverse subs)) ;; preserve user order of alts
	 (alist/push context.datatypes name dt)
	 )
    x -> (error1 "malformed datatype" x)
    )

  (define parse-define
    ;; (define name ...)
    ((sexp:symbol name) . body)
    -> (if (member? (sexp:symbol '->) body sexp=?)
	   ;; pattern-matching expression
	   (parse-pattern-matching-define name body)
	   ;; normal definition
	   (parse-no-formals-define name body))
    ;; (define (name arg ...) ...)
    ((sexp:list ((sexp:symbol name) . formals)) . body)
    -> (parse-normal-definition name formals body)
    x -> (error1 "malformed <define>" x))

  (define (parse-pattern-matching-define name body)
    (match (compile-pattern context expand body) with
      (:pair vars body0)
      -> (:pair name
		   (sexp:list (LIST (sexp:symbol 'function)
				    (sexp:symbol name)
				    (sexp:list (map sexp:symbol vars))
				    body0)))))

  (define (parse-no-formals-define name body)
    (:pair name (sexp:list body)))

  (define (parse-normal-definition name formals body)
    (:pair name (sexp:list (append (LIST (sexp:symbol 'function)
					 (sexp:symbol name)
					 (sexp:list formals))
				   body))))

  (define transform-table
    (literal
     (alist/make
      ('if expand-if)
      ('set! expand-set!)
      ('begin expand-begin)
      ('lambda expand-lambda)
      ('function expand-function)
      ('vcase expand-vcase)
      )))

  go

  )

(define (print-datatype dt)
  (print-string "(datatype ")
  (printn dt.name)
  (dt.iterate
   (lambda (tag alt)
     (print-string (format "  (:" (sym tag) " " (join type-repr " " alt.types) ")\n"))))
  (print-string "  )\n")
  )

;; (define (test-transform)
;;   (let ((context (make-context))
;; 	(transform (transformer context))
;; 	(tl (sexp:list (read-file sys.argv[1])))
;; 	(exp0 (transform tl)))
;;     (unread exp0)
;;     (newline)
;;     (print-string "repr (exp0) =>\n")
;;     (pp 0 exp0)
;;     (print-string (format "\npp-size=" (int (pp-size exp0)) "actual=" (int (string-length (repr exp0))) "\n"))
;;     (alist/iterate (lambda (name dt) (print-datatype dt)) context.datatypes)
;;     (newline)
;;     (alist/iterate (lambda (name macro) (macro.unread)) context.macros)
;;     (newline)
;;     ))

;; (include "lib/alist2.scm")
;; (test-transform)
