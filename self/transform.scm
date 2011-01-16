;; -*- Mode: Irken -*-

(include "self/lisp_reader.scm")
(include "self/mbe.scm")
(include "self/types.scm")
(include "self/context.scm")


(define (transformer context)

  (define counter 0)

;;   (define (gensym prefix)
;;     (let ((r (string-concat (LIST prefix (int->string counter)))))
;;       (set! counter (+ counter 1))
;;       (sexp:symbol (string->symbol r))))

  
  (define (go exp)
    (let ((expanded
	   (match exp with
	     (sexp:list ())    -> (sexp:list '())
	     (sexp:list (one)) -> (expand one)
	     (sexp:list exps)  -> (expand-body (find-declarations exps))
	     _ -> (error1 "unexpected s-expression in transformer:" exp)
	     )))
      ;; XXX add-constructors
      expanded
      ))

  (define (expand-body body)
    (sexp:list (list:cons (sexp:symbol 'begin) (map expand body))))

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
  
  (define expand-field
    (field:t name exp) -> (field:t name (expand exp)))

  (define (expand exp)
    ;;(print-string "expanding... ") (printn exp)
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
				     (maybe:yes macro) -> (expand (macro.apply (sexp:list l)))
				     (maybe:no)	       -> (sexp:list (list:cons rator (map expand rands)))))
	   _ -> (sexp:list (map expand l)))))

  (define (sexp1 sym rest)
    ;; build an s-expression with <sym> at the front followed by <rest>
    (sexp:list (list:cons (sexp:symbol sym) rest)))

  (define expand-if
    (tst then)	    -> (sexp1 'if (LIST (expand tst) (expand then) (sexp:undef)))
    (tst then else) -> (sexp1 'if (LIST (expand tst) (expand then) (expand else)))
    x		    -> (error1 "malformed <if>" x)
    )

  (define expand-set!
    ((sexp:attr lhs attr) val)				   -> (sexp1 '%%record-set (LIST (sexp:symbol attr) (expand lhs) (expand val)))
    ((sexp:list ((sexp:symbol '%%array-ref) lhs idx)) val) -> (sexp1 '%%array-set (LIST (expand lhs) (expand val) (expand idx)))
    exp							   -> (sexp:list (list:cons (sexp:symbol 'set!) exp))
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
    (name formals . body) -> (exp-function name formals (sexp1 'begin (map expand body)))
    x			  -> (error1 "malformed FUNCTION" x))

  (define (exp-function name formals body)
    (sexp1 'function (LIST name formals body)))

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

  (define (make-datatype name alts)
    (alist/push context.datatypes name { name = name alts = alts })
    )

  (define parse-datatype
    ((sexp:symbol name) . subs)
    -> (let ((tvars '()))
	 (make-datatype
	  name
	  (map
	   (lambda (sub)
	     (match sub with
	       (sexp:list ((sexp:cons 'nil tag) . types)) -> (:alt tag (map parse-type types))
	       x			                  -> (error1 "malformed alt in datatype" x)))
	   subs)))
    x -> (error1 "malformed datatype" x)
    )

  (define transform-table
    (literal
     (alist/make
      ('if expand-if)
      ('set! expand-set!)
      ('begin expand-begin)
      ('lambda expand-lambda)
      ('function expand-function)
      )))

  go

  )

(define (print-datatype dt)
  (print-string "(datatype ")
  (printn dt.name)
  (for-each
   (lambda (alt)
     (match alt with
       (:alt tag types)
       -> (begin (print-string "  (:")
		 (print tag)
		 (print-string " ")
		 (print-sep print-type " " types)
		 (print-string ")")
		 (newline))))
   dt.alts)
  (print-string "  )\n")
  )

(define (test-transform)
  (let ((context (make-context))
	(transform (transformer context))
	(tl (read-file
	    (if (> sys.argc 1)
		sys.argv[1]
		"lib/core.scm"))))
    (unread (transform (sexp:list tl)))
    (newline)
    (alist/iterate (lambda (name dt) (print-datatype dt)) context.datatypes)
    (alist/iterate (lambda (name macro) (macro.unread)) context.macros)
;;    (printn context)
;;     (for-each printn tl)
;;     (for-each (lambda (x)
;; 		(let ((tx (transform x)))
;; 		  (printn tx)
;; 		  (printn (unread tx))))
;; 	      tl)
    ))

;;(test-transform)
