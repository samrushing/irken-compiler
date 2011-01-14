;; -*- Mode: Irken -*-

(include "self/nodes.scm")

(datatype insn
  (:literal literal cont)               ;; <literal> <k>
  (:return int)                         ;; return register
  (:cexp string (list int) cont)	;; <template> <args> <k>
  (:test int insn insn cont)            ;; <reg> <then> <else> <k>
  (:jump int cont)                      ;; <reg> <k>
  (:close symbol insn cont)             ;; <name> <body> <k>
  (:varref int int cont)                ;; <depth> <index> <k>
  (:varset int int int cont)            ;; <depth> <index> <reg>
  (:new-env int cont)                   ;; <size> <k>
  (:store-tuple int int int int cont)   ;; <offset> <arg> <tuple> <i> <k>
  (:invoke int int cont)                ;; <closure> <args> <k>
  (:invoke-tail int int cont)           ;; <closure> <args> <k>
  )

(datatype cont
  (:k int (list int) insn)
  )

(define (make-register-allocator)
  (let ((max-reg -1))
    (define (allocate free)
      (let loop ((i 0))
	(if (member? i free =)
	    (loop (+ i 1))
	    (begin (set! max-reg (max i max-reg)) i))))
    (define (get-max) max-reg)
    {alloc=allocate get-max=get-max}
    ))

(define the-register-allocator (make-register-allocator))

(define (cont free generator)
  (let ((reg (the-register-allocator.alloc free)))
    (cont:k reg free (generator reg))))

(define (dead free k)
  (cont:k -1 free k))

(define k/free   (cont:k _ free _)   -> free)
(define k/target (cont:k target _ _) -> target)

(define (compile tail? exp lenv k)

  ;; override continuation when in tail position
  (if tail?
      (set! k (cont (k/free k) gen-return)))
  
  (match exp.t with
    (node:literal lit)		 -> (c-literal lit k)
    (node:sequence)		 -> (c-sequence tail? exp.subs lenv k)
    (node:if)			 -> (c-conditional tail? exp lenv k)
    (node:function name formals) -> (c-function name formals (car exp.subs) lenv k)
    (node:varref name)		 -> (c-varref name lenv k)
    (node:varset name)		 -> (c-varset name (car exp.subs) lenv k)
    (node:cexp _ template)       -> (c-primapp template exp.subs lenv k)
    (node:call)			 -> (c-call tail? exp.subs lenv k)
    _ -> (begin (pp-node exp 0) (error1 "NYI" exp))
    )
  )

(define (c-literal val k) (insn:literal val k))

(define (c-sequence tail? nodes lenv k)
  (match nodes with
    ()		 -> (error "empty sequence?")
    (exp)	 -> (compile tail? exp lenv k)
    (exp . exps) -> (compile #f exp lenv (dead (k/free k) (c-sequence tail? exps lenv k)))
    ))

(define (c-conditional tail? exp lenv k)
  (match exp.subs with
    (test then else)
    -> (compile
	#f test lenv
	(cont (k/free k)
	      (lambda (reg)
		(insn:test
		 reg
		 (compile tail? then lenv (cont (k/free k) (lambda (reg) (insn:jump reg k))))
		 (compile tail? else lenv (cont (k/free k) (lambda (reg) (insn:jump reg k))))
		 k))
	      ))
    _ -> (error1 "c-conditional" exp)
    ))

(define extend-lenv
  () lenv -> lenv ;; don't extend with an empty rib
  fs lenv -> (list:cons fs lenv)
  )

(define (c-function name formals body lenv k)
  (insn:close
   name
   (compile #t
	    body
	    (extend-lenv formals lenv)
	    (cont '() gen-return))
   k))

(define search-rib
  name0 _ ()		  -> (maybe:no)
  name0 i (name1 . names) -> (if (eq? name0 name1)
				 (maybe:yes i)
				 (search-rib name0 (+ i 1) names)))

(define lexical-address
  name _ ()           -> (error1 "unbound variable" name)
  name d (rib . lenv) -> (match (search-rib name 0 rib) with
			    (maybe:yes i) -> (:pair d i)
			    (maybe:no)    -> (lexical-address name (+ d 1) lenv)
			    ))

(define (c-varref name lenv k)
  (match (lexical-address name 0 lenv) with
     (:pair depth index) -> (insn:varref depth index k)
     ))

(define (c-varset name exp lenv k)
  (match (lexical-address name 0 lenv) with
     (:pair depth index)
     -> (compile
	 #f exp lenv
	 (cont (k/free k)
	       (lambda (reg)
		 (insn:varset depth index reg k))))))

(define (c-primapp cexp args lenv k)
  (c-primargs cexp args lenv k))

(define (c-primargs cexp args lenv k)
  (collect-primargs args '() lenv k (lambda (regs) (insn:cexp cexp regs k))))

(define (collect-primargs args regs lenv k ck)
  (match args with
    ()        -> (ck regs)
    (hd . tl) -> (compile #f hd lenv
			  (cont (append (k/free k) regs)
				(lambda (reg) (collect-primargs tl (cons reg regs) lenv k ck))))
    ))

(define (c-call tail? subs lenv k)
  (let ((gen-invoke (if tail? gen-invoke-tail gen-invoke)))
    (match subs with
      (fun . args)
      -> (letrec ((make-call
		   (lambda (args-reg)
		     (compile #f fun lenv (cont (cons args-reg (k/free k))
						(lambda (closure-reg) (gen-invoke closure-reg args-reg k)))))))
	   (if (> (length args) 0)
	       (compile-args args lenv (cont (k/free k) make-call))
	       (make-call -1)))
      () -> (error "c-call: no function?")
      )))

(define (compile-args args lenv k)
  (match args with
    () -> (insn:new-env 0 k)
    _  -> (let ((nargs (length args)))
	    (insn:new-env
	     nargs
	     (cont (k/free k)
		   (lambda (tuple-reg)
		     (compile-store-args 0 1 nargs args tuple-reg
					 (cons tuple-reg (k/free k)) lenv k)))))
    ))

(define (compile-store-args i offset nargs args tuple-reg free-regs lenv k)
  (compile
   #f (car args) lenv
   (cont free-regs
	 (lambda (arg-reg)
	   (insn:store-tuple
	    offset arg-reg tuple-reg i
	    (if (< (+ i 1) nargs)
		(dead
		 free-regs
		 (compile-store-args (+ i 1) offset nargs (cdr args) tuple-reg free-regs lenv k))
		k))))
   ))

(define (gen-return reg)
  (insn:return reg))
(define (gen-invoke closure-reg args-reg k)
  (insn:invoke closure-reg args-reg k))
(define (gen-invoke-tail closure-reg args-reg k)
  (insn:invoke-tail closure-reg args-reg k))

(define (print-insn insn d)
  (define (print-line print-info k)
    (match k with
      (cont:k target free k0)
      -> (begin
	   (newline)
	   (indent d)
	   (if (= target -1) (print-string "-") (print target))
	   ;;(print-string " ")
	   ;;(print free)
	   (print-string " ")
	   (print-info)
	   (print-insn k0 d)
	   )))
  (define (ps x) (print x) (print-string " "))
  (match insn with
    (insn:return target)         -> (begin (newline) (indent d) (print-string "- ret ") (print target))
    (insn:literal lit k)         -> (print-line (lambda () (print-string "lit ") (print lit)) k)
    (insn:cexp template args k)  -> (print-line (lambda () (print-string "cexp ") (ps template) (ps args)) k)
    (insn:test reg then else k)  -> (print-line (lambda () (print-string "test ") (print reg) (print-insn then (+ d 1)) (print-insn else (+ d 1))) k)
    (insn:jump reg k)            -> (print-line (lambda () (print-string "jmp ") (print reg)) k)
    (insn:close name body k)     -> (print-line (lambda () (print-string "close ") (print name) (print-insn body (+ d 1))) k)
    (insn:varref d i k)          -> (print-line (lambda () (print-string "ref ") (ps d) (ps i)) k)
    (insn:varset d i v k)        -> (print-line (lambda () (print-string "set ") (ps d) (ps i) (ps v)) k)
    (insn:store-tuple o a t i k) -> (print-line (lambda () (print-string "stor ") (ps o) (ps a) (ps t) (ps i)) k)
    (insn:invoke c a k)          -> (print-line (lambda () (print-string "invoke ") (ps c) (ps a)) k)
    (insn:invoke-tail c a k)     -> (print-line (lambda () (print-string "tail ") (ps c) (ps a)) k)
    (insn:new-env n k)           -> (print-line (lambda () (print-string "env ") (ps n)) k)
    ))


(define (test)
  (let ((context (make-context))
	(transform (transformer context))
	;;(exp0 (sexp:list (read-string "(lambda (x y) (set! y 9) (if #t x y))")))
	(exp0 (sexp:list (read-string "((lambda (x y) (%%cexp (int int -> int) \"%s+%s\" x y)) 3 4)")))
	;;(exp0 (sexp:list (read-string "(if #t 23 94)")))
	;;(exp0 (sexp:list (read-string "(%%cexp (int int -> int) \"%s+%s\" 3 4)")))
	(exp1 (transform exp0))
	(node0 (walk exp1))
	)
    (rename-variables node0)
    (let ((cps (compile #t node0 '() (cont '() gen-return))))
      (print-insn cps 0)
      (newline)
      #t
      )
    )
  )

(test)
