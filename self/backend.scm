;; -*- Mode: Irken -*-

(include "self/cps.scm")

(define (make-writer file)
  (let ((level 1))
    (define (write-indent)
      (let loop ((n level))
	(match n with
	  0 -> #u
	  n -> (begin (write file.fd "  ") (loop (- n 1)))
	  )))
    (define (write-string s)
      (write-indent)
      (write file.fd s)
      (write file.fd "\n")
      #u)
    (define (indent) (set! level (+ level 1)))
    (define (dedent) (set! level (- level 1)))
    {write=write-string indent=indent dedent=dedent}
    ))

(define (make-name-frobber)
  (define safe-name-map
    (literal
     (alist/make
      (#\! "_bang")
      (#\* "_splat")
      (#\? "_question")
      (#\- "_")
      (#\+ "_plus")
      (#\% "_percent")
      )))
  (define c-legal? (char-class (string->list "abcdefghijklmnopqrstuvwxyz_0123456789")))
  (define (frob-name name)
    (define (frob)
      (let loop ((i 0) (r '()))
	(if (= i (string-length name))
	    r
	    (let ((ch (string-ref name i)))
	      (loop (+ i 1)
		    (list:cons
		     (if (c-legal? ch)
			 (char->string ch)
			 (match (alist/lookup safe-name-map ch) with
			   (maybe:yes sub) -> sub
			   (maybe:no)      -> (format "_" (hex (char->ascii ch)))))
		     r))))))
    (let ((r (string-concat (reverse (frob)))))
      (if (string=? r "_")
	  ;; special-case
	  "minus"
	  r)))
  frob-name)

(define label-maker
  (let ((counter (make-counter 0)))
    (lambda ()
      (format "L" (int (counter.inc))))))

(define (wrap-in types args)
  (define (wrap type arg)
    (match type with
      (type:base b) -> (match b with
			 'int	 -> (format "unbox(" arg ")")
			 'string -> (format "((pxll_string*)(" arg "))->data")
			 _	 -> arg)
      (type:tvar v) -> arg
      (type:pred name predargs)
      -> (match name with
	   'arrow  -> arg
	   'vector -> arg
	   'symbol -> arg
	   'raw    -> (match predargs with
			((type:base 'string)) -> (format "((pxll_string*)(" arg "))")
			_ -> (error1 "unknown raw type in %cexp" type))
	   _ -> (error1 "unexpected predicate in cexp type sig" type))))
  (map2 wrap types args))

(define (wrap-out type exp)
  (match type with
    (type:base 'int)	-> (format "box(" exp ")")
    (type:pred 'bool _) -> (format "PXLL_TEST(" exp ")")
    _			-> exp
    ))

;; substitute <values> into <template>, e.g. "%0 + %1" ("unbox(r3)" "unbox(r5)") => "r3
(define (cexp-subst template values)
  (let ((split (string-split template #\%)))
    (let loop ((r (LIST (car split)))
	       (l (cdr split)))
      (match l with
	;; wouldn't it be cool to generalize pattern matching to strings somehow?
	()	    -> (string-concat (reverse r))
	("")	    -> (error1 "malformed cexp template string" template) ;; template should not end with %
	("" x . tl) -> (loop (prepend x "%" r) tl) ;; %% causes this
	(x  . tl)   -> (match (alist/lookup dec-map (string-ref x 0)) with
			  (maybe:no)	-> (error1 "malformed cexp template string" template)
			  (maybe:yes n) -> (loop (prepend (substring x 1 (string-length x))
							  (nth values n)
							  r)
						 tl))))))

(define emitk o (cont:k _ _ k) -> (emit o k))

(define emit
  o (insn:return target)	    -> (o.write (format "PXLL_RETURN(" (int target) ");"))
  o (insn:literal lit k)	    -> (begin (emit-literal o lit (k/target k)) (emitk o k))
  o (insn:test reg k0 k1 k)	    -> (begin (emit-test o reg k0 k1) (emitk o k))
  o (insn:jump reg k)		    -> (begin (emit-jump o reg (k/target k)))
  o (insn:cexp sig template args k) -> (begin (emit-cexp o sig template args (k/target k)) (emitk o k))
  o (insn:close name body k)	    -> (begin (emit-close o name body k) (emitk o k))
  o (insn:varref d i k)		    -> (begin (emit-varref o d i (k/target k)) (emitk o k))
  o (insn:varset d i v k)	    -> (begin (emit-varset o d i v) (emitk o k))
  o (insn:new-env size k)	    -> (begin (emit-new-env o size (k/target k)) (emitk o k))
  o (insn:store off arg tup i k)    -> (begin (emit-store o off arg tup i) (emitk o k))
  o (insn:invoke fun args k)	    -> (begin (emit-call o fun args k) (emitk o k))
  o (insn:tail fun args k)	    -> (begin (emit-tail o fun args) (emitk o k))
  o x -> (error1 "NYI " x) ;; XXX remove me
  )

(define (emit-literal o lit target)
  (let ((val
	 (match lit with
	   (literal:int n)   -> (tag n)
	   (literal:bool b)  -> (if b #x106 #x006)
	   (literal:char ch) -> (logior 2 (<< (char->ascii ch) 8))
	   (literal:undef)   -> #x0e
	   _ -> (error1 "NYI " lit))))
    (if (= target -1)
	(o.write "// ")) ;; why bother with a dead literal?
    (o.write (format "r" (int target) " = (object *) " (int val) ";"))
    ))

(define (emit-test o reg k0 k1)
  (o.write (format "if PXLL_IS_TRUE(r" (int reg)") {"))
  (o.indent)
  (emit o k0)
  (o.dedent)
  (o.write "} else {")
  (o.indent)
  (emit o k1)
  (o.dedent)
  (o.write "}"))

(define (emit-jump o reg target)
  (if (>= reg 0)
      (o.write (format "r" (int target) "=r" (int reg) ";"))))

(define (emit-cexp o sig template args target)
  (let ((result-type sig)
	(arg-types '()))
    (match sig with
      (type:pred 'arrow (rtype . atypes))
      -> (begin (set! result-type rtype) (set! arg-types atypes))
      _ -> #u)
    (let ((args (map (lambda (reg) (format "r" (int reg))) args))
	  (args (wrap-in arg-types args))
	  (exp (wrap-out result-type (cexp-subst template args))))
      (if (= target -1)
	  (o.write (format exp ";"))
	  (o.write (format "r" (int target) " = " exp))))))

(define frob-name (make-name-frobber))

(define (gen-function-label sym)
  (format "FUN_" (frob-name (symbol->string sym))))  

(define (emit-close o name body k)
  (let ((proc-label (gen-function-label name))
	(jump-label (label-maker))
	(target (k/target k))
	)
    ;; emit a jump over the function definition
    (o.write (format "// def " (sym name)))
    (o.write (format "goto " jump-label ";"))
    ;; emit the function definition
    (o.write (format proc-label ":"))
    (o.indent)
    (emit o body)
    (o.dedent)
    (o.write (format jump-label ":"))
    (o.write (format "r" (int target) " = allocate (TC_CLOSURE, 2);"))
    (o.write (format "r" (int target) "[1] = &&" proc-label "; r" (int target) " = lenv;"))
    ))

(define (emit-varref o d i target)
  (if (>= target 0)
      (o.write (format "r" (int target) " = varref (" (int d) ", " (int i) ");"))))

(define (emit-varset o d i v)
  (o.write (format "varset (" (int d) ", " (int i) ", " (int v) ");")))

(define (emit-new-env o size target)
  (o.write (format "r" (int target) " = allocate (TC_TUPLE, " (int (+ size 1)) ");")))

(define (emit-store o off arg tup i)
  (o.write (format "r" (int tup) "[" (int (+ 1 (+ i off))) "] = r" (int arg) ";")))

(define (emit-tail o fun args)
  (let ((goto (format "goto *r" (int fun) ";")))
    (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto))))

(define (emit-call o fun args k)
  (let ((free (sort < (k/free k))) ;; sorting these might improve things
	(return-label (label-maker))
	(nregs (length free))
	(target (k/target k)))
    ;; save
    (o.write (format "t = allocate (TC_SAVE, " (int (+ 3 nregs)) ");"))
    (let ((saves
	   (let loop ((i nregs) (l '()))
	     (if (= i 0)
		 (string-join l " ")
		 (loop (- i 1) (list:cons (format "t[" (int (+ i 4)) "] = r" (int (nth free i))) l))))))
      (o.write (format "t[1] = k; t[2] = lenv; t[3] = &&" return-label "; " saves " k = t;")))
    ;; call
    (let ((goto (format "goto *r" (int fun) "[1]")))
      (if (>= args 0)
	  (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto ";"))
	  (o.write (format "lenv = r" (int fun) "[2]; " goto ";"))))
    ;; label
    (o.write (format return-label ":"))
    ;; restore
    (let ((restores
	   (let loop ((i nregs) (l '()))
	     (if (= i 0)
		 (string-join l " ")
		 (loop (- i 1) (list:cons (format "r" (int (nth free i)) " = k[" (int (+ i 4)) "];") l))))))
      (o.write (format restores "; lenv = k[2]; k = k[1];")))
    (if (>= target 0)
	(o.write (format "r" (int target) " = result;")))
    ))

(define (tag n)
  (logior 1 (<< n 1)))
    
(define (test)
  (let ((context (make-context))
	(transform (transformer context))
	;;(exp0 (sexp:list (read-string "42")))
	;;(exp0 (sexp:list (read-string "(lambda (x y) (set! y 9) (if #t x y))")))
	;;(exp0 (sexp:list (read-string "((lambda (x y) (%%cexp (int int -> int) \"%0+%1\" x y)) 3 4)")))
	;;(exp0 (sexp:list (read-string "((lambda (x y) x) 3 4)")))
	;;(exp0 (sexp:list (read-string "(if #t 23 94)")))
	;;(exp0 (sexp:list (read-string "(%%cexp (int int -> int) \"%0+%1\" 3 4)")))
	(exp0 (sexp:list (read-string "(%%cexp (int int -> int) \"%0+%1\" ((lambda (x y) (%%cexp (int int -> int) \"%0+%1\" x y)) 3 4) 9)")))
	(exp1 (transform exp0))
	(node0 (walk exp1))
	)
    (print-string "\n-- reader --\n")
    (unread exp0)
    (newline)
    (print-string "\n-- macros --\n")
    (unread exp1)
    (newline)
    (print-string "\n-- node tree --\n")
    (pp-node node0 0) (newline)
    (rename-variables node0)
    (let ((cps (compile #t node0 '() (cont '() gen-return)))
	  (ofile (file/open-stdout))
	  (o (make-writer ofile))
	  )
      (print-string "\n-- RTL --\n")
      (print-insn cps 0)
      (newline)
      (print-string "\n-- C output --\n")
      (emit o cps)
      )
    )
  )
  
(test)
