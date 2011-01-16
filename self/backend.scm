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

(define emitk o (cont:k _ _ k) -> (emit o k))

(define emit
  o (insn:return target)	 -> (o.write (format "PXLL_RETURN(" (int target) ");"))
  o (insn:literal lit k)	 -> (begin (emit-literal o lit (k/target k)) (emitk o k))
  o (insn:test reg k0 k1 k)	 -> (begin (emit-test o reg k0 k1) (emitk o k))
  o (insn:jump reg k)		 -> (begin (emit-jump o reg (k/target k)))
;;  o (insn:cexp template args k)	 -> (begin (emit-cexp o template args (k/target k)) (emitk o k))
  o (insn:close name body k)	 -> (begin (emit-close o name body k) (emitk o k))
  o (insn:varref d i k)		 -> (begin (emit-varref o d i (k/target k)) (emitk o k))
  o (insn:varset d i v k)	 -> (begin (emit-varset o d i v) (emitk o k))
  o (insn:new-env size k)	 -> (begin (emit-new-env o size (k/target k)) (emitk o k))
  o (insn:store off arg tup i k) -> (begin (emit-store o off arg tup i) (emitk o k))
;;  o (insn:invoke fun args k)     -> (begin (emit-call o fun args) (emitk o k))
  o (insn:tail fun args k)       -> (begin (emit-tail o fun args) (emitk o k))
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

(define (tag n)
  (logior 1 (<< n 1)))
    
(define (test)
  (let ((context (make-context))
	(transform (transformer context))
	;;(exp0 (sexp:list (read-string "42")))
	;;(exp0 (sexp:list (read-string "(lambda (x y) (set! y 9) (if #t x y))")))
	;;(exp0 (sexp:list (read-string "((lambda (x y) (%%cexp (int int -> int) \"%s+%s\" x y)) 3 4)")))
	(exp0 (sexp:list (read-string "((lambda (x y) x) 3 4)")))
	;;(exp0 (sexp:list (read-string "(if #t 23 94)")))
	;;(exp0 (sexp:list (read-string "(%%cexp (int int -> int) \"%s+%s\" 3 4)")))
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
