;; -*- Mode: Irken -*-

(include "self/cps.scm")
(include "self/typing.scm")
(include "self/graph.scm")
(include "self/analyze.scm")

(define (make-writer file)
  (let ((level 1))
    (define (write-indent)
      (let loop ((i level))
	(cond ((> i 0)
	       (write file.fd "  ")
	       (loop (- i 1))))))
    (define (write-string s)
      (write-indent)
      (write file.fd s)
      (write file.fd "\n")
      #u)
    (define (copy s)
      (write file.fd s))
    (define (indent) (set! level (+ level 1)))
    (define (dedent) (set! level (- level 1)))
    (define (close-file) (close file.fd))
    {write=write-string indent=indent dedent=dedent copy=copy close=close-file}
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
      (type:tvar id _) -> arg
      (type:pred name predargs _)
      -> (match name with
	   'int    -> (format "unbox(" arg ")")
	   'string -> (format "((pxll_string*)(" arg "))->data")
	   'arrow  -> arg
	   'vector -> arg
	   'symbol -> arg
	   'raw    -> (match predargs with
			((type:pred 'string _ _)) -> (format "((pxll_string*)(" arg "))")
			_ -> (error1 "unknown raw type in %cexp" type))
	   _ -> (error1 "unexpected predicate in cexp type sig" type))))
  (map2 wrap types args))

(define (wrap-out type exp)
  (match type with
    (type:pred 'int _ _)  -> (format "box(" exp ")")
    (type:pred 'bool _ _) -> (format "PXLL_TEST(" exp ")")
    _			  -> exp
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

(define (emit o insns context)

  (define emitk
    (cont:k _ _ k) -> (emit k)
    (cont:nil)	   -> #u)

  (define (emit insn)
    (emitk
     (match insn with
       (insn:return target)		      -> (begin (o.write (format "PXLL_RETURN(" (int target) ");")) (cont:nil))
       (insn:literal lit k)		      -> (begin (emit-literal lit (k/target k)) k)
       (insn:test reg k0 k1 k)		      -> (begin (emit-test reg k0 k1) k)
       (insn:testcexp regs sig tmpl k0 k1 k)  -> (begin (emit-testcexp regs sig tmpl k0 k1) k)
       (insn:jump reg target)		      -> (begin (emit-jump reg target) (cont:nil))
       (insn:cexp sig template args k)	      -> (begin (emit-cexp sig template args (k/target k)) k)
       (insn:close name body k)		      -> (begin (emit-close name body (k/target k)) k)
       (insn:varref d i k)		      -> (begin (emit-varref d i (k/target k)) k)
       (insn:varset d i v k)		      -> (begin (emit-varset d i v) k)
       (insn:new-env size k)		      -> (begin (emit-new-env size (k/target k)) k)
       (insn:alloc tag size k)		      -> (begin (emit-alloc tag size (k/target k)) k)
       (insn:store off arg tup i k)	      -> (begin (emit-store off arg tup i) k)
       (insn:invoke name fun args k)	      -> (begin (emit-call name fun args k) k)
       (insn:tail name fun args)	      -> (begin (emit-tail name fun args) (cont:nil))
       (insn:trcall d n args)		      -> (begin (emit-trcall d n args) (cont:nil))
       (insn:push r k)			      -> (begin (emit-push r) k)
       (insn:pop r k)			      -> (begin (emit-pop r (k/target k)) k)
       (insn:primop name parm args k)	      -> (begin (emit-primop name parm args (k/target k)) k)
       (insn:move dst var k)		      -> (begin (emit-move dst var (k/target k)) k)
       (insn:fatbar lab k0 k1 k)	      -> (begin (emit-fatbar lab k0 k1) k)
       (insn:fail label npop)		      -> (begin (emit-fail label npop) (cont:nil))
       (insn:nvcase test dt tags alts ealt k) -> (begin (emit-nvcase test dt tags alts ealt) k)
       )))

  (define (emit-literal lit target)
    (let ((val
	   (match lit with
	     (literal:int n)   -> (logior 1 (<< n 1))
	     (literal:bool b)  -> (if b #x106 #x006)
	     (literal:char ch) -> (logior 2 (<< (char->ascii ch) 8))
	     (literal:undef)   -> #x0e
	     _ -> (error1 "NYI " lit))))
      (let ((prefix (if (= target -1)
			"// dead " ;; why bother with a dead literal?
			(format "r" (int target)))))
	(o.write (format prefix " = (object *) " (int val) ";"))
      )))

  (define (emit-test reg k0 k1)
    (o.write (format "if PXLL_IS_TRUE(r" (int reg)") {"))
    (o.indent)
    (emit k0)
    (o.dedent)
    (o.write "} else {")
    (o.indent)
    (emit k1)
    (o.dedent)
    (o.write "}"))

  (define (emit-testcexp args sig template k0 k1)
    ;; we know we're testing a cexp, just inline it here
    (match sig with
      (type:pred 'arrow (result-type . arg-types) _)
      -> (let ((args (map (lambda (reg) (format "r" (int reg))) args))
	       (args (wrap-in arg-types args))
	       (exp (wrap-out result-type (cexp-subst template args))))
	   (o.write (format "if PXLL_IS_TRUE(" exp ") {"))
	   (o.indent)
	   (emit k0)
	   (o.dedent)
	   (o.write "} else {")
	   (o.indent)
	   (emit k1)
	   (o.dedent)
	   (o.write "}"))
      _ -> (impossible)))

  (define (emit-jump reg target)
    (if (>= reg 0)
	(o.write (format "r" (int target) "=r" (int reg) ";"))))

  (define (emit-cexp sig template args target)
    (match sig with
      (type:pred 'arrow (result-type . arg-types) _)
      -> (let ((args (map (lambda (reg) (format "r" (int reg))) args))
	       (args (wrap-in arg-types args))
	       (exp (wrap-out result-type (cexp-subst template args))))
	   (if (= target -1)
	       (o.write (format exp ";"))
	       (o.write (format "r" (int target) " = " exp ";"))))
      _ -> (impossible)
      ))

  (define frob-name (make-name-frobber))

  (define (gen-function-label sym)
    (format "FUN_" (frob-name (symbol->string sym))))  

  (define (emit-close name body target)
    (let ((proc-label (gen-function-label name))
	  (jump-label (label-maker)))
      ;; emit a jump over the function definition
      (o.write (format "// def " (sym name)))
      (o.write (format "goto " jump-label ";"))
      ;; emit the function definition
      (o.write (format proc-label ":"))
      (print-string (format "emit-close: " (sym name) " allocates? " (bool (vars-get-flag context name VFLAG-ALLOCATES)) "\n"))
      (o.indent)
      (if (vars-get-flag context name VFLAG-ALLOCATES)
	  (o.write "check_heap (0);"))
      (emit body)
      (o.dedent)
      (o.write (format jump-label ":"))
      (o.write (format "r" (int target) " = allocate (TC_CLOSURE, 2);"))
      (o.write (format "r" (int target) "[1] = &&" proc-label "; r" (int target) "[2] = lenv;"))
      ))

  (define (emit-varref d i target)
    (if (>= target 0)
	(o.write (format "r" (int target) " = varref (" (int d) ", " (int i) ");"))))

  (define (emit-varset d i v)
    (o.write (format "varset (" (int d) ", " (int i) ", " (int v) ");")))

  (define (emit-new-env size target)
    (o.write (format "r" (int target) " = allocate (TC_TUPLE, " (int (+ size 1)) ");")))

  (define (emit-alloc tag size target)
    (let ((tag-string
	   (match tag with
	     (tag:bare v) -> (format (int v))
	     (tag:uobj v) -> (format "TC_USEROBJ+(" (int v) "<<2)"))))
      (o.write (format "r" (int target) " = allocate (" tag-string ", " (int size) ");"))))

  (define (emit-store off arg tup i)
    (o.write (format "r" (int tup) "[" (int (+ 1 (+ i off))) "] = r" (int arg) ";")))

  (define (emit-tail name fun args)
    (let ((goto
	   (match name with
	     (maybe:no)	      -> (format "goto *r" (int fun) "[1];")
	     (maybe:yes name) -> (format "goto " (gen-function-label name) ";"))))
      (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto))))

  (define (emit-call name fun args k)
    (let ((free (sort < (k/free k))) ;; sorting these might improve things
	  (return-label (label-maker))
	  (nregs (length free))
	  (target (k/target k)))
      ;; save
      (o.write (format "t = allocate (TC_SAVE, " (int (+ 3 nregs)) ");"))
      (let ((saves
	     (map-range
		 i nregs
		 (format "t[" (int (+ i 4)) "] = r" (int (nth free i))))))
	(o.write (format "t[1] = k; t[2] = lenv; t[3] = &&" return-label "; " (string-join saves "; ") "; k = t;")))
      ;; call
      (let ((goto
	     (match name with
	       ;; strange - LLVM actually slows down if I jump to a known label.
	       (maybe:no)	-> (format "goto *r" (int fun) "[1];")
	       (maybe:yes name) -> (format "goto " (gen-function-label name) ";"))))
	(if (>= args 0)
	    (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto ";"))
	    (o.write (format "lenv = r" (int fun) "[2]; " goto ";"))))
      ;; label
      (o.write (format return-label ":"))
      ;; restore
      (let ((restores
	     (map-range
		 i nregs
		 (format "r" (int (nth free i)) " = k[" (int (+ i 4)) "]"))))
	(o.write (format (string-join restores "; ") "; lenv = k[2]; k = k[1];")))
      (if (>= target 0)
	  (o.write (format "r" (int target) " = result;")))
      ))

  (define (emit-trcall depth name regs)
    (let ((nargs (length regs))
	  (npop (- depth 1)))
      (if (= nargs 0)
	  ;; a zero-arg trcall needs an extra level of pop
	  (set! npop (+ npop 1)))
      (if (> npop 0)
	  (o.write (format "lenv = ((object " (joins (n-of npop "*")) ")lenv)" (joins (n-of npop "[1]")) ";")))
      (for-range
	  i nargs
	  (o.write (format "lenv[" (int (+ 2 i)) "] = r" (int (nth regs i)) ";")))
      (o.write (format "goto " (gen-function-label name) ";"))))

  (define (emit-push args)
    (o.write (format "r" (int args) "[1] = lenv; lenv = r" (int args) ";")))

  (define (emit-pop src target)
    (o.write (format "lenv = lenv[1];"))
    (if (>= target 0)
	(o.write (format "r" (int target) " = r" (int src)))))

  (define (subset? a b)
    (every? (lambda (x) (member-eq? x b)) a))

  (define (guess-record-type sig)
    ;; can we disambiguate this record signature?
    (let ((sig (map (lambda (x) ;; remove sexp wrapping
		      (match x with
			(sexp:symbol field) -> field
			_ -> (impossible))) sig))
	  (sig (filter (lambda (x) (not (eq? x '...))) sig)))
      (let ((candidates '()))
	(for-each
	 (lambda (x)
	   (match x with
	     (:pair sig0 index0)
	     -> (if (subset? sig sig0)
		    (PUSH candidates sig0))))
	 context.records)
	(if (= 1 (length candidates))
	    ;; unambiguous - there's only one possible match.
	    (maybe:yes (nth candidates 0))
	    ;; this sig is ambiguous given the set of known records.
	    (maybe:no)))))

  (define (emit-primop name parm args target)
    (define (primop-error)
      (error1 "primop" name))
    (let ((nargs (length args)))
      (match name with
	'%dtcon  -> (match parm with
		      (sexp:cons dtname altname)
		      -> (match (alist/lookup context.datatypes dtname) with
			   (maybe:no) -> (error1 "emit-primop: no such datatype" dtname)
			   (maybe:yes dt)
			   -> (let ((alt (dt.get altname)))
				(cond ((= nargs 0)
				       (o.write (format "r" (int target) " = (object*)UITAG(" (int alt.index) ");")))
				      (else
				       (o.write (format "t = alloc_no_clear (TC_USEROBJ+" (int (<< alt.index 2)) "," (int nargs) ");"))
				       (for-range
					   i nargs
					   (o.write (format "t[" (int (+ i 1)) "] = r" (int (nth args i)) ";")))
				       (o.write (format "r" (int target) " = t;"))))))
		      _ -> (primop-error)
		      )
	'%nvget   -> (match parm args with
		       (sexp:list (_ (sexp:int index))) (reg)
		       -> (o.write (format "r" (int target) " = UOBJ_GET(r" (int reg) "," (int index) ");"))
		       _ _ -> (primop-error))
	'%record-get -> (match parm args with
			  (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg)
			  -> (let ((label-code (lookup-label-code label context)))
			       (match (guess-record-type sig) with
				 (maybe:yes sig0)
				 -> (o.write (format "r" (int target) ;; compile-time lookup
						     " = ((pxll_vector*)r" (int rec-reg)
						     ")->val[" (int (index-eq label sig0))
						     "];"))
				 (maybe:no)
				 -> (o.write (format "r" (int target) ;; run-time lookup
						     " = ((pxll_vector*)r" (int rec-reg)
						     ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
						     ")-TC_USEROBJ)>>2," (int label-code)
						     ")];"))))
			  _ _ -> (primop-error))
	_ -> (primop-error))))

  (define (emit-move var src target)
    (cond ((>= src 0)
	   ;; from varset
	   (o.write (format "r" (int var) " = r" (int src) ";")))
	  ((>= target 0)
	   ;; from varref
	   (o.write (format "r" (int target) " = r" (int var) ";")))))

  (define (emit-fatbar label k0 k1)
    (emit k0)
    (o.write (format "goto fatbar_" (int label) "_over;"))
    (o.write (format "fatbar_" (int label) ":"))
    (emit k1)
    ;; Note: the extra semicolon here is necessary because C99 requires a 'statement'
    ;;  to follow a label.  Sometimes there's no code after the label, so this avoids
    ;;  that problem.  [might be possible to look at the insn's continuation instead]
    (o.write (format "fatbar_" (int label) "_over: ;")))

  (define (emit-fail label npop)
    (if (> npop 0)
	(o.write (format "lenv = ((object " (joins (n-of npop "*")) ")lenv)" (joins (n-of npop "[1]")) ";")))
    (o.write (format "goto fatbar_" (int label) ";")))

  (define (which-typecode-fun dt) "get_case") ;; XXX

  (define (emit-nvcase test dtname tags subs ealt)
    (match (alist/lookup context.datatypes dtname) with
      (maybe:no) -> (error1 "emit-nvcase" dtname)
      (maybe:yes dt)
      -> (if (and (= (length subs) 1) (= (dt.get-nalts) 1))
	     ;; nothing to switch on, just emit the code
	     (emit (nth subs 0))
	     (let ((use-else (not (= (length subs) (dt.get-nalts))))
		   (get-typecode (which-typecode-fun dt)))
	       (o.write (format "switch (" get-typecode " (r" (int test) ")) {"))
	       ;; XXX reorder tags to put immediate tests first!
	       (for-range
		   i (length tags)
		   (let ((label (nth tags i))
			 (sub (nth subs i))
			 (alt (dt.get label))
			 (arity alt.arity)
			 (uimm #f)
			 (tag (if (= arity 0) ;; immediate/unit constructor
				  (format "UITAG(" (int alt.index) ")")
				  (format "UOTAG(" (int alt.index) ")"))))
		     (o.indent)
		     (if (= i (- (length tags) 1))
			 (o.write "default: {")
			 (o.write (format "case (" tag "): {")))
		     (o.indent)
		     (emit sub)
		     (o.dedent)
		     (o.write "} break;")
		     (o.dedent)
		     ))
	       (cond (use-else
		      (o.indent)
		      (o.write "default: {")
		      (o.indent)
		      (emit ealt)
		      (o.dedent)
		      (o.write "}")
		      (o.dedent)))
	       (o.write "}")))))
		      
  ;; body of emit
  (emit insns)
  (o.write "Lreturn:")
  (o.write "return (pxll_int) result;")
  (o.dedent)
  (o.write "}")
  )

(define (emit-registers o context)
  (let ((nreg (+ 1 (context.regalloc.get-max))))
    (for-range
	i nreg
	(o.write (format "register object * r" (int i) ";")))
    (o.write "void gc_regs_in (int n) {")
    (o.write "  switch (n) {")
    (for-each
     (lambda (i)
       (o.write (format "  case " (int (+ i 1)) ": heap1[" (int (+ i 3)) "] = r" (int i) ";")))
     (reverse (range nreg)))
    (o.write "}}")
    (o.write "void gc_regs_out (int n) {")
    (o.write "  switch (n) {")
    (for-each
     (lambda (i)
       (o.write (format "  case " (int (+ i 1)) ": r" (int i) " = heap0[" (int (+ i 3)) "];")))
     (reverse (range nreg)))
    (o.write "}}")))
