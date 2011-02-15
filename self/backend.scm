;; -*- Mode: Irken -*-

(include "self/cps.scm")
(include "self/typing.scm")
(include "self/graph.scm")

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

  (define emitk (cont:k _ _ k) -> (emit k))

  (define emit
    (insn:return target)	    -> (o.write (format "PXLL_RETURN(" (int target) ");"))
    (insn:literal lit k)	    -> (begin (emit-literal lit (k/target k)) (emitk k))
    (insn:test reg k0 k1 k)	    -> (begin (emit-test reg k0 k1) (emitk k))
    (insn:jump reg k)		    -> (begin (emit-jump reg (k/target k)))
    (insn:cexp sig template args k) -> (begin (emit-cexp sig template args (k/target k)) (emitk k))
    (insn:close name body k)	    -> (begin (emit-close name body k) (emitk k))
    (insn:varref d i k)		    -> (begin (emit-varref d i (k/target k)) (emitk k))
    (insn:varset d i v k)	    -> (begin (emit-varset d i v) (emitk k))
    (insn:new-env size k)	    -> (begin (emit-new-env size (k/target k)) (emitk k))
    (insn:store off arg tup i k)    -> (begin (emit-store off arg tup i) (emitk k))
    (insn:invoke fun args k)	    -> (begin (emit-call fun args k) (emitk k))
    (insn:tail fun args k)	    -> (begin (emit-tail fun args) (emitk k))
    (insn:push r k)                 -> (begin (emit-push r) (emitk k))
    (insn:pop r k)                  -> (begin (emit-pop r (k/target k)) (emitk k))
    (insn:primop name parm args k)  -> (begin (emit-primop name parm args (k/target k)) (emitk k))
    (insn:move dst var k)           -> (begin (emit-move dst var (k/target k)) (emitk k))
    (insn:fatbar lab k0 k1 k)       -> (begin (emit-fatbar lab k0 k1) (emitk k))
    (insn:fail label npop k)        -> (begin (emit-fail label npop) (emitk k))
    (insn:nvcase test dt tags alts ealt k)
    -> (begin (emit-nvcase test dt tags alts ealt) (emitk k))
    x -> (error1 "NYI " x) ;; XXX remove me
    )

  (define (emit-literal lit target)
    (let ((val
	   (match lit with
	     (literal:int n)   -> (logior 1 (<< n 1))
	     (literal:bool b)  -> (if b #x106 #x006)
	     (literal:char ch) -> (logior 2 (<< (char->ascii ch) 8))
	     (literal:undef)   -> #x0e
	     _ -> (error1 "NYI " lit))))
      (if (= target -1)
	  (o.write "// ")) ;; why bother with a dead literal?
      (o.write (format "r" (int target) " = (object *) " (int val) ";"))
      ))

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

  (define (emit-jump reg target)
    (if (>= reg 0)
	(o.write (format "r" (int target) "=r" (int reg) ";"))))

  (define (emit-cexp sig template args target)
    (let ((result-type sig)
	  (arg-types '()))
      (match sig with
	(type:pred 'arrow (rtype . atypes) _)
	-> (begin (set! result-type rtype) (set! arg-types atypes))
	_ -> #u)
      (let ((args (map (lambda (reg) (format "r" (int reg))) args))
	    (args (wrap-in arg-types args))
	    (exp (wrap-out result-type (cexp-subst template args))))
	(if (= target -1)
	    (o.write (format exp ";"))
	    (o.write (format "r" (int target) " = " exp ";"))))))

  (define frob-name (make-name-frobber))

  (define (gen-function-label sym)
    (format "FUN_" (frob-name (symbol->string sym))))  

  (define (emit-close name body k)
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

  (define (emit-store off arg tup i)
    (o.write (format "r" (int tup) "[" (int (+ 1 (+ i off))) "] = r" (int arg) ";")))

  (define (emit-tail fun args)
    (let ((goto (format "goto *r" (int fun) "[1];")))
      (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto))))

  (define (emit-call fun args k)
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
      (let ((goto (format "goto *r" (int fun) "[1]")))
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

  (define (emit-push args)
    (o.write (format "r" (int args) "[1] = lenv; lenv = r" (int args) ";")))

  (define (emit-pop src target)
    (o.write (format "lenv = lenv[1];"))
    (if (>= target 0)
	(o.write (format "r" (int target) " = r" (int src)))))

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
	(o.write (format "lenv = ((object " (joins (n-of npop "*"))
			 ")lenv)" (joins (n-of npop "[1]")) ";")))
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
	(o.write (format "  register object * r" (int i) ";")))
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

(define (find-base path)
  (let ((parts (string-split path #\.))
	(rparts (reverse parts)))
    (if (not (string=? (first rparts) "scm"))
	(error1 "find-base" path)
	(string-join (reverse (cdr rparts)) "."))))

(define (read-template)
  (let ((ifile (file/open-read "header.c")))
    (let loop ((buf (file/read-buffer ifile))
	       (l '()))
      (cond ((= (string-length buf) 0) (string-concat (reverse l)))
	    (else (loop (file/read-buffer ifile)
			(list:cons buf l)))))))

(define sentinel0 "// CONSTRUCTED LITERALS //\n")
(define sentinel1 "// REGISTER_DECLARATIONS //\n")

(define (get-header-parts)
  (let ((header (read-template))
	(pos0 (string-find sentinel0 header))
	(pos1 (string-find sentinel1 header))
	)
    (if (or (= pos0 -1) (= pos1 -1))
	(error1 "template strings not found in header.c?" (:pair pos0 pos1))
	(let ((pos0 (+ pos0 (string-length sentinel0)))
	      (pos1 (+ pos1 (string-length sentinel1)))
	      (part0 (substring header 0 pos0))
	      (part1 (substring header pos0 pos1))
	      (part2 (substring header pos1 (string-length header))))
	  (:header part0 part1 part2)))))

(define (system cmd)
  (%%cexp (string -> int) "system (%0)" cmd))

(define (test)
  (if (< sys.argc 2)
      (error "Usage: compile <irken-src-file>"))
  (let ((context (make-context))
	(transform (transformer context))
	(path sys.argv[1])
	(base (find-base path))
	(opath (string-append base ".c"))
	(exp0 (sexp:list (read-file path)))
	(exp1 (transform exp0))
	(_ (begin (pp 0 exp1) (newline)))
	(node0 (walk exp1))
	(node0 (apply-substs node0))
	(_ (rename-variables node0))
	(_ (begin (pp-node node0 0) newline))
	(graph0 (build-dependency-graph node0))
	(_ (print-graph graph0))
	(strong (strongly graph0))
	(_ (printn strong))
	(_ (set! context.scc-graph strong))
	(type0 (type-program node0 context))
	)
    (print-string "\n-- reader --\n")
    (unread exp0)
    (newline)
    (print-string "\n-- macros --\n")
    (unread exp1)
    (newline)
    (print-string "\n-- node tree --\n")
    (pp-node node0 0) (newline)
    (let ((cps (compile node0 context))
	  (ofile (file/open-write opath #t #o644))
	  (o (make-writer ofile)))
      (print-string "\n-- RTL --\n")
      (print-insn cps 0)
      (newline)
      (print-string "\n-- datatypes --\n")
      (alist/iterate
       (lambda (name dt)
	 (print-datatype dt))
       context.datatypes)
      (print-string "\n-- C output --\n")
      (print-string " : ") (print-string opath) (newline)
      (match (get-header-parts) with
	(:header part0 part1 part2)
	-> (begin (printn part0)
		  (printn part1)
		  (printn part2)
		  (o.copy part0)
		  (o.copy part1)
		  (emit-registers o context)
		  (o.copy part2)
		  (emit o cps context)))
      (print-string "done.\n")
      (o.close)
      (print-string "compiling...\n")
      (system (format "/usr/local/bin/gcc -I. -g -m64 " opath " -o " base))
      )
    )
  )
  
(test)
