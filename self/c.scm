;; -*- Mode: Irken -*-

;; provide automatic conversions of base types for inputs to %%cexp
(define (wrap-in type arg)
  (match type with
    (type:tvar id _) -> arg
    (type:pred name predargs _)
    -> (match name with
	 'int	       -> (format "UNBOX_INTEGER(" arg ")")
	 'bool         -> (format "PXLL_IS_TRUE(" arg ")")
	 'string       -> (format "((pxll_string*)(" arg "))->data")
	 'cstring      -> (format "(char*)" arg)
         ;; cmem is either a buffer or a pointer
         'cmem         -> (format "(" (irken-type->c-type type) ")(mem2ptr(" arg "))")
	 'buffer       -> (format "(" (irken-type->c-type type) "(((pxll_vector*)" arg ")+1))")
         '*            -> (format "(" (irken-type->c-type type) ")" arg)
	 'arrow	       -> arg
	 'vector       -> arg
	 'symbol       -> arg
	 'char	       -> arg
	 'continuation -> arg
	 'raw	       -> (match predargs with
			    ((type:pred 'string _ _)) -> (format "((pxll_string*)(" arg "))")
			    _ -> (error1 "unknown raw type in %cexp" type))
	 kind          -> (if (member-eq? kind c-int-types)
			      (format "unbox(" arg ")")
			      (error1 "wrap-in:" type))
	 )))

;; (buffer (struct sockaddr_t)) => (struct sockaddr_t *)
(define (irken-type->c-type t)
  (match t with
    (type:pred 'struct (arg) _) -> (format "struct " (irken-type->c-type arg))
    (type:pred 'cmem   (arg) _) -> (format (irken-type->c-type arg) "*")
    (type:pred 'buffer (arg) _) -> (format (irken-type->c-type arg) "*")
    (type:pred '*      (arg) _) -> (format (irken-type->c-type arg) "*")
    (type:pred name () _)       -> (format (sym name))
    (type:tvar _ _)             -> "void"
    _ -> (error1 "malformed ctype" (type-repr t))))


;;
;; ok, for *now*, I don't really want subtyping.  but I *do* want
;;  automatic casting/conversion... what's the cleanest way to get that?
;; We have to deal with both typing and code generation.
;;

(define c-int-types
  ;; XXX distinguish between signed and unsigned!
  ;; XXX also need to handle 64-bit types on a 32-bit platform.
  '(uint8_t uint16_t uint32_t uint64_t
    int8_t int16_t int32_t int64_t))

(define (wrap-out type exp)
  (match type with
    (type:pred 'int _ _)     -> (format "BOX_INTEGER((pxll_int)" exp ")")
    (type:pred 'bool _ _)    -> (format "PXLL_TEST(" exp ")")
    (type:pred 'cstring _ _) -> (format "(object*)" exp)
    (type:pred '* _ _)       -> (format "(ptr2mem(" exp ")")
    (type:pred 'buffer _ _)  -> (format "(buf2mem(" exp ")")
    (type:pred 'void _ _)    -> (format "(" exp ", (object*)PXLL_UNDEFINED)")
    (type:pred kind _ _)     -> (if (member-eq? kind c-int-types)
				    (format "box((pxll_int)" exp ")")
				    exp)
    _			     -> exp
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

(define (emit-c o decls insns)

  (let ((fun-stack '())
	(current-function-cname "")
	(current-function-name 'toplevel)
	(current-function-part (make-counter 1))
	(env-counter (make-counter 0))
	(env-stack '())
	(used-jumps (find-jumps insns))
	(fatbar-free (map-maker int-cmp))
	(declared (set2-maker string-compare))
	)

    (define emitk
      (cont:k _ _ k) -> (emit k)
      (cont:nil)     -> #u)

    (define (emit insn)
      (emitk
       (match insn with
	 (insn:return target)			      -> (begin (o.write (format "PXLL_RETURN(" (int target) ");")) (cont:nil))
	 (insn:literal lit k)			      -> (begin (emit-literal lit (k/target k)) k)
	 (insn:litcon i kind k)			      -> (begin (emit-litcon i kind (k/target k)) k)
	 (insn:test reg jn k0 k1 k)		      -> (begin (emit-test reg jn k0 k1 k) (cont:nil))
	 (insn:testcexp regs sig tmpl jn k0 k1 k)     -> (begin (emit-testcexp regs sig tmpl jn k0 k1 k) (cont:nil))
	 (insn:jump reg target jn free)		      -> (begin (emit-jump reg target jn free) (cont:nil))
	 (insn:cexp sig type template args k)	      -> (begin (emit-cexp sig type template args (k/target k)) k)
	 (insn:ffi sig type name args k)	      -> (error1 "no FFI in C backend" insn)
	 (insn:close name nreg body k)		      -> (begin (emit-close name nreg body (k/target k)) k)
	 (insn:varref d i k)			      -> (begin (emit-varref d i (k/target k)) k)
	 (insn:varset d i v k)			      -> (begin (emit-varset d i v (k/target k)) k)
	 (insn:new-env size top? types k)	      -> (begin (emit-new-env size top? types (k/target k)) k)
	 (insn:alloc tag size k)		      -> (begin (emit-alloc tag size (k/target k)) k)
	 (insn:store off arg tup i k)		      -> (begin (emit-store off arg tup i) k)
	 (insn:invoke name fun args k)		      -> (begin (emit-call name fun args k) (cont:nil))
	 (insn:tail name fun args)		      -> (begin (emit-tail name fun args) (cont:nil))
	 (insn:trcall d n args)			      -> (begin (emit-trcall d n args) (cont:nil))
	 (insn:push r k)			      -> (begin (emit-push r) k)
	 (insn:pop r k)				      -> (begin (emit-pop r (k/target k)) k)
	 (insn:primop name parm t args k)	      -> (begin (emit-primop name parm t args k) k)
	 (insn:move dst var k)			      -> (begin (emit-move dst var (k/target k)) k)
	 (insn:fatbar lab jn k0 k1 k)		      -> (begin (emit-fatbar lab jn k0 k1 k) (cont:nil))
	 (insn:fail label npop free)		      -> (begin (emit-fail label npop free) (cont:nil))
	 (insn:nvcase tr dt tags jn alts ealt k)      -> (begin (emit-nvcase tr dt tags jn alts ealt k) (cont:nil))
	 (insn:pvcase tr tags arities jn alts ealt k) -> (begin (emit-pvcase tr tags arities jn alts ealt k) (cont:nil))
	 )))

    ;; XXX arrange to avoid duplicates caused by jump conts
    (define (declare-function name extern?)
      (when (not (declared::member name))
	    (declared::add name)
	    (let ((linkage (if extern? "extern" "static")))
	      (decls.write (format linkage " void " name "(void);")))))

    (define (move src dst)
      (if (and (>= dst 0) (not (= src dst)))
	  (o.write (format "O r" (int dst) " = r" (int src) ";"))))

    (define (emit-literal lit target)
      (let ((val (encode-immediate lit))
	    (prefix (if (= target -1)
			"// dead " ;; why bother with a dead literal?
			(format "O r" (int target)))))
	(o.write (format prefix " = (object *) " (int val) ";"))
	))

    (define (emit-litcon index kind target)
      (if (>= target 0)
	  (cond ((eq? kind 'string)
		 (o.write (format "O r" (int target) " = (object*) &constructed_" (int index) ";")))
		(else
		 (o.write (format "O r" (int target) " = (object *) constructed_" (int index) "[0];"))))))

    (define (emit-test reg jn k0 k1 k)
      (push-jump-continuation k jn)
      (o.write (format "if PXLL_IS_TRUE(r" (int reg)") {"))
      (o.indent)
      (emit k0)
      (o.dedent)
      (o.write "} else {")
      (o.indent)
      (emit k1)
      (o.dedent)
      (o.write "}")
      )

    (define (emit-testcexp args sig template jn k0 k1 k)
      ;; we know we're testing a cexp, just inline it here
      (match sig with
	(type:pred 'arrow (result-type . arg-types) _)
	-> (let ((args0 (map (lambda (reg) (format "r" (int reg))) args))
		 (args1 (map2 wrap-in arg-types args0))
		 (exp (wrap-out result-type (cexp-subst template args1))))
	     (push-jump-continuation k jn)
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

    (define (emit-jump reg target jump-num free)
      (move reg target)
      (let ((jname (format "JUMP_" (int jump-num))))
	(match (used-jumps::get jump-num) with
	  (maybe:yes free)
	  -> (o.write (format jname "(" (join (lambda (x) (format "r" (int x))) ", " free) ");"))
	  (maybe:no)
	  -> (impossible))
	))

    ;; XXX consider this: giving access to the set of free registers.
    ;;   would make it possible to do %ensure-heap in a %%cexp.
    (define (emit-cexp sig type template args target)
      (let ((exp
	     (match sig with
	       (type:pred 'arrow (result-type . arg-types) _)
	       -> (let ((args0 (map (lambda (reg) (format "r" (int reg))) args))
			(args1 (map2 wrap-in arg-types args0)))
		    ;; from the sig
		    ;;(wrap-out result-type (cexp-subst template args1))
		    ;; the solved type
		    (wrap-out type (cexp-subst template args1))
		    )
	       ;; some constant type
	       _ -> (wrap-out sig template))))
	(if (= target -1)
	    (o.write (format exp ";"))
	    (o.write (format "O r" (int target) " = " exp ";")))))

    (define (emit-check-heap free size)
      (let ((n (length free)))
	(o.write (format "if (freep + " size " >= limit) {"))
	(o.indent)
	;; copy free variables into tospace
	(for-range
	    i n
	    (o.write (format "heap1[" (int (+ i 3)) "] = r" (int (nth free i)) ";")))
	;; gc
	(o.write (format "gc_flip (" (int n) ");"))
	;; copy values back into free variables
	(for-range
	    i n
	    (o.write (format "r" (int (nth free i)) " = heap0[" (int (+ i 3)) "];")))
	(o.dedent)
	(o.write "}")
	))

    (define (emit-cfun o name cname body)
      (set! current-function-name name)
      (set! current-function-cname cname)
      (o.write (format "static void " cname " (void) {"))
      (o.indent)
      (when the-context.options.trace (o.write (format "TRACE(\"" cname "\");")))
      (when the-context.options.profile
	(match (tree/member the-context.profile-funs symbol-index-cmp name) with
	  (maybe:yes {index=index names=_})
	  -> (begin
	       (o.write "prof_mark1 = rdtsc();")
	       ;; charge to the calling function
	       (o.write "prof_funs[prof_current_fun].ticks += (prof_mark1 - prof_mark0);")
	       ;; set the current function (note: 'top' is at position zero)
	       (o.write (format "prof_current_fun = " (int (+ index 1)) ";"))
	       (o.write "prof_funs[prof_current_fun].calls++;"))
	  (maybe:no) -> (impossible)
	  ))
      (if (vars-get-flag name VFLAG-ALLOCATES)
	  ;; XXX this only works because we disabled letreg around functions
	  (emit-check-heap '() "0"))
      (when the-context.options.profile
	;; this avoids charging gc to this fun
	(o.write "prof_mark0 = rdtsc();"))
      (emit body)
      (o.dedent)
      (o.write "}")
      )

    (define (emit-close name nreg body target)
      (let ((cname (gen-function-cname name 0)))
	(declare-function cname #f)
	(PUSH fun-stack (lambda () (emit-cfun o name cname body)))
	(o.write (format "O r" (int target) " = allocate (TC_CLOSURE, 2);"))
	(o.write (format "r" (int target) "[1] = " cname "; r" (int target) "[2] = lenv;"))
	))

    (define (push-continuation cname insn args)
      (let ((args (format (join (lambda (x) (format "O r" (int x))) ", " args))))
	(PUSH fun-stack
	      (lambda ()
		(o.write (format "static void " cname "(" args ") {"))
		(o.indent)
		(when the-context.options.trace (o.write (format "TRACE(\"" cname "\");")))
		(emit insn)
		(o.dedent)
		(o.write "}")
		))))

    (define (push-fail-continuation insn jump args)
      (push-continuation (format "FAIL_" (int jump)) insn args))

    (define (push-jump-continuation cont jump)
      (match (used-jumps::get jump) with
	(maybe:yes free)
	-> (let ((cname (format "JUMP_" (int jump))))
	     (decls.write (format "static void " cname "(" (string-join (n-of (length free) "O") ", ") ");"))
	     (push-continuation (format "JUMP_" (int jump)) (k/insn cont) free)
	     )
	(maybe:no)
	-> #u))

    (define (emit-varref d i target)
      (if (>= target 0)
	  (let ((src
		 (if (= d -1)
		     (format "top[" (int (+ 2 i)) "];") ;; the +2 is to skip the header and next ptr
		     ;;(format "varref (" (int d) ", " (int i) ");")
		     (format "((object*" (repeat d "*") ") lenv) " (repeat d "[1]") "[" (int (+ i 2)) "];")
		     )))
	    (o.write (format "O r" (int target) " = " src)))))

    (define (emit-varset d i v target)
      (if (= d -1)
	  (o.write (format "top[" (int (+ 2 i)) "] = r" (int v) ";"))
	  ;;(o.write (format "varset (" (int d) ", " (int i) ", r" (int v) ");"))
	  (o.write (format "((object*" (repeat d "*") ") lenv) " (repeat d "[1]") "[" (int (+ i 2)) "] = r" (int v) ";"))
	  )
      (when (>= target 0)
	    ;; this handles this idiom:
	    ;; (let ((x 3)
	    ;;       (_ (set! x 99))
	    ;;       (y ...)) ...)
	    ;; the set! is a dead assignment, but we need to put something there
	    (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))

    (define (emit-new-env size top? types target)
      (let ((env-index (env-counter.inc)))
	(o.write (format "// env " (int env-index) ":"))
	(o.write (format "//   " (join type-repr " " types)))
	(o.write (format "O r" (int target) " = allocate (TC_ENV, " (int (+ size 1)) ");"))
	(if top?
	    (o.write (format "top = r" (int target) ";")))))

    (define (emit-alloc tag size target)
      (let ((tag-string
	     (match tag with
	       (tag:bare v) -> (format (int v))
	       (tag:uobj v) -> (format (if (= size 0) "UITAG(" "UOTAG(") (int v) ")"))))
	(if (= size 0)
	    ;; unit type - use an immediate
	    (o.write (format "O r" (int target) " = (object*)" tag-string ";"))
	    (o.write (format "O r" (int target) " = allocate (" tag-string ", " (int size) ");")))))

    (define (emit-store off arg tup i)
      (o.write (format "r" (int tup) "[" (int (+ 1 (+ i off))) "] = r" (int arg) ";")))

    (define (safe-known-fun name)
      (let ((var (vars-get-var name)))
        (= 0 var.sets)))

    (define (format-call mname funreg)
      (match mname with
        (maybe:no) -> (format "((kfun)(r" (int funreg) "[1]))();") ;; unknown
        (maybe:yes name)
        -> (let ((var (vars-get-var name)))
             (if (= 0 var.sets)
                 (let ((cfun (gen-function-cname name 0))) ;; known
                   ;; include last-minute forward declaration
                   (declare-function cfun #f)
                   (format cfun "();")
                   )
                 (format "((kfun)(r" (int funreg) "[1]))();") ;; mutated
                 ))
        ))

    (define (emit-tail name fun args)
      (let ((funcall (format-call name fun)))
	(if (>= args 0)
	    (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " funcall))
	    (o.write (format "lenv = r" (int fun) "[2]; " funcall))
	    )))

    (define (emit-call name fun args k)
      (let ((free (sort < (k/free k))) ;; sorting these might improve things
	    (nregs (length free))
	    (target (k/target k))
	    (kfun (gen-function-cname current-function-name (current-function-part.inc)))
	    )
	;; save
	(o.write (format "O t = allocate (TC_SAVE, " (int (+ 3 nregs)) ");"))
	(let ((saves
	       (map-range
		   i nregs
		   (format "t[" (int (+ i 4)) "] = r" (int (nth free i))))))
	  (declare-function kfun #f)
	  (o.write (format "t[1] = k; t[2] = lenv; t[3] = " kfun "; " (string-join saves "; ") "; k = t;")))
	;; call
	(let ((funcall (format-call name fun)))
	  (if (>= args 0)
	      (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " funcall))
	      (o.write (format "lenv = r" (int fun) "[2]; " funcall))))
	;; emit a new c function to represent the continuation of the current irken function
	(PUSH fun-stack
	      (lambda ()
		(set! current-function-cname kfun)
		(o.write (format "static void " kfun " (void) {"))
		(o.indent)
		;; restore
		(let ((restores
		       (map-range
			   i nregs
			   (format "O r" (int (nth free i)) " = k[" (int (+ i 4)) "]"))))
		  (o.write (format (string-join restores "; ") "; lenv = k[2]; k = k[1];")))
		(if (>= target 0)
		    (o.write (format "O r" (int target) " = result;")))
		(emitk k)
		(o.dedent)
		(o.write (format "}"))
		)
	      )
	))

    (define (emit-trcall depth name regs)
      (let ((nargs (length regs))
	    (npop (- depth 1))
	    (cname (gen-function-cname name 0)))
	(if (= nargs 0)
	    ;; a zero-arg trcall needs an extra level of pop
	    (set! npop (+ npop 1)))
	(if (> npop 0)
	    (o.write (format "lenv = ((object " (join (n-of npop "*")) ")lenv)" (join (n-of npop "[1]")) ";")))
	(for-range
	    i nargs
	    (o.write (format "lenv[" (int (+ 2 i)) "] = r" (int (nth regs i)) ";")))
	(declare-function cname #f)
	(o.write (format cname "();"))
      ))

    (define (emit-push args)
      (o.write (format "r" (int args) "[1] = lenv; lenv = r" (int args) ";")))

    (define (emit-pop src target)
      (o.write (format "lenv = lenv[1];"))
      (move src target))

    ;; hacks for datatypes known by the runtime
    (define (get-uotag dtname altname index)
      (match dtname altname with
	'list 'cons -> "TC_PAIR"
	'symbol 't -> "TC_SYMBOL"
	_ _ -> (format "UOTAG(" (int index) ")")))

    (define (get-uitag dtname altname index)
      (match dtname altname with
	'list 'nil -> "TC_NIL"
	'bool 'true -> "(pxll_int)PXLL_TRUE"
	'bool 'false -> "(pxll_int)PXLL_FALSE"
	_ _ -> (format "UITAG(" (int index) ")")))

    (define (emit-primop name parm type args k)

      ;; this *really* needs the print-context-of-error thing that typing.scm uses.
      (define (primop-error)
	(error1 "primop" name))

      (let ((target (k/target k))
	    (nargs (length args)))

        (define prim-dtcon
          (sexp:cons dtname altname)
          -> (match (alist/lookup the-context.datatypes dtname) with
               (maybe:no) -> (error1 "emit-primop: no such datatype" dtname)
               (maybe:yes dt)
               -> (let ((alt (dt.get altname)))
                    (cond ((= nargs 0)
                           (o.write (format "O r" (int target) " = (object*)"
                                            (get-uitag dtname altname alt.index) ";")))
                          (else
                           (if (>= target 0)
                               (let ((trg (format "r" (int target))))
                                 (o.write (format "O " trg " = alloc_no_clear ("
                                                  (get-uotag dtname altname alt.index)
                                                  "," (int nargs) ");"))
                                 (for-range
                                     i nargs
                                   (o.write (format trg "[" (int (+ i 1))
                                                    "] = r" (int (nth args i))
                                                    ";"))))
                               (warning (format "dead target in primop " (sym name) "\n"))
                               )))))
          _ -> (primop-error))

        (define prim-nvget
          (sexp:list (_ (sexp:int index) _)) (reg)
          -> (o.write (format "O r" (int target) " = UOBJ_GET(r" (int reg) "," (int index) ");"))
          _ _ -> (primop-error))

        (define prim-make-vector
          (vlen vval)
          -> (begin
               ;; since we cannot know the size at compile-time, there should
               ;; always be a call to ensure_heap() before any call to %make-vector
               (o.write (format "O r" (int target) ";"))
               (o.write (format "if (unbox(r" (int vlen) ") == 0) { r" (int target) " = (object *) TC_EMPTY_VECTOR; } else {"))
               (o.write (format "  O t = alloc_no_clear (TC_VECTOR, unbox(r" (int vlen) "));"))
               (o.write (format "  for (int i=0; i<unbox(r" (int vlen) "); i++) { t[i+1] = r" (int vval) "; }"))
               (o.write (format "  r" (int target) " = t;"))
               (o.write "}"))
          _ -> (primop-error))

        (define prim-array-ref
          (vec index)
          -> (begin
               (o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
               (o.write (format "O r" (int target) " = ((pxll_vector*)r" (int vec) ")->val[unbox(r" (int index) ")];")))
          _ -> (primop-error))

        (define prim-array-set
          (vec index val)
          -> (begin
               (o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
               (o.write (format "((pxll_vector*)r" (int vec) ")->val[unbox(r" (int index) ")] = r" (int val) ";"))
               (when (> target 0)
                 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
          _ -> (primop-error))

        (define prim-record-get
          (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg)
          -> (let ((label-code (lookup-label-code label)))
               (match (guess-record-type sig) with
                 (maybe:yes sig0)
                 -> (o.write (format "O r" (int target) ;; compile-time lookup
                                     " = ((pxll_vector*)r" (int rec-reg)
                                     ")->val[" (int (index-eq label sig0))
                                     "];"))
                 (maybe:no)
                 -> (o.write (format "O r" (int target) ;; run-time lookup
                                     " = ((pxll_vector*)r" (int rec-reg)
                                     ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
                                     ")-TC_USEROBJ)>>2," (int label-code)
                                     ")]; // label=" (sym label)))))
          _ _ -> (primop-error))

        ;; XXX very similar to record-get, maybe some way to collapse the code?
        (define prim-record-set
          (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg arg-reg)
          -> (let ((label-code (lookup-label-code label)))
               (match (guess-record-type sig) with
                 (maybe:yes sig0)
                 -> (o.write (format "((pxll_vector*)r" (int rec-reg) ;; compile-time lookup
                                     ")->val[" (int (index-eq label sig0))
                                     "] = r" (int arg-reg) ";"))
                 (maybe:no)
                 -> (o.write (format "((pxll_vector*)r" (int rec-reg) ;; run-time lookup
                                     ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
                                     ")-TC_USEROBJ)>>2," (int label-code)
                                     ")] = r" (int arg-reg) ";")))
               (when (>= target 0)
                 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
          _ _ -> (primop-error))

        (define (prim-callocate parm args)
          (let ((type (parse-type parm))) ;; gets parsed twice, convert to %%cexp?
            ;; XXX maybe make alloc_no_clear do an ensure_heap itself?
            (if (>= target 0)
                (o.write (format "O r" (int target) " = alloc_no_clear (TC_BUFFER, HOW_MANY (sizeof (" (irken-type->c-type type)
                                 ") * unbox(r" (int (car args)) "), sizeof (object)));"))
                (error1 "%callocate: dead target?" type))))

        (define (prim-callocate2 parm args malloc?)
          (let ((type (parse-type parm))) ;; gets parsed twice, convert to %%cexp?
            ;; XXX maybe make alloc_no_clear do an ensure_heap itself?
            ;; in this version we wrap the buffer object with a user datatype tuple
            ;; indicating that it is a cmem:{buffer,ptr} type.
            (cond ((>= target 0)
                   (o.write (format "O r" (int target) " = allocate (TC_USEROBJ+0, 1);"))
                   (if malloc?
                       (o.write (format "O r" (int target) "t = (object*) malloc (sizeof ("
                                        (irken-type->c-type type) ") * unbox(r" (int (car args)) "));"))
                       (o.write (format "O r" (int target) "t = alloc_no_clear (TC_BUFFER, HOW_MANY (sizeof ("
                                        (irken-type->c-type type) ") * unbox(r" (int (car args)) "), sizeof (object)));")))
                   (o.write (format "r" (int target) "[1] = r" (int target) "t;")))
                  (else
                   (error1 "%callocate2: dead target?" type)))))

        (define (prim-exit args)
          (o.write (format "result=r" (int (car args)) "; exit_continuation();"))
          (when (> target 0)
            (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))

        (define prim-cget
          (rbase rindex)
          ;; XXX range-check (probably need to add a length param to TC_BUFFER)
          -> (let ((cexp (format "(((" (type-repr type) "*)((pxll_int*)r" (int rbase) ")+1)[" (int rindex) "])")))
               (o.write (format "O r" (int target) " = " (wrap-out type cexp) ";")))
          _ -> (primop-error))

        (define prim-cset
          (rbase rindex rval) (type:pred 'arrow (to-type from-type) _)
          ;; XXX range-check (probably need to add a length param to TC_BUFFER)
          -> (let ((rval-exp (lookup-cast to-type from-type (format "r" (int rval))))
                   (lval (format "(((" (type-repr to-type) "*)((pxll_int*)r" (int rbase) ")+1)[" (int rindex) "])")))
               (o.write (format lval " = " rval-exp ";"))
               (when (> target 0)
                 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
          _ _ -> (primop-error))

        (define prim-getcc
          () -> (o.write (format "O r" (int target) " = k; // %getcc"))
          _  -> (primop-error))

        (define prim-putcc
          (rk rv) -> (begin
                       (o.write (format "k = r" (int rk) "; // %putcc"))
                       (move rv target))
          _ -> (primop-error))

        (define (prim-ffi2 parm args)
          (match parm with
            (sexp:symbol name)
            -> (match (ffi-info.sigs::get name) with
                 (maybe:yes (csig:fun name rtype argtypes))
                 -> (let ((args0 (map (lambda (reg) (format "r" (int reg))) args))
                          (argt0 (map ctype->irken-type argtypes))
                          (args2 (map2 wrap-in argt0 args0))
                          (call0 (format (sym name) "(" (join "," args2) ")"))
                          (call1 (if (= target -1) call0 (wrap-out type call0))))
                      (if (= target -1)
                          (o.write (format call1 ";"))
                          (o.write (format "O r" (int target) " = " call1 ";"))))
                 _ -> (primop-error))
            _ -> (primop-error)
            ))

        (define prim-cget3
          (sexp:symbol name)
          -> (match (ffi-info.sigs::get name) with
               (maybe:yes (csig:obj name obtype))
               -> (let (;; might need to use ctype->irken-type here
                        (ob0 (format "((" (type-repr type) ")" (sym name) ")"))
                        (ob1 (wrap-out type ob0)))
                    (o.write (format "O r" (int target) " = " ob1 ";"))
                    )
               _ -> (primop-error))
          _ -> (primop-error))

        (match name with
          '%dtcon       -> (prim-dtcon parm)
          '%nvget       -> (prim-nvget parm args)
          '%make-vector -> (prim-make-vector args)
          '%array-ref   -> (prim-array-ref args)
          '%array-set   -> (prim-array-set args)
          '%record-get  -> (prim-record-get parm args)
          '%record-set  -> (prim-record-set parm args)
          '%callocate   -> (prim-callocate parm args)
          '%halloc      -> (prim-callocate2 parm args #f)
          '%malloc      -> (prim-callocate2 parm args #t)
          ;;'%free        -> (prim-free args)
          '%exit        -> (prim-exit args)
          '%cget        -> (prim-cget args)
          '%cset        -> (prim-cset args type)
          '%getcc       -> (prim-getcc args)
          '%putcc       -> (prim-putcc args)
          '%ensure-heap -> (emit-check-heap (k/free k) (format "unbox(r" (int (car args)) ")"))
          '%ffi2        -> (prim-ffi2 parm args)
          '%cget3       -> (prim-cget3 parm)
          _ -> (primop-error))))

    (define (lookup-cast to-type from-type exp)
      (match to-type from-type with
	(type:pred tout _ _) (type:pred 'int _ _)
	-> (if (member-eq? tout c-int-types)
	       (format "((" (sym tout) ")unbox(" exp "))")
	       (error1 "lookup-cast: can't cast from int to: " tout))
	_ _ -> (error1 "lookup-cast: unable to cast between types: " (:pair to-type from-type))))

    (define (emit-move var src target)
      (cond ((and (>= src 0) (not (= src var)))
	     ;; from varset
	     (o.write (format "r" (int var) " = r" (int src) "; // reg varset"))
	     (if (>= target 0)
		 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
	    ((and (>= target 0) (not (= target var)))
	     ;; from varref
	     (o.write (format "O r" (int target) " = r" (int var) "; // reg varref")))))

    ;; we emit insns for k0, which may or may not jump to fail continuation in k1
    (define (emit-fatbar label jn k0 k1 k)
      (fatbar-free::add label (k/free k))
      (push-fail-continuation k1 label (k/free k))
      (push-jump-continuation k jn)
      (o.write (format "// fatbar jn=" (int jn) " label=" (int label)))
      (emit k0))

    (define (emit-fail label npop free)
      (if (> npop 0)
	  (o.write (format "lenv = ((object " (join (n-of npop "*")) ")lenv)" (join (n-of npop "[1]")) ";")))
      (let ((jname (format "FAIL_" (int label))))
	(match (fatbar-free::get label) with
	  (maybe:yes free)
	  -> (begin
	       (o.write (format jname "(" (join (lambda (x) (format "r" (int x))) ", " free) ");"))
	       (decls.write (format "static void " jname "(" (string-join (n-of (length free) "O") ", ") ");")))
	  (maybe:no)
	  -> (impossible)
	  )))

    ;;
    ;; thinking about get_case():
    ;;
    ;;  We can avoid even more branching and checking of pointers by choosing the test function
    ;;  *after* deciding on where to put the 'default' fall-through.  For example, if we are testing
    ;;  a list, then the switch usually looks like this:
    ;;
    ;;  switch () {
    ;;  case TC_NIL:
    ;;     ...
    ;;  default:
    ;;     ...
    ;;  }
    ;;
    ;; So in this particular case we need only check for immediate TC_NIL.
    ;; XXX Before implementing, see if the C compiler isn't already doing this for us.
    ;;
    (define (which-typecode-fun dt) "get_case") ;; XXX

    (define (emit-nvcase test dtname tags jump-num subs ealt k)
      (let ((use-else? (maybe? ealt)))
	(match (alist/lookup the-context.datatypes dtname) with
	  (maybe:no) -> (error1 "emit-nvcase" dtname)
	  (maybe:yes dt)
	  -> ;;(if (and (= (length subs) 1) (= (dt.get-nalts) 1))
		 ;; (begin
		 ;;   ;; nothing to switch on, just emit the code
		 ;;   (printf "unused jump-num: " (int jump-num) "\n")
		 ;;   (emit (nth subs 0))
		 ;;   (emitk k) ;; and continue...
		 ;;   )
		 (let ((get-typecode (which-typecode-fun dt)))
		   (push-jump-continuation k jump-num)
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
				      (get-uitag dtname label alt.index)
				      (get-uotag dtname label alt.index))))
			 (o.indent)
			 (if (and (not use-else?) (= i (- (length tags) 1)))
			     (o.write "default: {")
			     (o.write (format "case (" tag "): { // " (sym label))))
			 (o.indent)
			 (emit sub)
			 (o.dedent)
			 (o.write "} break;")
			 (o.dedent)
			 ))
		   (match ealt with
		     (maybe:yes ealt0)
		     -> (begin
			  (o.indent)
			  (o.write "default: {")
			  (o.indent)
			  (emit ealt0)
			  (o.dedent)
			  (o.write "}")
			  (o.dedent))
		     _ -> #u)
		   (o.write "}")))))

    (define (emit-pvcase test-reg tags arities jump-num alts ealt k)
      (o.write (format "switch (get_case_noint (r" (int test-reg) ")) {"))
      (let ((else? (maybe? ealt))
	    (n (length alts)))
	(push-jump-continuation k jump-num)
	(for-range
	    i n
	    (let ((label (nth tags i))
		  (arity (nth arities i))
		  (alt (nth alts i))
		  (tag0 (match (alist/lookup the-context.variant-labels label) with
			  (maybe:yes v) -> v
			  (maybe:no) -> (error1 "variant constructor never called" label)))
		  (tag1 (format (if (= arity 0) "UITAG(" "UOTAG(") (int tag0) ")"))
		  (case0 (format "case (" tag1 "): {"))
		  (case1 (if (and (not else?) (= i (- n 1))) "default: {" case0)))
	      (o.indent)
	      (o.write case1)
	      (o.indent)
	      (emit alt)
	      (o.dedent)
	      (o.write "} break;")
	      (o.dedent)))
	(match ealt with
	  (maybe:yes ealt)
	  -> (begin
	       (o.indent)
	       (o.write (format "default: {"))
	       (o.indent)
	       (emit ealt)
	       (o.dedent)
	       (o.write "};")
	       (o.dedent))
	  (maybe:no) -> #u)
	(o.write "}")))


    ;; emit the top-level insns
    (o.write "void toplevel (void) {")
    (o.indent)
    (when the-context.options.trace (o.write "TRACE(\"toplevel\");"))
    (emit insns)
    (o.dedent)
    (o.write "}")
    ;; now emit all function defns
    (let loop ()
      (match fun-stack with
	() -> #u
	_  -> (begin ((pop fun-stack)) (loop))
	))
    ))
