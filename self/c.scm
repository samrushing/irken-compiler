;; -*- Mode: Irken -*-

(require "self/backend.scm")

;; provide automatic conversions of base types for inputs to %%cexp
(define (wrap-in type arg)
  (match type with
    (type:tvar id _) -> arg
    (type:pred name predargs _)
    -> (match name with
	 'int	       -> (format "UNTAG_INTEGER(" arg ")")
	 'bool         -> (format "IRK_IS_TRUE(" arg ")")
	 'string       -> (format "((irk_string*)(" arg "))->data")
	 'cstring      -> (format "(char*)" arg)
	 'buffer       -> (format "(" (irken-type->c-type type) "(((irk_vector*)" arg ")+1))")
         'cref         -> (format "(" (irken-type->c-type type) ") get_foreign(" arg ")")
	 'arrow	       -> arg
	 'vector       -> arg
	 'symbol       -> arg
	 'char	       -> arg
	 'continuation -> arg
	 'raw	       -> (match predargs with
			    ((type:pred 'string _ _)) -> (format "((irk_string*)(" arg "))")
			    _ -> (error1 "unknown raw type in %cexp" type))
	 kind          -> (if (member-eq? kind c-int-types)
			      (format "unbox(" arg ")")
			      (error1 "wrap-in:" type))
	 )))

;; (buffer (struct sockaddr_t)) => (struct sockaddr_t *)
(define (irken-type->c-type t)

  (define decode-name
    'i8        -> "int8_t"
    'u8        -> "uint8_t"
    'i16       -> "int16_t"
    'u16       -> "uint16_t"
    'i32       -> "int32_t"
    'u32       -> "uint32_t"
    'i64       -> "int64_t"
    'u64       -> "uint64_t"
    'i128      -> "int128_t"
    'u128      -> "uint128_t"
    'i256      -> "int256_t"
    'u256      -> "uint256_t"
    'char      -> "char"
    'uchar     -> "unsigned char"
    'int       -> "int"
    'uint      -> "unsigned int"
    'short     -> "short"
    'ushort    -> "unsigned short"
    'long      -> "long"
    'ulong     -> "unsigned long"
    'longlong  -> "long long"
    'ulonglong -> "unsigned long long"
    x -> (format (sym x)))

  (match t with
    (type:pred 'struct (arg) _) -> (format "struct " (irken-type->c-type arg))
    (type:pred 'cref   (arg) _) -> (format (irken-type->c-type arg) "*")
    (type:pred 'buffer (arg) _) -> (format (irken-type->c-type arg) "*")
    (type:pred '*      (arg) _) -> (format (irken-type->c-type arg) "*")
    (type:pred name () _)       -> (decode-name name)
    (type:tvar _ _)             -> "void"
    _ -> (error1 "malformed ctype" (type-repr t))
    ))

;;
;; ok, for *now*, I don't really want subtyping.  but I *do* want
;;  automatic casting/conversion... what's the cleanest way to get that?
;; We have to deal with both typing and code generation.
;;

(define (wrap-out type exp)
  (match type with
    (type:pred 'int _ _)     -> (format "TAG_INTEGER((irk_int)" exp ")")
    (type:pred 'bool _ _)    -> (format "IRK_TEST(" exp ")")
    (type:pred 'cstring _ _) -> (format "(object*)" exp)
    (type:pred 'cref _ _)    -> (format "(make_foreign((void*)" exp "))")
    (type:pred '* _ _)       -> (format "(make_foreign((void*)" exp "))")
    (type:pred 'void _ _)    -> (format "((" exp "), IRK_UNDEFINED)")
    (type:pred kind _ _)     -> (if (member-eq? kind c-int-types)
				    (format "box((irk_int)" exp ")")
				    exp)
    _                        -> exp
    ))

;; substitute <values> into <template>, e.g. "%0 + %1" ("unbox(r3)" "unbox(r5)") => "r3
(define (cexp-subst template values)
  (let ((split (string-split template #\%)))
    (let loop ((r (list (car split)))
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


(define (emit-c-lookup-field-hashtables o)
  (let ((ambig (build-ambig-table))
        (size (tree/size ambig))
        (table (make-vector size {k0=0 k1=0 v=0}))
        (i 0))
    (tree/inorder
     (lambda (k v)
       (match k with
         (:tuple tag label)
         -> (set! table[i] {k0= tag k1=label v=v}))
       (set! i (+ i 1)))
     ambig)
    (let (((G V) (create-minimal-perfect-hash table)))
      (oformat "uint32_t irk_ambig_size = " (int size) ";")
      (o.copy "int32_t G[] = {")
      (for-vector val G
        (o.copy (format (int val) ", ")))
      (oformat "0};")
      (o.copy "int32_t V[] = {")
      (for-vector val V
        (o.copy (format (int val) ", ")))
      (oformat "0};")
      )))

(define (emit-get-metadata o)
  (o.write
   (format
    "object * irk_get_metadata (void) {\n"
    "  return (object *) constructed_" (int (- the-context.literals.count 1)) "[0];\n"
    "}\n")))

(define (emit-c o decls insns)

  (let ((fun-stack '())
	(current-function-cname "")
	(current-function-name 'toplevel)
	(current-function-part (make-counter 1))
	(env-counter (make-counter 0))
	(env-stack '())
	(used-jumps (find-jumps insns))
	(fatbar-free (map-maker int-cmp))
	(declared (set2-maker string-compare)))

    (define emit
      (insn:return target)                         -> (o.write (format "IRK_RETURN(" (int target) ");"))
      (insn:literal lit k)                         -> (begin (emit-literal lit k.target) (emit k.insn))
      (insn:litcon i kind k)                       -> (begin (emit-litcon i kind k.target) (emit k.insn))
      (insn:test reg jn k0 k1 k)                   -> (emit-test reg jn k0 k1 k)
      (insn:testcexp regs sig tmpl jn k0 k1 k)     -> (emit-testcexp regs sig tmpl jn k0 k1 k)
      (insn:jump reg target jn free)               -> (emit-jump reg target jn free.val)
      (insn:cexp sig type template args k)         -> (begin (emit-cexp sig type template args k.target) (emit k.insn))
      (insn:ffi sig type name args k)              -> (error1 "no FFI in C backend" name)
      (insn:close name nreg body k)                -> (begin (emit-close name nreg body k.target) (emit k.insn))
      (insn:varref d i k)                          -> (begin (emit-varref d i k.target) (emit k.insn))
      (insn:varset d i v k)                        -> (begin (emit-varset d i v k.target) (emit k.insn))
      (insn:new-env size top? types k)             -> (begin (emit-new-env size top? types k.target) (emit k.insn))
      (insn:alloc tag size k)                      -> (begin (emit-alloc tag size k.target) (emit k.insn))
      (insn:store off arg tup i k)                 -> (begin (emit-store off arg tup i) (emit k.insn))
      (insn:invoke name fun args k)                -> (emit-call name fun args k)
      (insn:tail name fun args)                    -> (emit-tail name fun args)
      (insn:trcall d n args)                       -> (emit-trcall d n args)
      (insn:push r k)                              -> (begin (emit-push r) (emit k.insn))
      (insn:pop r k)                               -> (begin (emit-pop r k.target) (emit k.insn))
      (insn:primop name parm t args k)             -> (begin (emit-primop name parm t args k) (emit k.insn))
      (insn:move dst var k)                        -> (begin (emit-move dst var k.target) (emit k.insn))
      (insn:fatbar lab jn k0 k1 k)                 -> (emit-fatbar lab jn k0 k1 k)
      (insn:fail label npop free)                  -> (emit-fail label npop free.val)
      (insn:nvcase tr dt tags jn alts ealt k)      -> (emit-nvcase tr dt tags jn alts ealt k)
      (insn:pvcase tr tags arities jn alts ealt k) -> (emit-pvcase tr tags arities jn alts ealt k)
      (insn:label label next)                      -> (emit next)
      )

    ;; XXX arrange to avoid duplicates caused by jump conts
    (define (declare-function name extern? kfun?)
      (when (not (declared::member name))
	    (declared::add name)
	    (let ((linkage (if extern? "extern" "static")))
	      (decls.write (format linkage " void " name (if kfun? "(O);" "(void);"))))))

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
          (match kind with
            'string -> (o.write (format "O r" (int target) " = (O) &constructed_" (int index) ";"))
            'record -> (o.write (format "O r" (int target) " = irk_copy_tuple ((O) constructed_" (int index) "[0]);"))
            'vector -> (o.write (format "O r" (int target) " = irk_copy_tuple ((O) constructed_" (int index) "[0]);"))
            _       -> (o.write (format "O r" (int target) " = (object*) constructed_" (int index) "[0];"))
            )))

    (define (emit-test reg jn k0 k1 k)
      (push-jump-continuation k jn)
      (o.write (format "if IRK_IS_TRUE(r" (int reg)") {"))
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
	     (o.write (format "if IRK_IS_TRUE(" exp ") {"))
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
	  -> (begin (printf "jump " (int jump-num) "\n") (impossible))
          )))

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

    (define (emit-profile o name call?)
      (match (tree/member the-context.profile-funs symbol-index-cmp name) with
        (maybe:yes {index=index names=_})
        -> (o.write
            (format "if (prof_current_fun != " (int (+ index 1)) ") {\n"
                    "    prof_mark1 = rdtsc();\n"
                    ;; charge to the calling function
                    "    prof_funs[prof_current_fun].ticks += (prof_mark1 - prof_mark0);\n"
                    ;; set the current function (note: 'top' is at position zero)
                    "    prof_current_fun = " (int (+ index 1)) ";\n"
                    (if call?
                        "    prof_funs[prof_current_fun].calls++;\n"
                        "")
                    "  }"))
        (maybe:no) -> (begin
                        (printf "missing profile fun? " (sym name) "\n")
                        (impossible))
        ))

    (define (emit-cfun o name cname body)
      (set! current-function-name name)
      (set! current-function-cname cname)
      (o.write (format "static void " cname " (void) {"))
      (o.indent)
      (when the-context.options.trace (o.write (format "TRACE(\"" cname "\");")))
      (when the-context.options.profile (emit-profile o name #t))
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
	(declare-function cname #f #f)
	(push! fun-stack (lambda () (emit-cfun o name cname body)))
	(o.write (format "O r" (int target) " = allocate (TC_CLOSURE, 2);"))
	(o.write (format "r" (int target) "[1] = " cname "; r" (int target) "[2] = lenv;"))
	))

    (define (push-continuation cname insn args)
      (let ((args (format (join (lambda (x) (format "O r" (int x))) ", " args))))
	(push! fun-stack
	      (lambda ()
		(o.write (format "static void " cname "(" args ") {"))
		(o.indent)
		(when the-context.options.trace (o.write (format "TRACE(\"" cname "\");")))
                (when the-context.options.profile (emit-profile o current-function-name #f))
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
	     (push-continuation (format "JUMP_" (int jump)) cont.insn free)
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
      (let ((tag-string (format (if (= size 0) "UITAG(" "UOTAG(") (int tag) ")")))
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
        (maybe:no) -> (format "((pfun)(r" (int funreg) "[1]))();") ;; unknown
        (maybe:yes name)
        -> (let ((var (vars-get-var name)))
             (if (= 0 var.sets)
                 (let ((cfun (gen-function-cname name 0))) ;; known
                   ;; include last-minute forward declaration
                   (declare-function cfun #f #f)
                   (format cfun "();")
                   )
                 (format "((pfun)(r" (int funreg) "[1]))();") ;; mutated
                 ))
        ))

    (define (emit-tail name fun args)
      (let ((funcall (format-call name fun)))
	(if (>= args 0)
	    (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " funcall))
	    (o.write (format "lenv = r" (int fun) "[2]; " funcall))
	    )))

    (define (emit-call name fun args k)
      (let ((free (sort < k.free)) ;; sorting these might improve things
	    (nregs (length free))
            (current-name current-function-name)
	    (kfun (gen-function-cname current-function-name (current-function-part.inc)))
	    )
	;; save
	(o.write (format "O t = allocate (TC_SAVE, " (int (+ 3 nregs)) ");"))
	(let ((saves
	       (map-range
		   i nregs
		   (format "t[" (int (+ i 4)) "] = r" (int (nth free i))))))
	  (declare-function kfun #f #t)
	  (o.write (format "t[1] = k; t[2] = lenv; t[3] = " kfun "; " (string-join saves "; ") "; k = t;")))
	;; call
	(let ((funcall (format-call name fun)))
	  (if (>= args 0)
	      (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " funcall))
	      (o.write (format "lenv = r" (int fun) "[2]; " funcall))))
	;; emit a new c function to represent the continuation of the current irken function
	(push! fun-stack
	      (lambda ()
		(set! current-function-cname kfun)
		(o.write (format "static void " kfun " (O rr) {"))
		(o.indent)
                (when the-context.options.profile (emit-profile o current-name #f))
		;; restore
		(let ((restores
		       (map-range
			   i nregs
			   (format "O r" (int (nth free i)) " = k[" (int (+ i 4)) "]"))))
		  (o.write (format (string-join restores "; ") "; lenv = k[2]; k = k[1];")))
		(if (>= k.target 0)
		    (o.write (format "O r" (int k.target) " = rr;")))
		(emit k.insn)
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
	(declare-function cname #f #f)
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
	'bool 'true -> "(irk_int)IRK_TRUE"
	'bool 'false -> "(irk_int)IRK_FALSE"
	_ _ -> (format "UITAG(" (int index) ")")))

    (define (emit-primop name parm type args k)

      ;; this *really* needs the print-context-of-error thing that typing.scm uses.
      (define (primop-error)
	(error1 "primop" name))

      (let ((nargs (length args)))

        (define prim-dtcon
          (sexp:cons dtname altname)
          -> (match (alist/lookup the-context.datatypes dtname) with
               (maybe:no) -> (error1 "emit-primop: no such datatype" dtname)
               (maybe:yes dt)
               -> (let ((alt (dt.get altname)))
                    (cond ((= nargs 0)
                           (o.write (format "O r" (int k.target) " = (object*)"
                                            (get-uitag dtname altname alt.index) ";")))
                          (else
                           (if (>= k.target 0)
                               (let ((trg (format "r" (int k.target))))
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
          -> (o.write (format "O r" (int k.target) " = UOBJ_GET(r" (int reg) "," (int index) ");"))
          _ _ -> (primop-error))

        (define prim-make-vector
          (vlen vval)
          -> (begin
               ;; since we cannot know the size at compile-time, there should
               ;; always be a call to ensure_heap() before any call to %make-vector
               (o.write (format "O r" (int k.target) ";"))
               (o.write (format "if (unbox(r" (int vlen) ") == 0) { r" (int k.target) " = (object *) TC_EMPTY_VECTOR; } else {"))
               (o.write (format "  O t = alloc_no_clear (TC_VECTOR, unbox(r" (int vlen) "));"))
               (o.write (format "  for (int i=0; i<unbox(r" (int vlen) "); i++) { t[i+1] = r" (int vval) "; }"))
               (o.write (format "  r" (int k.target) " = t;"))
               (o.write "}"))
          _ -> (primop-error))

        (define prim-array-ref
          (vec index)
          -> (when (not (= -1 k.target))
               (o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
               (o.write (format "O r" (int k.target) " = ((irk_vector*)r" (int vec) ")->val[unbox(r" (int index) ")];")))
          _ -> (primop-error))

        (define prim-array-set
          (vec index val)
          -> (begin
               (o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
               (o.write (format "((irk_vector*)r" (int vec) ")->val[unbox(r" (int index) ")] = r" (int val) ";"))
               (when (>= k.target 0)
                 (o.write (format "O r" (int k.target) " = (object *) TC_UNDEFINED;"))))
          _ -> (primop-error))

        (define (ambig code)
          (tree/insert! the-context.ambig-rec int-cmp code #u))

        (define prim-record-get
          (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg)
          -> (let ((label-code (lookup-label-code label)))
               (match (guess-record-type sig) with
                 (maybe:yes sig0)
                 -> (o.write (format "O r" (int k.target) ;; compile-time lookup
                                     " = ((irk_vector*)r" (int rec-reg)
                                     ")->val[" (int (index-eq label sig0))
                                     "];"))
                 (maybe:no)
                 -> (begin
                      (o.write (format "O r" (int k.target) ;; run-time lookup
                                       " = ((irk_vector*)r" (int rec-reg)
                                       ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
                                       ")-TC_USEROBJ)>>2," (int label-code)
                                       ")]; // label=" (sym label)))
                      (ambig label-code)
                      )))
          _ _ -> (primop-error))

        ;; XXX very similar to record-get, maybe some way to collapse the code?
        (define prim-record-set
          (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg arg-reg)
          -> (let ((label-code (lookup-label-code label)))
               (match (guess-record-type sig) with
                 (maybe:yes sig0)
                 -> (o.write (format "((irk_vector*)r" (int rec-reg) ;; compile-time lookup
                                     ")->val[" (int (index-eq label sig0))
                                     "] = r" (int arg-reg) ";"))
                 (maybe:no)
                 -> (begin
                      (o.write (format "((irk_vector*)r" (int rec-reg) ;; run-time lookup
                                       ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
                                       ")-TC_USEROBJ)>>2," (int label-code)
                                       ")] = r" (int arg-reg) ";"))
                      (ambig label-code)
                      ))
               (when (>= k.target 0)
                 (o.write (format "O r" (int k.target) " = (object *) TC_UNDEFINED;"))))
          _ _ -> (primop-error))

        (define (prim-callocate parm args)
          (let ((type (parse-type parm))) ;; gets parsed twice, convert to %%cexp?
            ;; XXX maybe make alloc_no_clear do an ensure_heap itself?
            (if (>= k.target 0)
                (o.write (format "O r" (int k.target) " = alloc_no_clear (TC_BUFFER, HOW_MANY (sizeof (" (irken-type->c-type type)
                                 ") * unbox(r" (int (car args)) "), sizeof (object)));"))
                (error1 "%callocate: dead target?" type))))

        (define (prim-malloc parm args)
          (let ((type (parse-type parm)))
            (cond ((>= k.target 0)
                   (o.write (format "O r" (int k.target) " = make_foreign (malloc (sizeof ("
                                    (irken-type->c-type type) ") * unbox(r" (int (car args)) ")));")))
                  (else
                   (error1 "%malloc: dead target?" type)))))

        (define (prim-halloc parm args)
          (let ((type (parse-type parm)))
            (cond ((>= k.target 0)
                   (o.write (format "O r" (int k.target)
                                    " = make_halloc (sizeof ("
                                    (irken-type->c-type type) ") , unbox(r" (int (car args))
                                    "));")))
                  (else
                   (error1 "%malloc: dead target?" type)))))

        (define (prim-free args)
          (o.write (format "free_foreign (r" (int (car args)) ");"))
          (when (>= k.target 0)
            (o.write (format "O r" (int k.target) " = IRK_UNDEFINED;"))))

        (define (prim-exit args)
          (o.write (format "exit_continuation(r" (int (car args)) ");"))
          (when (> k.target 0)
            (o.write (format "O r" (int k.target) " = (object *) TC_UNDEFINED;"))))

        (define prim-getcc
          () -> (o.write (format "O r" (int k.target) " = k; // %getcc"))
          _  -> (primop-error))

        (define prim-putcc
          (rk rv) -> (begin
                       (o.write (format "k = r" (int rk) "; // %putcc"))
                       (move rv k.target))
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
                          (call1 (if (= k.target -1) call0 (wrap-out type call0))))
                      (if (= k.target -1)
                          (o.write (format call1 ";"))
                          (o.write (format "O r" (int k.target) " = " call1 ";"))))
                 (maybe:yes (csig:obj name obtype))
                 -> (o.write (format "O r" (int k.target) " = make_foreign ((void*)&" (sym name) ");"))
                 _ -> (primop-error))
            _ -> (primop-error)
            ))

        (define (prim-c-aref args)
          (match args type with
            (src index) (type:pred 'cref (subtype) _)
            -> (let ((ctype (irken-type->c-type subtype)))
                 (o.write (format "// ctype = " ctype))
                 (o.write (format "O r" (int k.target)
                                  " = offset_foreign (r" (int src)", sizeof(" ctype
                                  ") * UNTAG_INTEGER(r" (int index)"));")))
            _ _ -> (primop-error)))

        (define prim-c-get-ptr
          (src)
          -> (begin
               (o.write (format "// %c-get-ptr"))
               (o.write (format "O r" (int k.target) " = make_foreign (*(void**)get_foreign (r" (int src) "));")))
          _ -> (primop-error)
          )

        (define prim-c-set-ptr
          (dst src)
          -> (begin
               (o.write (format "// %c-set-ptr"))
               (o.write (format "*((void**)get_foreign (r" (int dst) ")) = get_foreign (r" (int src) ");"))
               (when (> k.target 0)
                 (o.write (format "O r" (int k.target) " = (object *) TC_UNDEFINED;"))))
          _ -> (primop-error)
          )

        (define (prim-c-sizeof ctexp)
          (let ((t0 (parse-type ctexp))
                (t1 (irken-type->c-type t0)))
            (o.write (format "O r" (int k.target) " = TAG_INTEGER (sizeof (" t1 "));"))))

        ;; generic version, hopefully we can make this work.
        ;; things we can do:
        ;;   int (char,short,...)
        ;;   float (double, etc... TBD)
        ;;   pointer
        ;; things we can't do:
        ;;   struct
        ;;   union

        (define prim-c-get
          (src)
          -> (match type with
               (type:pred 'struct _ _)
               -> (primop-error)
               (type:pred '* _ _)
               -> (o.write (format "O r" (int k.target) " = make_foreign (*(void**)get_foreign (r" (int src) "));"))
               int-type
               -> (o.write (format "O r" (int k.target) " = TAG_INTEGER((irk_int)*(("
                                   (irken-type->c-type int-type) "*)get_foreign(r" (int src) ")));"))
               )
          _ -> (primop-error))

        (define prim-c-set
          (dst src)
          -> (match type with
               (type:pred 'struct _ _)
               -> (primop-error)
               (type:pred '* _ _)
               -> (o.write (format "*((void**)get_foreign (r" (int dst) ")) = get_foreign (r" (int src) ");"))
               int-type
               ;; XXX here's the trick - getting the C type.
               -> (let ((ctype (irken-type->c-type type)))
                    (printf "prim-c-set ctype = " ctype " type = " (type-repr type) "\n")
                    (o.write (format "*((" ctype "*)get_foreign(r" (int dst) ")) = (" ctype ") UNTAG_INTEGER(r" (int src) ");"))
                    (when (> k.target 0)
                      (o.write (format "O r" (int k.target) " = (object *) TC_UNDEFINED;")))
                    )
               )
          _ -> (primop-error))

        (define prim-c-get-int
          (src)
          -> (o.write
              (format "O r" (int k.target) " = TAG_INTEGER((irk_int)*(("
                      (irken-type->c-type type) "*)get_foreign(r" (int src) ")));"))
          _ -> (primop-error))

        ;; note: this relies on a hack in cps.scm that sets the type of this insn to the type of the rval.
        (define prim-c-set-int
          (dst src)
          -> (let ((ctype (irken-type->c-type type)))
               ;; XXX check against word size
               (printf "c-set-int " ctype "\n")
               (o.write (format "*((" ctype "*)get_foreign(r" (int dst) ")) = (" ctype ") UNTAG_INTEGER(r" (int src) ");"))
               (when (> k.target 0)
                 (o.write (format "O r" (int k.target) " = (object *) TC_UNDEFINED;"))))
          _ -> (primop-error))

        (define sref->c
          ;; structname.field0.field1.field2...
          (sexp:attr (sexp:symbol sname) fname) src
          -> (format "((((struct " (sym sname) "*)" src "))->" (sym fname) ")")
          (sexp:attr sub fname) src
          -> (format "(" (sref->c sub src) "." (sym fname) ")")
          _ _ -> (impossible)
          )

        (define prim-c-sref
          refexp (src)
          -> (o.write (format "O r" (int k.target) " = offset_foreign (r" (int src)
                              ;; this is very similar to the offsetof macro
                              ", ((size_t)&(" (sref->c refexp "0") ")));"))
          _ _ -> (primop-error))

        (define prim-c-sfromc
          (src len)
          -> (begin
               (o.write (format "O r" (int k.target) " = make_string (UNTAG_INTEGER (r" (int len) "));"))
               (o.write (format "memcpy (GET_STRING_POINTER (r"
                                (int k.target) "), get_foreign (r" (int src)
                                "), UNTAG_INTEGER (r" (int len) "));")))
          _ -> (primop-error))

        (define prim-string->cref
          (src) -> (o.write (format "O r" (int k.target) " = make_foreign (GET_STRING_POINTER (r" (int src) "));"))
          _ -> (primop-error))

        ;; essentially a no-op.
        (define prim-c-cast
          (src) -> (o.write (format "O r" (int k.target) " = r" (int src) ";"))
          _ -> (primop-error))

        (define prim-cref->int
          (src) -> (o.write (format "O r" (int k.target) " = TAG_INTEGER (((irk_int) get_foreign (r" (int src) ")));"))
          _ -> (primop-error))

        (define prim-int->cref
          (addr) -> (o.write (format "O r" (int k.target) " = make_foreign ((void*)UNTAG_INTEGER (r" (int addr) "));"))
          _ -> (primop-error))

        (match name with
          '%dtcon        -> (prim-dtcon parm)
          '%nvget        -> (prim-nvget parm args)
          '%make-vector  -> (prim-make-vector args)
          '%array-ref    -> (prim-array-ref args)
          '%array-set    -> (prim-array-set args)
          '%record-get   -> (prim-record-get parm args)
          '%record-set   -> (prim-record-set parm args)
          '%callocate    -> (prim-callocate parm args)
          '%exit         -> (prim-exit args)
          '%getcc        -> (prim-getcc args)
          '%putcc        -> (prim-putcc args)
          '%ensure-heap  -> (emit-check-heap k.free (format "unbox(r" (int (car args)) ")"))
          ;; --------------- FFI ---------------
          '%malloc       -> (prim-malloc parm args)
          '%halloc       -> (prim-halloc parm args)
          '%free         -> (prim-free args)
          '%ffi2         -> (prim-ffi2 parm args)
          '%c-aref       -> (prim-c-aref args)
          '%c-get-int    -> (prim-c-get-int args)
          '%c-set-int    -> (prim-c-set-int args)
          '%c-get-ptr    -> (prim-c-get-ptr args)
          '%c-set-ptr    -> (prim-c-set-ptr args)
          ;; experimental
          '%c-get        -> (prim-c-get args)
          '%c-set        -> (prim-c-set args)

          '%c-sref       -> (prim-c-sref parm args)
          ;;'%c-uref     -> (prim-c-uref parm args)
          '%cref->string -> (prim-c-sfromc args)
          '%string->cref -> (prim-string->cref args)
          '%c-sizeof     -> (prim-c-sizeof parm)
          '%cref->int    -> (prim-cref->int args)
          '%int->cref    -> (prim-int->cref args)
          ;; -----------------------------------
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
      (match (used-jumps::get label) with
        (maybe:yes free) -> (begin
                              (fatbar-free::add label free)
                              (push-fail-continuation k1 label free)
                              (push-jump-continuation k jn)
                              (o.write (format "// fatbar jn=" (int jn) " label=" (int label)))
                              (emit k0))
        _                -> (begin (printf "sucks dude: jn:"(int jn) " fail:" (int label) "\n") (impossible))
        ))

    (define (emit-fail label npop free)
      (if (> npop 0)
	  (o.write (format "lenv = ((object " (join (n-of npop "*")) ")lenv)" (join (n-of npop "[1]")) ";")))
      (let ((jname (format "FAIL_" (int label))))
	(match (fatbar-free::get label) with
	  (maybe:yes free0)
	  -> (begin
	       (o.write (format jname "(" (join (lambda (x) (format "r" (int x))) ", " free) ");"))
	       (decls.write (format "static void " jname "(" (string-join (n-of (length free) "O") ", ") ");")))
	  (maybe:no)
	  -> (begin (printf "fail " (int label) "\n") (impossible))
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
		 ;;   (emit k.insn) ;; and continue...
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
	_  -> (begin ((pop! fun-stack)) (loop))
	))
    (emit-get-metadata o)
    (emit-c-lookup-field-hashtables o)
    ))

(define (compile-to-c base cps)
  (let ((opath (string-append base ".c"))
        (ofile (file/open-write opath #t #o644))
        (o (make-writer ofile))
        (tmp-path (format base ".tmp.c"))
        (tfile (file/open-write tmp-path #t #o644))
        (o0 (make-writer tfile)))
    (notquiet (printf "\n-- C output --\n : " opath "\n"))
    (for-each
     (lambda (path) (o.write (format "#include <" path ">")))
     (reverse the-context.cincludes))
    (for-each
     (lambda (path) (o.write (format "#include \"" path "\"")))
     (reverse the-context.lincludes))
    (for-each o.write (reverse the-context.cverbatim))
    (o.copy (get-file-contents "include/header1.c"))
    (emit-constructed o)
    (emit-datatype-table o)
    (number-profile-funs)
    (if the-context.options.profile
        (emit-profile-0 o)
        (o.write "void prof_dump (void) {}"))
    (emit-c o0 o cps)
    (if the-context.options.profile (emit-profile-1 o))
    (notquiet (print-string "done.\n"))
    (o0.close)
    ;; copy code after declarations
    (copy-file-contents o tmp-path)
    (o.close)
    (notquiet (printf "wrote " (int (o.get-total)) " bytes to " opath ".\n"))
    (unlink tmp-path)
    (list opath)))
