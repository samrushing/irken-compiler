;; -*- Mode: Irken -*-

(include "self/cps.scm")
(include "self/typing.scm")
(include "self/graph.scm")
(include "self/analyze.scm")

(define (make-writer file)
  (let ((level 0))
    (define (write-string s)
      (write file.fd
	     (format (repeat level "  ") s "\n"))
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

(define frob-name (make-name-frobber))

(define (gen-function-label sym)
  (format "FUN_" (frob-name (symbol->string sym))))

(define label-maker
  (let ((counter (make-counter 0)))
    (lambda ()
      (format "L" (int (counter.inc))))))

(define encode-immediate
  (literal:int n)   -> (logior 1 (<< n 1))
  (literal:char ch) -> (logior 2 (<< (char->ascii ch) 8))
  (literal:undef)   -> #x0e
  (literal:cons 'bool 'true _) -> #x106
  (literal:cons 'bool 'false _) -> #x006
  x -> (error1 "expected immediate literal " x))

(define (wrap-in type arg)
  (match type with
    (type:tvar id _) -> arg
    (type:pred name predargs _)
    -> (match name with
	 'int	       -> (format "unbox(" arg ")")
	 'string       -> (format "((pxll_string*)(" arg "))->data")
	 'cstring      -> (format "(char*)" arg)
	 'buffer       -> (format "(" (irken-type->c-type type) "(((pxll_vector*)" arg ")+1))")
	 'ptr	       -> arg
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
    (type:pred 'buffer (arg) _)	-> (format "(" (irken-type->c-type arg) "*)")
    (type:pred 'struct (arg) _) -> (format "struct " (irken-type->c-type arg))
    (type:pred name () _)	-> (format (sym name))
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
    (type:pred 'int _ _)     -> (format "box((pxll_int)" exp ")")
    (type:pred 'bool _ _)    -> (format "PXLL_TEST(" exp ")")
    (type:pred 'cstring _ _) -> (format "(object*)" exp)
    (type:pred 'ptr _ _)     -> (format "(object*)" exp)
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

(define (emit o insns context)

  (let ((fun-counter 1)
	(fun-stack '(0)))

    (define emitk
      (cont:k _ _ k) -> (emit k)
      (cont:nil)	   -> #u)

    (define (emit insn)
      (emitk
       (match insn with
	 (insn:return target)			   -> (begin (o.write (format "PXLL_RETURN(" (int target) ");")) (cont:nil))
	 (insn:literal lit k)			   -> (begin (emit-literal lit (k/target k)) k)
	 (insn:litcon i kind k)			   -> (begin (emit-litcon i kind (k/target k)) k)
	 (insn:test reg k0 k1 k)		   -> (begin (emit-test reg k0 k1) k)
	 (insn:testcexp regs sig tmpl k0 k1 k)	   -> (begin (emit-testcexp regs sig tmpl k0 k1) k)
	 (insn:jump reg target)			   -> (begin (emit-jump reg target) (cont:nil))
	 (insn:cexp sig type template args k)      -> (begin (emit-cexp sig type template args (k/target k)) k)
	 (insn:close name body k)		   -> (begin (emit-close name body (k/target k)) k)
	 (insn:varref d i k)			   -> (begin (emit-varref d i (k/target k)) k)
	 (insn:varset d i v k)			   -> (begin (emit-varset d i v) k)
	 (insn:new-env size top? k)		   -> (begin (emit-new-env size top? (k/target k)) k)
	 (insn:alloc tag size k)		   -> (begin (emit-alloc tag size (k/target k)) k)
	 (insn:store off arg tup i k)		   -> (begin (emit-store off arg tup i) k)
	 (insn:invoke name fun args k)		   -> (begin (emit-call name fun args k) k)
	 (insn:tail name fun args)		   -> (begin (emit-tail name fun args) (cont:nil))
	 (insn:trcall d n args)			   -> (begin (emit-trcall d n args) (cont:nil))
	 (insn:push r k)			   -> (begin (emit-push r) k)
	 (insn:pop r k)				   -> (begin (emit-pop r (k/target k)) k)
	 (insn:primop name parm t args k)	   -> (begin (emit-primop name parm t args k) k)
	 (insn:move dst var k)			   -> (begin (emit-move dst var (k/target k)) k)
	 (insn:fatbar lab k0 k1 k)		   -> (begin (emit-fatbar lab k0 k1) k)
	 (insn:fail label npop)			   -> (begin (emit-fail label npop) (cont:nil))
	 (insn:nvcase tr dt tags alts ealt k)	   -> (begin (emit-nvcase tr dt tags alts ealt) k)
	 (insn:pvcase tr tags arities alts ealt k) -> (begin (emit-pvcase tr tags arities alts ealt) k)
	 )))

    (define (move src dst)
      (if (and (>= dst 0) (not (= src dst)))
	  (o.write (format "r" (int dst) " = r" (int src) ";"))))

    (define (emit-literal lit target)
      (let ((val (encode-immediate lit))
	    (prefix (if (= target -1)
			"// dead " ;; why bother with a dead literal?
			(format "r" (int target)))))
	(o.write (format prefix " = (object *) " (int val) ";"))
	))

    (define (emit-litcon index kind target)
      (if (>= target 0)
	  (cond ((eq? kind 'string)
		 (o.write (format "r" (int target) " = (object*) &constructed_" (int index) ";")))
		(else
		 (o.write (format "r" (int target) " = (object *) constructed_" (int index) "[0];"))))))

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
	-> (let ((args0 (map (lambda (reg) (format "r" (int reg))) args))
		 (args1 (map2 wrap-in arg-types args0))
		 (exp (wrap-out result-type (cexp-subst template args1))))
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
      (move reg target))

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
	    (o.write (format "r" (int target) " = " exp ";")))))

    (define (current-fun-index)
      (nth fun-stack 0))

    (define (emit-close name body target)
      (let ((proc-label (gen-function-label name))
	    (jump-label (label-maker)))
	(PUSH fun-stack fun-counter)
	(set! fun-counter (+ 1 fun-counter))
	;; emit a jump over the function definition
	(o.write (format "// def " (sym name)))
	(o.write (format "goto " jump-label ";"))
	;; emit the function definition
	(o.write (format proc-label ":"))
	(o.indent)
	;; XXX context flag for this...
	(if context.options.trace
	    (o.write (format "stack_depth_indent(k); fprintf (stderr, \">> [%d] " proc-label "\\n\", __LINE__);")))
	;; profile
	(when context.options.profile
	      (o.write "prof_mark1 = rdtsc();")
	      ;; charge to the calling function
	      (o.write "prof_funs[prof_current_fun].ticks += (prof_mark1 - prof_mark0);")
	      ;; set the current function
	      (o.write (format "prof_current_fun = " (int (car fun-stack)) ";"))
	      (o.write "prof_funs[prof_current_fun].calls++;"))
	(if (vars-get-flag context name VFLAG-ALLOCATES)
	    (o.write "check_heap (0);"))
	(when context.options.profile
	      ;; this avoids charging gc to this fun
	      (o.write "prof_mark0 = rdtsc();"))
	(emit body)
	(pop fun-stack)
	(o.dedent)
	(o.write (format jump-label ":"))
	(o.write (format "r" (int target) " = allocate (TC_CLOSURE, 2);"))
	(o.write (format "r" (int target) "[1] = &&" proc-label "; r" (int target) "[2] = lenv;"))
	))

    (define (emit-varref d i target)
      (if (>= target 0)
	  (let ((src 
		 (if (= d -1)
		     (format "top[" (int (+ 2 i)) "];") ;; the +2 is to skip the header and next ptr
		     ;;(format "varref (" (int d) "," (int i) ");")
		     (format "((object*" (repeat d "*") ") lenv) " (repeat d "[1]") "[" (int (+ i 2)) "];")
		     )))
	    (o.write (format "r" (int target) " = " src)))))

    (define (emit-varset d i v)
      (if (= d -1)
	  (o.write (format "top[" (int (+ 2 i)) "] = r" (int v) ";"))
	  ;;(o.write (format "varset (" (int d) ", " (int i) ", r" (int v) ");"))
	  (o.write (format "((object*" (repeat d "*") ") lenv) " (repeat d "[1]") "[" (int (+ i 2)) "] = r" (int v) ";"))
	  ))

    (define (emit-new-env size top? target)
      (o.write (format "r" (int target) " = allocate (TC_TUPLE, " (int (+ size 1)) ");"))
      (if top?
	  (o.write (format "top = r" (int target) ";"))))

    (define (emit-alloc tag size target)
      (let ((tag-string
	     (match tag with
	       (tag:bare v) -> (format (int v))
	       (tag:uobj v) -> (format (if (= size 0) "UITAG(" "UOTAG(") (int v) ")"))))
	(if (= size 0)
	    ;; unit type - use an immediate
	    (o.write (format "r" (int target) " = (object*)" tag-string ";"))
	    (o.write (format "r" (int target) " = allocate (" tag-string ", " (int size) ");")))))

    (define (emit-store off arg tup i)
      (o.write (format "r" (int tup) "[" (int (+ 1 (+ i off))) "] = r" (int arg) ";")))

    (define (emit-tail name fun args)
      (let ((goto
	     (match name with
	       (maybe:no)	      -> (format "goto *r" (int fun) "[1];")
	       (maybe:yes name) -> (format "goto " (gen-function-label name) ";"))))
	(if (>= args 0)
	    (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto))
	    (o.write (format "lenv = r" (int fun) "[2]; " goto))
	    )))

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
	      (o.write (format "r" (int args) "[1] = r" (int fun) "[2]; lenv = r" (int args) "; " goto))
	      (o.write (format "lenv = r" (int fun) "[2]; " goto))))
	;; label
	(o.write (format return-label ":"))
	;; profile
	(when context.options.profile
	      (o.write "prof_mark1 = rdtsc();")
	      (o.write "prof_funs[prof_current_fun].ticks += (prof_mark1 - prof_mark0);")
	      (o.write "prof_mark1 = prof_mark0;")
	      (o.write (format "prof_current_fun = " (int (car fun-stack)) ";")))
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
      (move src target))

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

      (define (primop-error)
	(error1 "primop" name))

      (let ((target (k/target k))
	    (nargs (length args)))
	;; these need to be broken up into separate functions...
	(match name with
	  '%dtcon  -> (match parm with
			(sexp:cons dtname altname)
			-> (match (alist/lookup context.datatypes dtname) with
			     (maybe:no) -> (error1 "emit-primop: no such datatype" dtname)
			     (maybe:yes dt)
			     -> (let ((alt (dt.get altname)))
				  (cond ((= nargs 0)
					 (o.write (format "r" (int target) " = (object*)" (get-uitag dtname altname alt.index) ";")))
					(else
					 (o.write (format "t = alloc_no_clear (" (get-uotag dtname altname alt.index) "," (int nargs) ");"))
					 (for-range
					     i nargs
					     (o.write (format "t[" (int (+ i 1)) "] = r" (int (nth args i)) ";")))
					 (o.write (format "r" (int target) " = t;"))))))
			_ -> (primop-error)
			)
	  '%nvget   -> (match parm args with
			 (sexp:list (_ (sexp:int index) _)) (reg)
			 -> (o.write (format "r" (int target) " = UOBJ_GET(r" (int reg) "," (int index) ");"))
			 _ _ -> (primop-error))
	  '%make-vector -> (match args with
			     (vlen vval)
			     -> (begin
				  ;; since we cannot know the size at compile-time, there should
				  ;; always be a call to ensure_heap() before any call to %make-vector
				  (o.write (format "if (unbox(r" (int vlen) ") == 0) { r" (int target) " = (object *) TC_EMPTY_VECTOR; } else {"))
				  (o.write (format "  t = alloc_no_clear (TC_VECTOR, unbox(r" (int vlen) "));"))
				  (o.write (format "  for (i=0; i<unbox(r" (int vlen) "); i++) { t[i+1] = r" (int vval) "; }"))
				  (o.write (format "  r" (int target) " = t;"))
				  (o.write "}"))
			     _ -> (primop-error))
	  '%array-ref -> (match args with
			   (vec index)
			   -> (begin
				(o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
				(o.write (format "r" (int target) " = ((pxll_vector*)r" (int vec) ")->val[unbox(r" (int index) ")];")))
			   _ -> (primop-error))
	  '%array-set -> (match args with
			   (vec index val)
			   -> (begin
				(o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
				(o.write (format "((pxll_vector*)r" (int vec) ")->val[unbox(r" (int index) ")] = r" (int val) ";")))
			   _ -> (primop-error))
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
	  ;; XXX very similar to record-get, maybe some way to collapse the code?
	  '%record-set -> (match parm args with
			    (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg arg-reg)
			    -> (let ((label-code (lookup-label-code label context)))
				 (match (guess-record-type sig) with
				   (maybe:yes sig0)
				   -> (o.write (format "((pxll_vector*)r" (int rec-reg) ;; compile-time lookup
						       ")->val[" (int (index-eq label sig0))
						       "] = r" (int arg-reg) ";"))
				   (maybe:no)
				   -> (o.write (format "((pxll_vector*)r" (int rec-reg) ;; run-time lookup
						       ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
						       ")-TC_USEROBJ)>>2," (int label-code)
						       ")] = r" (int arg-reg) ";"))))
			    _ _ -> (primop-error))
	  '%ensure-heap -> (o.write (format "ensure_heap (" (int (length (k/free k))) ", unbox(r" (int (car args)) "));"))
	  '%callocate -> (let ((type (parse-type parm))) ;; gets parsed twice, convert to %%cexp?
			   ;; XXX maybe make alloc_no_clear do an ensure_heap itself?
			   (if (>= target 0)
			       (o.write (format "r" (int target) " = alloc_no_clear (TC_BUFFER, HOW_MANY (sizeof (" (irken-type->c-type type)
						") * unbox(r" (int (car args)) "), sizeof (object)));"))
			       (error1 "%callocate: dead target?" type)))
	  '%exit -> (o.write (format "PXLL_UNDEFINED; result=r" (int (car args)) "; goto Lreturn;"))
	  '%cget -> (match args with
		      (rbase rindex)
		      ;; XXX range-check (probably need to add a length param to TC_BUFFER)
		      -> (let ((cexp (format "(((" (type-repr type) "*)((pxll_int*)r" (int rbase) ")+1)[" (int rindex) "])")))
			   (o.write (format "r" (int target) " = " (wrap-out type cexp) ";")))
		      _ -> (primop-error))
	  '%cset -> (match args type with
		      (rbase rindex rval) (type:pred 'arrow (to-type from-type) _)
		      ;; XXX range-check (probably need to add a length param to TC_BUFFER)
		      -> (let ((rval-exp (lookup-cast to-type from-type (format "r" (int rval))))
			       (lval (format "(((" (type-repr to-type) "*)((pxll_int*)r" (int rbase) ")+1)[" (int rindex) "])")))
			   (o.write (format lval " = " rval-exp ";")))
		      _ _ -> (primop-error))
	  '%getcc -> (match args with
		       () -> (o.write (format "r" (int target) " = k; // %getcc"))
		       _	-> (primop-error))
	  '%putcc -> (match args with
		       (rk rv) -> (begin
				    (o.write (format "k = r" (int rk) "; // %putcc"))
				    (move rv target))
		       _ -> (primop-error))
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
	     (o.write (format "r" (int var) " = r" (int src) ";")))
	    ((and (>= target 0) (not (= target var)))
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

    (define (emit-nvcase test dtname tags subs ealt)
      (let ((use-else? (maybe? ealt)))
	(match (alist/lookup context.datatypes dtname) with
	  (maybe:no) -> (error1 "emit-nvcase" dtname)
	  (maybe:yes dt)
	  -> (if (and (= (length subs) 1) (= (dt.get-nalts) 1))
		 ;; nothing to switch on, just emit the code
		 (emit (nth subs 0))
		 (let ((get-typecode (which-typecode-fun dt)))
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
			     (o.write (format "case (" tag "): {")))
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
		   (o.write "}"))))))
		      
    (define (emit-pvcase test-reg tags arities alts ealt)
      (o.write (format "switch (get_case_noint (r" (int test-reg) ")) {"))
      (let ((else? (maybe? ealt))
	    (n (length alts)))
	(for-range
	    i n
	    (let ((label (nth tags i))
		  (arity (nth arities i))
		  (alt (nth alts i))
		  (tag0 (match (alist/lookup context.variant-labels label) with
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

    ;; body of emit
    (o.indent)
    (when context.options.profile
	  (o.write "prof_current_fun = 0;") ;; entry for top is at [0]
	  (o.write "prof_mark0 = rdtsc();"))
    (emit insns)
    (o.write "Lreturn:")
    (if context.options.profile (o.write "prof_dump();"))
    (o.write "return (pxll_int) result;")
    (o.dedent)
    (o.write "}")
    ))

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

(define (emit-profile-0 o context)
  (o.write "
static int64_t prof_mark0;
static int64_t prof_mark1;
typedef struct {
  int calls;
  int64_t ticks;
  char * name;
} pxll_prof;
static pxll_prof prof_funs[];
static int prof_current_fun;
static int prof_num_funs;
static prof_dump (void)
{
 int i=0;
 for (i=0; prof_funs[i].name; i++) {
   fprintf (stderr, \"%20d\\t%20\" PRIu64 \"\\t%s\\n\", prof_funs[i].calls, prof_funs[i].ticks, prof_funs[i].name);
 }
}
"))

(define (emit-profile-1 o context)
  (o.write "static pxll_prof prof_funs[] = \n  {{0, 0, \"top\"},")
  (for-each
   (lambda (names)
     (let ((name (cdr (reverse names)))) ;; strip 'top' off
       (o.write (format "   {0, 0, \"" (join symbol->string "." name) "\"},"))))
   context.profile-funs)
  (o.write "   {0, 0, NULL}};"))

;; we support three types of non-immediate literals:
;;
;; 1) strings.  identical strings are *not* merged, since
;;      modifying strings is a reasonable choice.
;; 2) symbols.  this emits a string followed by a symbol tuple.
;;      these are collected so each is unique.  any runtime
;;      symbol table should be populated with these first.
;; 3) constructed.  trees of literals made of constructors
;;      (e.g. lists formed with QUOTE), and vectors.  each tree
;;      is rendered into a single C array where the first value
;;      in the array points to the beginning of the top-level
;;      object.

(define (emit-constructed o context)
  (let ((lits (reverse context.literals))
	(nlits (length lits))
	(strings (alist/make))
	(output '())
	(current-index 0)
	(symbol-counter 0)
	)

    ;; emit UOHEAD and UITAG macros, special-casing the builtin datatypes
    (define (uohead nargs dt variant index)
      (match dt variant with
	'list 'cons -> "CONS_HEADER"
	_ _ -> (format "UOHEAD(" (int nargs) "," (int index) ")")))

    (define (uitag dt variant index)
      (match dt variant with
	'list 'nil -> "TC_NIL"
	_ _ -> (format "UITAG(" (int index) ")")))

    (define (walk exp)
      (match exp with
	;; data constructor
	(literal:cons dt variant args)
	-> (let ((dto (alist/get context.datatypes dt "no such datatype"))
		 (alt (dto.get variant))
		 (nargs (length args)))
	     (if (> nargs 0)
		 ;; constructor with args
		 (let ((args0 (map walk args))
		       (addr (+ 1 (length output))))
		   (PUSH output (uohead nargs dt variant alt.index))
		   (for-each (lambda (x) (PUSH output x)) args0)
		   (format "UPTR(" (int current-index) "," (int addr) ")"))
		 ;; nullary constructor - immediate
		 (uitag dt variant alt.index)))
	(literal:vector args)
	-> (let ((args0 (map walk args))
		 (nargs (length args))
		 (addr (+ 1 (length output))))
	     (PUSH output (format "(" (int nargs) "<<8)|TC_VECTOR"))
	     (for-each (lambda (x) (PUSH output x)) args0)
	     (format "UPTR(" (int current-index) "," (int addr) ")"))
	(literal:symbol sym)
	-> (let ((index (alist/get context.symbols sym "unknown symbol?")))
	     (format "UPTR(" (int index) ",1)"))
	(literal:string s)
	-> (match (alist/lookup strings s) with
	     (maybe:yes index) -> (format "UPTR0(" (int index) ")")
	     (maybe:no) -> (error "emit-constructed: lost string"))
	_ -> (int->string (encode-immediate exp))
	))
    (o.dedent) ;; XXX fix this by defaulting to zero indent
    (for-range
	i nlits
	(set! output '())
	(set! current-index i)
	(let ((lit (nth lits i)))
	  (match lit with
	    ;; strings are a special case here because they have a non-uniform structure: the existence of
	    ;;   the uint32_t <length> field means it's hard for us to put a UPTR in the front.
	    (literal:string s)
	    -> (let ((slen (string-length s)))
		 ;; this works because we want strings compared for eq? identity...
		 (alist/push strings s i)
		 (o.write (format "pxll_string constructed_" (int i) " = {STRING_HEADER(" (int slen) "), " (int slen) ", \"" (c-string s) "\" };")))
	    ;; there's a temptation to skip the extra pointer at the front, but that would require additional smarts
	    ;;   in insn_constructed (as already exist for strings).
	    ;; NOTE: this reference to the string object only works because it comes before the symbol in self.context.constructed.
	    (literal:symbol s)
	    -> (begin
		 (o.write (format "// symbol " (sym s)))
		 (o.write (format "pxll_int constructed_" (int i)
				  "[] = {UPTR(" (int i)
				  ",1), SYMBOL_HEADER, UPTR0(" (int (- current-index 1))
				  "), INTCON(" (int symbol-counter) ")};"))
		 (set! symbol-counter (+ 1 symbol-counter))
		 )
	    _ -> (let ((val (walk (nth lits i)))
		       (rout (list:cons val (reverse output))))
		   (o.write (format "pxll_int constructed_" (int i) "[] = {" (join id "," rout) "};")))
	    )))
    (let ((symptrs '()))
      (alist/iterate
       (lambda (symbol index)
	 (PUSH symptrs (format "UPTR(" (int index) ",1)")))
       context.symbols)
      (o.write (format "pxll_int pxll_internal_symbols[] = {(" (int (length symptrs)) "<<8)|TC_VECTOR, " (join id ", " symptrs) "};"))
      )
    (o.indent)
    ))

(define c-string-safe?
  (char-class
   (string->list
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ ")))

;; fix when we get zero-padding format capability...
(define (char->oct-encoding ch)
  (let ((in-oct (format (oct (char->ascii ch)))))
    (format 
     (match (string-length in-oct) with
       0 -> "000"
       1 -> "00"
       2 -> "0"
       _ -> (error1 "unable to oct-encode character" ch)
       )
     in-oct)))

(define (c-string s)
  (let loop ((r '())
	     (s (string->list s)))
    (match s with
      () -> (string-concat (reverse r))
      (ch . rest)
      -> (loop
	  (list:cons
	   (match ch with
	     #\return  -> "\\r"
	     #\newline -> "\\n"
	     #\tab     -> "\\t"
	     #\\       -> "\\\\"
	     #\"       -> "\\\""
	     _ -> (if (c-string-safe? ch)
		      (char->string ch)
		      (char->oct-encoding ch)))
	   r)
	  rest))))

(define (emit-lookup-field o context)
  (cond ((> (length context.records) 0)
	 (o.write "static int lookup_field (int tag, int label)")
	 (o.write "{ switch (tag) {")
	 (for-each
	  (lambda (pair)
	    (match pair with
	      (:pair sig index)
	      -> (begin (o.write (format "  case " (int index) ":"))
			(o.write "  switch (label) {")
			(for-range
			    i (length sig)
			    (o.write (format "     case "
					     (int (lookup-label-code (nth sig i) context))
					     ": return " (int i) "; break;")))
			(o.write "  } break;"))))
	  context.records)
	 (o.write "}}"))))
	 
