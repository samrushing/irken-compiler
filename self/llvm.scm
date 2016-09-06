;; -*- Mode: Irken -*-

(define immediate-true  (encode-immediate (literal:cons 'bool 'true '())))
(define immediate-false (encode-immediate (literal:cons 'bool 'false '())))

;; any way to pull these from the include file?

;; immediate types (multiples of 2 (but not 4!))
(define TC_CHAR 2)           ;; (1<<1) // 00000010 02
(define TC_BOOL 6)           ;; (3<<1) // 00000110 06
(define TC_NIL 10)           ;; (5<<1) // 00001010 0a
(define TC_UNDEFINED 14)     ;; (7<<1) // 00001110 0e
(define TC_EMPTY_VECTOR 18)  ;; (9<<1) // 00010010 12
(define TC_USERIMM 22)       ;; (11<<1) // 00010110 16

;; pointer types (multiples of 4)
(define TC_SAVE 4)     ;; (1<<2) // 00000100  04
(define TC_CLOSURE 8)  ;; (2<<2) // 00001000  08
(define TC_TUPLE 12)   ;; (3<<2) // 00001100  0c
(define TC_ENV 12)     ;; (3<<2) // 00001100  0c alias
(define TC_STRING 16)  ;; (4<<2) // 00010000  10
(define TC_VECTOR 20)  ;; (5<<2) // 00010100  14
(define TC_PAIR 24)    ;; (6<<2) // 00011000  18
(define TC_SYMBOL 28)  ;; (7<<2) // 00011100  1c
(define TC_VEC16 32)   ;; (8<<2) // 00100000  20
(define TC_BUFFER 36)  ;; (9<<2) // 00100100  24
(define TC_USEROBJ 40) ;; (10<<2)// 00101000  28


;; XXX shouldn't be global (or should be kept in the-context).
(define fatbar-free (map-maker <))
(define litcons (set2-maker <))

;; CPS registers are mapped to LLVM idents like this:
;;  r5 -> "%r5"
;;  other idents are assigned sequentially like most llvm.
;;  [I may experiment with using unique names instead, though
;;   this might impact compile times]

(defmacro oformat 
  (oformat item ...) -> (o.write (format item ...))
  )

;; when ready, cname will become name, and called first as 'toplevel
(define (cps->llvm cps co o first-id name cname args external?)

  (let ((current-function-cname cname)
	(current-function-part (make-counter 1))
	(used-jumps (find-jumps cps))
	(ident first-id)
	(fun-stack '())
	(label-counter (make-counter 1))
	(arg-counter (make-counter 0))
	(renamed (map-maker <))
	)

    (define (ID)
      (set! ident (+ 1 ident))
      ident
      )

    (define (make-idents n)
      (let ((v (make-vector n 0)))
	(for-range i n
	   (set! v[i] (ID)))
	v))

    (define (new-label)
      (format "L" (int (label-counter.inc))))

    (define (make-labels n)
      (let ((v (make-vector n "")))
	(for-range i n
	   (set! v[i] (new-label)))
	v))

    (define (push-continuation name cname cps args)
      (PUSH fun-stack (lambda () (cps->llvm cps co o 0 name cname args #f))))

    (define (push-fail-continuation cps jump args)
      (push-continuation 'fail (format "FAIL_" (int jump)) cps args))

    (define (push-jump-continuation k jump)
      (match (used-jumps::get jump) with
	(maybe:yes free)
	-> (let ((cname (format "JUMP_" (int jump))))
	     (push-continuation 'jump (format "JUMP_" (int jump)) k free)
	     )
	(maybe:no)
	-> #u))

    (define (emit-label name)
      (o.dedent)
      (oformat name ":")
      (o.indent)
      )

    (define (move src dst)
      (if (and (>= dst 0) (not (= src dst)))
	  (oformat "%r" (int dst) " = bitcast i8** %r" (int src) " to i8**")))

    (define (dead-set target)
      ;; this happens with (let ((_ (set! x y))) ...)
      (if (not (= target -1))
	  (oformat "%r" (int target) " = inttoptr i64 " (int TC_UNDEFINED) " to i8**")))

    (define (emit-arith op arg0 arg1 trg)
      (let ((ids (make-idents 3)))
	(oformat "%" (int ids[0]) " = call i64 @insn_unbox (i8** %r" (int arg0) ")")
	(oformat "%" (int ids[1]) " = call i64 @insn_unbox (i8** %r" (int arg1) ")")
	(oformat "%" (int ids[2]) " = " (sym op) " i64 %" (int ids[0]) ", %" (int ids[1]))
	(oformat "%r" (int trg) " = call i8** @insn_box (i64 %" (int ids[2]) ")")
	))

    (define (emit-icmp op arg0 arg1 trg)
      (let ((ids (make-idents 4)))
	(oformat "%" (int ids[0]) " = call i64 @insn_unbox (i8** %r" (int arg0) ")")
	(oformat "%" (int ids[1]) " = call i64 @insn_unbox (i8** %r" (int arg1) ")")
	(oformat "%" (int ids[2]) " = icmp " (sym op) " i64 %" (int ids[0]) ", %" (int ids[1]))
	(oformat "%" (int ids[3]) " = select i1 %" (int ids[2]) ", i64 " (int immediate-true) ", i64 " (int immediate-false))
	(oformat "%r" (int trg) " = inttoptr i64 %" (int ids[3]) " to i8**")
	))

    (define (emit-dtcon dtname altname args target)
      (match (alist/lookup the-context.datatypes dtname) with
	(maybe:no) -> (error1 "emit-dtcon: no such datatype" dtname)
	(maybe:yes dt)
	-> (let ((alt (dt.get altname))
		 (nargs (length args)))
	     (if (= nargs 0)
		 (oformat "%r" (int target) " = inttoptr i64 " 
				  (int (get-uitag dtname altname alt.index)) " to i8**")
		 (if (= target -1)
		     (warning (format "dead target in primop " (sym dtname) ":" (sym altname) "\n"))
		     (begin
		       ;; XXX in the c backend this was alloc_no_clear()
		       (oformat "%r" (int target) " = call i8** @allocate ("
				"i64 " (int (get-uotag dtname altname alt.index)) 
				", i64 " (int nargs) 
				")")
		       (for-range
			   i nargs
			   (oformat "call void @insn_store ("
				    "  i8** %r" (int target) 
				    ", i64 " (int (+ i 1))
				    ", i8** %r" (int (nth args i))
				    ")"))
		       ))
		 ))
	))

    (define (emit-nvget target reg index)
      (oformat "%r" (int target) " = call i8** @insn_fetch (i8** %r" (int reg) ", i64 " (int (+ 1 index)) ")"))

    (define (emit-move val src target)
      ;; XXX the semantics of insn:move are very confusing.
      (cond ((and (>= src 0) (not (= src val)))
	     ;; from varset
	     (raise (:LLVMRegVarset)))
	    ((and (>= target 0) (not (= target val)))
	     ;; from varref
	     (oformat "%r" (int target) " = bitcast i8** %r" (int val) " to i8** ; // reg varref"))))

    (define (emit-test val then else)
      (let ((id0 (ID))
	    (lthen (new-label))
	    (lelse (new-label)))
	(oformat "%" (int id0) " = ptrtoint i8** %r" (int val) " to i64")
	(oformat "switch i64 %" (int id0) ", label %" lelse " [")
	(oformat "  i64 " (int immediate-true) ", label %" lthen)
	(oformat "]")
	(emit-label lthen)
	(walk then)
	(emit-label lelse)
	(walk else)
	))

    (define (emit-primop name params type args target)
      (match name params args with
	'%llarith (sexp:symbol op) (arg0 arg1)             -> (emit-arith op arg0 arg1 target)
	'%llicmp  (sexp:symbol op) (arg0 arg1)             -> (emit-icmp op arg0 arg1 target)
	'%dtcon   (sexp:cons dtname altname) args          -> (emit-dtcon dtname altname args target)
	'%nvget   (sexp:list (_ (sexp:int index) _)) (reg) -> (emit-nvget target reg index)
	'%make-vector _ (vlen vval)                        -> (emit-make-vector vlen vval target)
	'%array-ref _ (vec index)                          -> (emit-array-ref vec index target)
	'%array-set _ (vec index val)                      -> (emit-array-set vec index val target)

	'%record-get (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec)
	-> (emit-record-get label sig rec target)

	'%record-set (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec arg)
	-> (emit-record-set label sig rec arg target)

	'%call (sexp:list ((sexp:symbol name) sig)) args
	-> (emit-ccall name (parse-cexp-sig sig) args target)

	'%exit _ (arg)
	-> (begin
	     (oformat "store i8** %r" (int arg) ", i8*** @result")
	     (oformat "tail call void @exit_continuation()")
	     (oformat "ret void")
	     )

	;; XXX NYI
	'%ensure-heap _ _
	-> #u

	;; XXX move to emit-callocate
	'%callocate _ (count)
	-> (let ((id0 (ID))
		 (id1 (ID)))
	     (match type with 
	       (type:pred 'buffer (type0) _)
	       -> (match (the-context.callocates::get type0) with
		    (maybe:yes index)
		    -> (begin
			 (oformat "%" (int id0) " = load i64* @size_" (int index))
			 (oformat "%" (int id1) " = call i64 @insn_unbox (i8** %r" (int count) ")")
			 (oformat "%r" (int target) " = call i8** @insn_callocate (i64 %" (int id0) ", i64 %" (int id1) ")")
			 )
		    (maybe:no)
		    -> (error1 "%callocate failed type lookup" (type-repr type))
		    )
	       _ -> (error1 "%callocate unexpected type" (type-repr type))
	       ))

	'%getcc _ ()
	-> (o.write (format "%r" (int target) " = load i8*** @k ;; %getcc"))

	'%putcc _ (rk rv)
	-> (begin 
	     (o.write (format "store i8** %r" (int rk) ", i8*** @k ;; %putcc"))
	     (o.write (format "%r" (int target) " = bitcast i8** %r" (int rv) " to i8**"))
	     )

	;; '%cget
	;; '%cset XXX dont' forget call to dead-set
	;; '%getcc
	;; '%putcc
	x _ _ -> (error1 "unsupported/malformed llvm primop" (:tuple x (repr params) args))
	))

    (define (emit-ccall name sig args target)
      ;; XXX process the sig to transform args/results.
      ;;     until then assume all are i8**
      (oformat "%r" (int target) " = call i8** @" (sym name) "(" (build-args args) ")")
      )

    (define (emit-record-get label sig rec target)
      (let ((label-code (lookup-label-code label)))
	(match (guess-record-type sig) with
	  (maybe:yes sig0)
	  -> (oformat "%r" (int target) " = call i8** @insn_fetch ("
		      "i8** %r" (int rec) 
		      ", i64 " (int (+ 1 (index-eq label sig0)))
		      ")")
	  (maybe:no)
	  -> (oformat "%r" (int target) " = call i8** @record_fetch ("
		      "i8** %r" (int rec)
		      ", i64 " (int label-code)
		      ")")
	  )))

    (define (emit-record-set label sig rec val target)
      (let ((label-code (lookup-label-code label)))
	(match (guess-record-type sig) with
	  (maybe:yes sig0)
	  -> (oformat "call void @insn_store ("
		      "i8** %r" (int rec) 
		      ", i64 " (int (+ 1 (index-eq label sig0)))
		      ", i8** %r" (int val)
		      ")")
	  (maybe:no)
	  -> (oformat "call void @record_store ("
		      "i8** %r" (int rec)
		      ", i64 " (int label-code)
		      ", i8** %r" (int val)
		      ")")
	  )
	(dead-set target)))

    (define (emit-make-vector vlen vval target)
      (let ((id0 (ID)))
	(oformat "%" (int id0) " = call i64 @insn_unbox (i8** %r" (int vlen) ")")
	(oformat "%r" (int target) " = call i8** @make_vector (i64 %" (int id0) ", i8** %r" (int vval) ")")
	))

    (define (emit-array-ref vec index target)
      (let ((id0 (ID)) (id1 (ID)))
	(oformat "%" (int id0) " = call i64 @insn_unbox (i8** %r" (int index) ")")
	(when (not the-context.options.no-range-check)
	      (oformat "call void @vector_range_check (i8** %r" (int vec) ", i64 %" (int id0) ")"))
	(oformat "%" (int id1) " = add i64 %" (int id0) ", 1")
	(oformat "%r" (int target) " = call i8** @insn_fetch (i8** %r" (int vec) ", i64 %" (int id1) ")")
	))

    (define (emit-array-set vec index val target)
      (let ((id0 (ID)) (id1 (ID)))
	(oformat "%" (int id0) " = call i64 @insn_unbox (i8** %r" (int index) ")")
	(when (not the-context.options.no-range-check)
	      (oformat "call void @vector_range_check (i8** %r" (int vec) ", i64 %" (int id0) ")"))
	(oformat "%" (int id1) " = add i64 %" (int id0) ", 1")
	(oformat "call void @insn_store (i8** %r" (int vec) ", i64 %" (int id1) ", i8** %r" (int val) ")")
	(dead-set target)
	))

    ;; thoughts on C interface: we might need to keep %%cexp because of stuff like EV_SET,
    ;;  (i.e., macros are the official interface to some feature).  For actual expressions,
    ;;  we can maybe emit small C functions (and hope LTO can inline them), and for %callocate,
    ;;  maybe we can emit a "sizeof(struct X)" constant that can be referenced from llvm ir.

    ;; provide automatic conversions of base types for inputs to %%ffi
    (define (wrap-in type arg)
      (match type with
      	(type:tvar id _) 
	-> (format "i8** %r" (int arg))
      	(type:pred name predargs _)
      	-> (let ((arg0 (format "%arg" (int (arg-counter.inc)))))
	     (match name with
	       ;; XXX consider - if we could always use a helper fun this could be simplified.
	       'int     -> (begin (oformat arg0 " = call i64 @insn_unbox (i8** %r" (int arg) ")") (format "i64 " arg0))
	       'bool	-> (begin (oformat arg0 " = call i64 @irk_is_true (i8** %r" (int arg) ")") (format "i64 " arg0))
	       'string	-> (begin (oformat arg0 " = call i8** @irk_get_cstring (i8** %r" (int arg) ")") (format "i8* " arg0))
	       'cstring	-> (begin (oformat arg0 " = bitcast i8** to i8*") (format "i8* " arg0))
	       ;;'buffer -> (format "(" (irken-type->c-type type) "(((pxll_vector*)" arg ")+1))")
	       x -> (error1 "wrap-in:" type)
	       ))
	))

    (define (emit-ffi sig type name args target)
      (match sig with
	(type:pred 'arrow (result-type . arg-types) _)
	-> (let ((args0 (map2 wrap-in arg-types args)))
	     (if (not (= target -1))
		 (oformat "%r" (int target) " = "))
	     ;; XXX wrap-out
	     (oformat "call i8** @" (sym name) "(" (join id ", " args0) ")"))
	x -> (error1 "bad ffi type" (type-repr sig))
	))

    ;; this will just call the generated C function.
    (define (emit-cexp sig type template args target)
      ;; llvm automatically assigns a target to any non-void function call, so 
      ;;  we have to put the result somewhere even if dead.
      (let ((target (if (= target -1) 
			(format "%" (int (ID)) " = ") 
			(format "%r" (int target) " = "))))
	(match (the-context.cexps::get (:tuple sig template)) with
	  (maybe:yes index)
	  -> (match sig with
	       (type:pred 'arrow _ _)
	       -> (oformat target "call i8** @cexp_" (int index) "("
			   (join (lambda (r) (format "i8** %r" (int r))) ", " args)
			   ")")
	       _ -> (oformat target "load i8*** @cexp_" (int index))
	       )
	  (maybe:no)
	  -> (error1 "unknown cexp" (format (type-repr sig) " : "template))
	  )))

    (define (emit-new-env size top? types target)
      (oformat "%r" (int target) " = call i8** @allocate (i64 " (int TC_ENV) ", i64 " (int (+ size 1)) ")")
      (if top?
	  (oformat "store i8** %r" (int target) ", i8*** @top")))

    (define (emit-store off arg tup i)
      ;; XXX rewrite to use @insn_store
      (let ((id0 (ID)) (id1 (ID)))
	(oformat "%" (int id0) " = getelementptr i8** %r" (int tup) ", i64 " (int (+ 1 i off)))
	(oformat "%" (int id1) " = bitcast i8** %r" (int arg) " to i8*")
	(oformat "store i8* %" (int id1) ", i8** %" (int id0))
	))

    (define (emit-tail name fun args)
      (if (>= args 0)
	  (oformat "call void @link_env_with_args (i8** %r" (int args) ", i8** %r" (int fun) ")")
	  (oformat "call void @link_env_noargs (i8** %r" (int fun) ")"))
      (match name with
	(maybe:yes name) ;; known function
	-> (let ((cname (gen-function-cname name 0)))
	     (oformat "tail call void @" cname "()")
	     (oformat "ret void"))
	(maybe:no) ;; unknown function
	-> (begin 
	     (oformat "tail call void @tail_call (i8** %r" (int fun) ")")
	     (oformat "ret void"))
	))
  
    (define (emit-trcall depth name regs)
      (let ((nargs (length regs))
    	    (npop (- depth 1))
    	    (cname (gen-function-cname name 0)))
    	(if (= nargs 0)
    	    ;; a zero-arg trcall needs an extra level of pop
    	    (set! npop (+ npop 1)))
	(for-range i npop
	    (oformat "call void @pop_env()"))
    	(for-range i nargs
	    (oformat "call void @insn_varset (i64 0, i64 " 
		     (int i) ", i8** %r" (int (nth regs i)) ")"))
	(oformat "tail call void @" cname "()")
	(oformat "ret void")
	))

    (define (emit-call name fun args target free k)
      (let ((free (sort < free)) ;; sorting these might improve things
	    (nregs (length free))
	    (kfun (format current-function-cname "_" (int (current-function-part.inc))))
	    (ids (make-idents 2))
	    )
	;; save
	(oformat "%" (int ids[0]) " = call i8** @allocate (i64 " (int TC_SAVE) ", i64 " (int (+ 3 nregs)) ")")
	(oformat "%" (int ids[1]) " = bitcast void()* @" kfun " to i8**")
	(for-range i nregs
	   (oformat "call void @insn_store ("
		    "i8** %" (int ids[0])
		    ", i64 " (int (+ i 4))
		    ", i8** %r" (int (nth free i))
		    ")"))
	(oformat "call void @push_k (i8** %" (int ids[0]) ", i8** %" (int ids[1]) ")")
	;; push env
	(if (>= args 0)
	    (oformat "call void @link_env_with_args (i8** %r" (int args) ", i8** %r" (int fun) ")")
	    (oformat "call void @link_env_noargs (i8** %r" (int fun) ")"))
	;; call
	(match name with
	  (maybe:no)
	  -> (oformat "tail call void @tail_call (i8** %r" (int fun) ")")
	  (maybe:yes name)
	  -> (oformat "tail call void @" (gen-function-cname name 0) "()"))
	(oformat "ret void")
	;; emit a new c function to represent the continuation of the current irken function
	(PUSH fun-stack
	      (lambda ()
		(oformat "\ndefine internal void @" kfun "() {")
		;; XXX kludge
		(set! ident 0)
		(o.indent)
		;; restore
		(for-range i nregs
		   (oformat "%r" (int (nth free i))
			    " = call i8** @fetch (i8*** @k, i64 "
			    (int (+ 4 i)) ")"))
		(oformat "call void @pop_k()")
		(if (>= target 0)
		    (oformat "%r" (int target) " = load i8*** @result"))
		(walk k)
		(o.dedent)
		(oformat "}")
		)
	      )
	))

    ;; we emit insns for k0, which may or may not jump to fail continuation in k1
    (define (emit-fatbar label jn k0 k1 free k)
      (fatbar-free::add label free)
      (push-fail-continuation k1 label free)
      (push-jump-continuation k jn)
      (oformat ";; fatbar jn=" (int jn) " label=" (int label))
      (walk k0))

    (define (emit-fail label npop free)
      (for-range i npop
	 (oformat "call void @pop_env()"))
      (let ((jname (format "FAIL_" (int label))))
    	(match (fatbar-free::get label) with
    	  (maybe:yes free) 
	  -> (begin 
	       (oformat "tail call void @" jname "(" (build-args free) ")")
	       (o.write "ret void"))
    	  (maybe:no) 
	  -> (error1 "emit-fail: failed to lookup fatbar" label) ;; (impossible)
    	  )))

    (define (emit-jump reg trg jn free)
      (begin (oformat "tail call void @JUMP_" (int jn) "(" 
		      ;; note: c back end does a move from reg to target, we
		      ;;  achieve the same effect by passing reg as first arg.
		      (build-args (if (= trg -1) free (list:cons reg free)))
		      ")")
	     (oformat "ret void")))

    (define (emit-close name nreg body target)
      (let ((cname (gen-function-cname name 0)))
	(PUSH fun-stack
	      (lambda ()
		(cps->llvm body co o 0 name cname '() #f)))
	(oformat "%r" (int target) " = call i8** @insn_close (void()* @" cname ")")
	))

    (define (emit-litcon index kind target)
      (when (>= target 0) 
	    (litcons::add index)
	    (if (eq? kind 'string)
		(oformat "%r" (int target) " = bitcast i8*** @constructed_" (int index) " to i8**")
		(oformat "%r" (int target) " = load i8*** getelementptr (i8*** @constructed_" (int index) ", i64 0)")
		)))

    (define split-last
      ()        acc -> (impossible)
      (last)    acc -> (:tuple (reverse acc) last)
      (hd . tl) acc -> (split-last tl (list:cons hd acc))
      )

    (define (nvcase-frob-else tags subs ealt)
      ;; if there's no else clause, we need to pull out the last
      ;;   branch of the nvcase and make it one, because with llvm switch
      ;;   the else block is not optional.
      (match ealt with
	(maybe:yes elsek) -> (:tuple tags subs elsek)
	(maybe:no) 
	-> (let-values (((tags0 tagn) (split-last tags '()))
			((subs0 subn) (split-last subs '())))
	     (:tuple tags0 subs0 subn))
	))

    (define (emit-nvcase test dtname tags jump-num subs ealt k)
      (let-values (((tags subs elsek) (nvcase-frob-else tags subs ealt)))
	(match (alist/lookup the-context.datatypes dtname) with
	  (maybe:no) -> (error1 "emit-nvcase" dtname)
	  (maybe:yes dt)
	  -> (let ((id0 (ID))
		   (ntags (length tags))
		   (lelse (new-label))
		   (labs (make-labels ntags)))
	       (push-jump-continuation k jump-num)
	       (oformat "%" (int id0) " = call i64 @get_case (i8** %r" (int test) ")")
	       (oformat "switch i64 %" (int id0) ", label %" lelse " [")
	       (for-range i ntags
		  (let ((label (nth tags i))
			(alt (dt.get label))
			(tag (if (= alt.arity 0) ;; immediate/unit constructor
				 (get-uitag dtname label alt.index)
				 (get-uotag dtname label alt.index))))
		    (oformat " i64 " (int tag) ", label %" labs[i])))
	       (oformat "]")
	       (for-range i ntags
		  (emit-label labs[i])
		  (walk (nth subs i)))
	       (emit-label lelse)
	       (walk elsek)))))

    ;; XXX very similar to nvcase, consider factoring
    (define (emit-pvcase test tags arities jump-num subs ealt k)
      (let-values (((tags subs elsek) (nvcase-frob-else tags subs ealt)))
	(let ((id0 (ID))
	      (ntags (length tags))
	      (lelse (new-label))
	      (labs (make-labels ntags)))
	  (push-jump-continuation k jump-num)
	  (oformat "%" (int id0) " = call i64 @get_case (i8** %r" (int test) ")")
	  (oformat "switch i64 %" (int id0) ", label %" lelse " [")
	  (for-range i ntags
	     (let ((label (nth tags i))
		   (tag0 (match (alist/lookup the-context.variant-labels label) with
			   (maybe:yes v) -> v
			   (maybe:no) -> (error1 "variant constructor never called" label)))
		   (tag1 (if (= (nth arities i) 0) (UITAG tag0) (UOTAG tag0))))
	       (oformat " i64 " (int tag1) ", label %" labs[i])))
	  (oformat "]")
	  (for-range i ntags
	     (emit-label labs[i])
	     (walk (nth subs i)))
	  (emit-label lelse)
	  (walk elsek))))

    (define (build-args args)
      (format (join (lambda (x) (format "i8** %r" (int x))) ", " args)))

    (define (UITAG n) (+ TC_USERIMM (<< n 8)))
    (define (UOTAG n) (+ TC_USEROBJ (<< n 2)))

    ;; hacks for datatypes known by the runtime
    (define (get-uotag dtname altname index)
      (match dtname altname with
	'list 'cons -> TC_PAIR
	'symbol 't  -> TC_SYMBOL
	_ _         -> (UOTAG index)))

    (define (get-uitag dtname altname index)
      (match dtname altname with
	'list 'nil   -> TC_NIL
	'bool 'true  -> immediate-true
	'bool 'false -> immediate-false
	_ _          -> (UITAG index)))

    (define walk

      (insn:return reg)
      -> (begin 
	   (oformat "tail call void @insn_return (i8** %r" (int reg) ")")
	   (o.write "ret void"))

      (insn:literal lit (cont:k reg _ k))
      -> (begin
	   (if (not (= reg -1)) ;; ignore dead literals
	       (oformat "%r" (int reg) " = inttoptr i64 " (int (encode-immediate lit)) " to i8**"))
	   (walk k))

      (insn:varref depth index (cont:k reg _ k))
      -> (begin
	   (if (= depth -1)
	       (oformat "%r" (int reg) " = call i8** @insn_topref (i64 " (int index) ")")
	       (oformat "%r" (int reg) " = call i8** @insn_varref "
			"(i64 " (int depth) ", i64 " (int index) ")"))
	   (walk k))

      (insn:varset depth index val (cont:k target _ k))
      -> (begin
	   (if (= depth -1)
	       (oformat "call void @insn_topset (i64 " (int index) ", i8** %r" (int val) ")")
	       (oformat "call void @insn_varset "
			"(i64 " (int depth) ", i64 " (int index) ", i8** %r" (int val) ")"))
	   (dead-set target)
	   (walk k))

      (insn:test val jumpnum then else (cont:k _ _ k))
      -> (begin 
	   (push-jump-continuation k jumpnum)
	   (emit-test val then else))

      (insn:jump reg trg jn free)
      -> (emit-jump reg trg jn free)

      (insn:move dst var (cont:k reg _ k))
      -> (begin (emit-move dst var reg) (walk k))

      ;; note: special case to ignore continuation after %exit.
      ;; XXX check if this is still needed.
      (insn:primop '%exit params type args (cont:k target _ k))
      -> (begin (emit-primop '%exit params type args target))

      (insn:primop name params type args (cont:k target _ k))
      -> (begin (emit-primop name params type args target) (walk k))

      (insn:cexp sig type template args (cont:k target _ k))
      -> (begin (emit-cexp sig type template args target) (walk k))

      (insn:ffi sig type name args (cont:k target _ k))
      -> (begin (emit-ffi sig type name args target) (walk k))

      (insn:new-env size top? types (cont:k target _ k))
      -> (begin (emit-new-env size top? types target) (walk k))

      (insn:push r (cont:k target _ k))
      -> (begin (oformat "call void @push_env (i8** %r" (int r) ")") (walk k))

      (insn:pop r (cont:k target _ k))
      -> (begin
	   (oformat "call void @pop_env()")
	   (move r target)
	   (walk k))

      (insn:store off arg tup i (cont:k target _ k))
      -> (begin 
	   (oformat "call void @insn_store ("
		    "i8** %r" (int tup) 
		    ", i64 " (int (+ 1 i off)) 
		    ", i8** %r" (int arg) 
		    ")")
	   (if (not (= target -1))
	       (oformat "%r" (int target) " = inttoptr i64 " (int TC_UNDEFINED) " to i8**"))
	   (walk k))

      (insn:trcall d n args)
      -> (emit-trcall d n args)

      (insn:tail name fun args)
      -> (emit-tail name fun args)

      (insn:invoke name fun args (cont:k target free k))
      -> (emit-call name fun args target free k)

      (insn:alloc tag size (cont:k target _ k))
      -> (let ((tag0
		(match tag with
		  (tag:bare v) -> v
		  (tag:uobj v) -> (if (= size 0) (UITAG v) (UOTAG v)))))
	   (if (= size 0)
	       (oformat "%r" (int target) " = inttoptr i64 " (int tag0) " to i8**")
	       (oformat "%r" (int target) " = call i8** @allocate (i64 " (int tag0) ", i64 " (int size) ")"))
	   (walk k))
      
      (insn:fatbar lab jn k0 k1 (cont:k _ free k))
      -> (emit-fatbar lab jn k0 k1 free k)

      (insn:fail label npop free)
      -> (emit-fail label npop free)

      (insn:close name nreg body (cont:k target _ k))
      -> (begin (emit-close name nreg body target) (walk k))

      (insn:litcon index kind (cont:k target _ k))
      -> (begin (emit-litcon index kind target) (walk k))

      (insn:nvcase tr dt tags jn alts ealt (cont:k _ _ k))
      -> (emit-nvcase tr dt tags jn alts ealt k)

      (insn:pvcase tr tags arities jn alts ealt (cont:k _ _ k))
      -> (emit-pvcase tr tags arities jn alts ealt k)

      x -> (begin (printf "cps->llvm insn= ")
      		  (print-insn x 0) (printf "\n") 
      		  (raise (:CPSNotImplemented x)))
      )

    (when the-context.options.trace
	  (oformat "@." cname " = private unnamed_addr constant [" 
		   (int (+ 1 (string-length cname))) " x i8] c\""
		   cname
		   "\\00\", align 1"))

    (oformat "\ndefine " (if external? "external" "internal")
	     " void @" cname "("
	     (join (lambda (x) (format "i8** %r" (int x))) ", " args)
	     ") {")

    ;; emit function body
    (o.indent)

    (when the-context.options.trace 
	  (oformat "call void @TRACE (i8* getelementptr ([" 
		   (int (+ 1 (string-length cname)))
		   " x i8]* @." cname ", i64 0, i64 0))"))

    (if (and (not (member-eq? name '(toplevel fail jump)))
	     (vars-get-flag name VFLAG-ALLOCATES))
	(o.write (format "call void @check_heap()")))

    (walk cps)
    (o.dedent)
    (o.write "}")

    ;; emit JUMP and FAIL functions...
    (while (not (null? fun-stack))
	   ((pop fun-stack)))

    ))

;; collect all unique cexp templates and generate a C function for each one.

(define cexp<?
  (:tuple s0 t0) (:tuple s1 t1)
  -> (cond ((type<? s0 s1) #t)
	   ((type<? s1 s0) #f)
	   (else (string<? t0 t1))))

(define (find-cexps cps)
  (let ((cexp-map the-context.cexps)
	(counter (make-counter 0)))
    (walk-insns
     (lambda (insn _)
       (match insn with
	 (insn:cexp sig type template args _)
	 -> (match (cexp-map::get (:tuple sig template)) with
	      (maybe:yes _) -> #u
	      (maybe:no) -> (cexp-map::add (:tuple sig template) (counter.inc)))
	 _ -> #u
	 ))
     cps)
    cexp-map))

;; XXX combine this pass with find-cexps
(define (find-callocate cps)
  (let ((cmap the-context.callocates)
	(counter (make-counter 0)))
    (walk-insns
     (lambda (insn _)
       (match insn with
	 (insn:primop '%callocate _ (type:pred 'buffer (type) _) _ _)
	 -> (match (cmap::get type) with
	      (maybe:yes _) -> #u
	      (maybe:no)    -> (cmap::add type (counter.inc)))
	 _ -> #u
	 ))
     cps)
    ))

(define (emit-cexp-fun co o num sig template)
  (match sig with
    (type:pred 'arrow (result-type . arg-types) _)
    -> (let ((args0 (range (length arg-types)))
	     (args1 (map (lambda (x) (format "r" (int x))) args0))
	     (args2 (map2 wrap-in arg-types args1))
	     (args3 (format (join (lambda (x) (format "O " x)) ", " args1))))
	 (co.write (format "object* cexp_" (int num) "(" args3 ") { "
			   (if (is-pred? result-type 'undefined)
			       (format (cexp-subst template args2) "; return PXLL_UNDEFINED")
			       (format "return " (wrap-out result-type (cexp-subst template args2))))
			       ";}"))
	 (o.write (format "declare i8** @cexp_" (int num) "("
			  (join id ", " (n-of (length arg-types) "i8**"))
			  ")"))
	 )
    _ -> (begin
	   (co.write (format "const object * cexp_" (int num) " = " (wrap-out sig template) ";"))
	   (o.write (format "@cexp_" (int num) " = external global i8**"))
	   )
    ))

(define (emit-llvm co o cname cps)
  (printf "scanning for cexp...\n")
  (let ((cexp-map (find-cexps cps)))
    (cexp-map::iterate
     (lambda (k v)
       (match k with
	 (:tuple sig template)
	 -> (begin 
	      (printf (lpad 5 (int v)) " " (type-repr sig) " '" template "'\n")
	      (emit-cexp-fun co o v sig template)
	      )
	 )))
    (printf "scanning for callocate...\n")
    (find-callocate cps)
    (the-context.callocates::iterate
     (lambda (k v)
       (printf (lpad 5 (int v)) " " (type-repr k) "\n")
       (co.write (format "pxll_int size_" (int v) " = sizeof (" (irken-type->c-type k) ");"))
       (o.write (format "@size_" (int v) " = external global i64"))
       ))
    (cps->llvm cps co o 0 'toplevel cname '() #t)
    ;; emit litcon externals
    ;; *SOOOO* nice that llvm supports forward references...
    (litcons::iterate
     (lambda (index)
       (oformat "@constructed_" (int index) " = external global i8**")))
    ))
