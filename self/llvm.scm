;; -*- Mode: Irken -*-

;; LLVM backend: targets *only* LP64.  Note that this covers most operating systems
;;   that anyone cares about, *except* for Win64, which is LLP64.

;; TODO:
;;  in order to move toward full JIT capability, we need to remove
;;  all dependency on an external C file; or, at least as much as we
;;  can.
;;  1) we need to generate our *own* constructed literals.
;;  2) get rid of all external cexp.

;; XXX shouldn't be global (or should be kept in the-context).
;; [alternatively, we can make a wrapper for cps->llvm that defines these]
(define fatbar-free (map-maker int-cmp))
(define litcons (set2-maker int-cmp))
(define ffifuns (set2-maker symbol-index-cmp))

;; CPS registers are mapped to LLVM idents like this:
;;  r5 -> "%r5"

(defmacro oformat
  (oformat item ...) -> (o.write (format item ...))
  )

;; convert a ctype to its llvm representation.
(define ctype->llvm
  (ctype:name 'void)  -> "void"
  (ctype:name 'char)  -> "i8"
  ;;(ctype:name x)      -> (error1 "ctype->llvm: unsupported ctype:name" x)
  (ctype:name x)      -> "i8"
  (ctype:int size _)  -> (format "i" (int (* 8 (cint-size size))))
  (ctype:array len t) -> (format "[" (int len) " x " (ctype->llvm t) "]")
  (ctype:pointer t)   -> (format (ctype->llvm t) "*")
  (ctype:struct _)    -> (format "i8") ;; note: `struct *` becomes `i8*`.
  (ctype:union _)     -> (format "i8")
  )

;; when ready, cname will become name, and called first as 'toplevel
(define (cps->llvm cps co o name cname args external?)

  (let ((current-function-cname cname)
	(current-function-part (make-counter 1))
	(used-jumps (find-jumps cps))
	(fun-stack '())
	(label-counter (make-counter 1))
	(arg-counter (make-counter 0))
        (tid-counter (make-counter 0))
	(renamed (map-maker int-cmp))
	)

    (define (ID)
      (format "%t" (int (tid-counter.inc))))

    (define (make-idents n)
      (let ((v (make-vector n "")))
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

    (define (maybe-target target)
      (if (= -1 target)
          (ID)
          (format "%r" (int target))))

    (define (push-continuation name cname cps args)
      (PUSH fun-stack (lambda () (cps->llvm cps co o name cname args #f))))

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
	(oformat ids[0] " = call fastcc i64 @insn_unbox (i8** %r" (int arg0) ")")
	(oformat ids[1] " = call fastcc i64 @insn_unbox (i8** %r" (int arg1) ")")
	(oformat ids[2] " = " (sym op) " i64 " ids[0] ", " ids[1])
	(oformat "%r" (int trg) " = call fastcc i8** @insn_box (i64 " ids[2] ")")
	))

    (define (emit-icmp op arg0 arg1 trg)
      (let ((ids (make-idents 4)))
	(oformat ids[0] " = call fastcc i64 @insn_unbox (i8** %r" (int arg0) ")")
	(oformat ids[1] " = call fastcc i64 @insn_unbox (i8** %r" (int arg1) ")")
	(oformat ids[2] " = icmp " (sym op) " i64 " ids[0] ", " ids[1])
	(oformat ids[3] " = select i1 " ids[2] ", i64 " (int immediate-true) ", i64 " (int immediate-false))
	(oformat "%r" (int trg) " = inttoptr i64 " ids[3] " to i8**")
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
		     (warning (format "dead target in primop " (sym dtname) ":" (sym altname)))
		     (begin
		       ;; XXX in the c backend this was alloc_no_clear()
		       (oformat "%r" (int target) " = call fastcc i8** @allocate ("
				"i64 " (int (get-uotag dtname altname alt.index))
				", i64 " (int nargs)
				")")
		       (for-range
			   i nargs
			   (oformat "call fastcc void @insn_store ("
				    "  i8** %r" (int target)
				    ", i64 " (int (+ i 1))
				    ", i8** %r" (int (nth args i))
				    ")"))
		       ))
		 ))
	))

    (define (emit-nvget target reg index)
      (oformat "%r" (int target) " = call fastcc i8** @insn_fetch (i8** %r" (int reg) ", i64 " (int (+ 1 index)) ")"))

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
	(oformat id0 " = ptrtoint i8** %r" (int val) " to i64")
	(oformat "switch i64 " id0 ", label %" lelse " [")
	(oformat "  i64 " (int immediate-true) ", label %" lthen)
	(oformat "]")
	(emit-label lthen)
	(walk then)
	(emit-label lelse)
	(walk else)
	))

    ;; currently used only by %c-get-int
    (define lp64->cint
      'int   -> (ctype:int (cint:width 4) #t)
      'uint  -> (ctype:int (cint:width 4) #f)
      'long  -> (ctype:int (cint:width 8) #t)
      'ulong -> (ctype:int (cint:width 8) #f)
      'i8    -> (ctype:int (cint:width 1) #t)
      'u8    -> (ctype:int (cint:width 1) #f)
      'i16   -> (ctype:int (cint:width 2) #t)
      'u16   -> (ctype:int (cint:width 2) #f)
      'i32   -> (ctype:int (cint:width 4) #t)
      'u32   -> (ctype:int (cint:width 4) #f)
      'i64   -> (ctype:int (cint:width 8) #t)
      'u64   -> (ctype:int (cint:width 8) #f)
      x -> (error1 "llvm/parse-c-int failed" x)
      )

    (define (emit-primop name params type args target)
      (match name params args with
	'%llarith (sexp:symbol op) (arg0 arg1)             -> (emit-arith op arg0 arg1 target)
	'%llicmp  (sexp:symbol op) (arg0 arg1)             -> (emit-icmp op arg0 arg1 target)
	'%lleq    _                (arg0 arg1)             -> (emit-icmp 'eq arg0 arg1 target)
	'%dtcon   (sexp:cons dtname altname) args          -> (emit-dtcon dtname altname args target)
	'%nvget   (sexp:list (_ (sexp:int index) _)) (reg) -> (emit-nvget target reg index)
	'%make-vector _ (vlen vval)                        -> (emit-make-vector vlen vval target)
	'%array-ref _ (vec index)                          -> (emit-array-ref vec index target)
	'%array-set _ (vec index val)                      -> (emit-array-set vec index val target)

	'%record-get (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec)
	-> (emit-record-get label sig rec target)

	'%record-set (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec arg)
	-> (emit-record-set label sig rec arg target)

	'%llvm-call (sexp:list ((sexp:string name) sig)) args
	-> (emit-llvm-call name args target)

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
			 (oformat id0 " = load i64, i64* @size_" (int index))
			 (oformat id1 " = call fastcc i64 @insn_unbox (i8** %r" (int count) ")")
			 (oformat "%r" (int target) " = call i8** @insn_callocate (i64 " id0 ", i64 " id1 ")")
			 )
		    (maybe:no)
		    -> (error1 "%callocate failed type lookup" (type-repr type))
		    )
	       _ -> (error1 "%callocate unexpected type" (type-repr type))
	       ))

	'%getcc _ ()
	-> (o.write (format "%r" (int target) " = load i8**, i8*** @k ;; %getcc"))

	'%putcc _ (rk rv)
	-> (begin
	     (o.write (format "store i8** %r" (int rk) ", i8*** @k ;; %putcc"))
	     (o.write (format "%r" (int target) " = bitcast i8** %r" (int rv) " to i8**"))
	     )

        ;; ------------------------------------------------------------------------------------------
        ;; still needed for FFI:
        ;; %halloc %c-set-int %c-sizeof
        ;; for FFI runtime we need make_halloc
        '%malloc params (count)
        -> (let ((ctype (parse-ctype params))
                 (size (ctype->size ctype))
                 (id0 (ID)))
             (printf "%malloc type param = " (repr params) " size = " (int size) "\n")
             (if (< target 0)
                 (error "dead %malloc target"))
             (oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int count) ")")
             (oformat "%r" (int target) " = call i8** @make_malloc (i64 " (int size) ", i64 " id0 ")")
             )

        '%halloc params (count)
        -> (let ((ctype (parse-ctype params))
                 (size (ctype->size ctype))
                 (id0 (ID)))
             (printf "%halloc type param = " (repr params) " size = " (int size) "\n")
             (if (< target 0)
                 (error "dead %halloc target"))
             (oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int count) ")")
             (oformat "%r" (int target) " = call i8** @make_halloc (i64 " (int size) ", i64 " id0 ")")
             )

        '%c-aref params (ref index)
        -> (let ((ctype (parse-ctype params))
                 (size (ctype->size ctype))
                 (id0 (ID))
                 (id1 (ID)))
             (if (< target 0)
                 (error "dead %c-aref target"))
             (oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int index) ")")
             (oformat id1 " = mul i64 " id0 ", " (int size))
             (oformat "%r" (int target) " = call i8** @offset_foreign (i8** %r" (int ref) ", i64 " id1 ")"))

        '%c-pref params (src)
        -> (let ((id0 (ID))
                 (id1 (ID))
                 (id2 (ID)))
             (oformat id0 " = call i8* @get_foreign (i8** %r" (int src) ")")
             (oformat id1 " = bitcast i8* " id0 " to i8**")
             (oformat id2 " = load i8*, i8** " id1)
             (oformat "%r" (int target) " = call i8** @make_foreign (i8* " id2 ")"))

        '%c-sref refexp (src)
        -> (let ((ref0 (sexp->ref refexp)))
             (if (< target 0)
                 (error "dead %c-sref target"))
             (oformat "%r" (int target) " = call i8** @offset_foreign (i8** %r" (int src) ", i64 " (int ref0.off) ")")
             )

        ;; XXX I think we need to make helper funs for sext/zext/trunc.
        '%c-get-int (sexp:symbol itype) (src)
        -> (let ((cint (lp64->cint itype))
                 (iname (ctype->llvm cint))
                 (id0 (ID))
                 (id1 (ID))
                 (id2 (ID))
                 (id3 (ID)))
             (oformat id0 " = call i8* @get_foreign (i8** %r" (int src) ")")
             (oformat id1 " = bitcast i8* " id0 " to " iname "*")
             (oformat id2 " = load " iname ", " iname "* " id1)
             (match cint with
               (ctype:int cint signed?)
               -> (if (= (cint-size cint) 8) ;; already i64
                      (begin
                        (warning (format "64-bit int fetched into irken int."))
                        (oformat "%r" (int target) " = call fastcc i8** @insn_box (i64 " id2 ")"))
                      (begin
                        (if signed?
                            (oformat id3 " = sext " iname " " id2 " to i64")
                            (oformat id3 " = zext " iname " " id2 " to i64"))
                        (oformat "%r" (int target) " = call fastcc i8** @insn_box (i64 " id3 ")"))
                      )
               _ -> (error1 "llvm/%c-get-int: unexpected ctype" (ctype-repr cint))
               ))
        '%ffi2 params args
        -> (emit-ffi2 params args target)
        '%free _ (ref)
        -> (oformat (maybe-target target) " = call i8** @free_foreign (i8** %r" (int ref) ")")
        '%string->cref params (src)
        -> (oformat "%r" (int target) " = call i8** @irk_string_2_cref (i8** %r" (int src) ")")
        '%cref->string params (src len)
        -> (oformat "%r" (int target) " = call i8** @irk_cref_2_string (i8** %r" (int src) ", i8** %r" (int len) ")")
        '%cref->int params (src)
        -> (oformat "%r" (int target) " = call i8** @irk_cref_2_int (i8** %r" (int src) ")")
        ;; ------------------------------------------------------------------------------------------
	x _ _ -> (error1 "unsupported/malformed llvm primop" (:tuple x (repr params) args))
	))

    (define (emit-llvm-call name args target)
      ;; assumes all arguments and retval are irken objects.
      (oformat "%r" (int target) " = call fastcc i8** " name " (" (build-args args) ")")
      )

    (define (emit-record-get label sig rec target)
      (let ((label-code (lookup-label-code label)))
	(match (guess-record-type sig) with
	  (maybe:yes sig0)
	  -> (oformat "%r" (int target) " = call fastcc i8** @insn_fetch ("
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
	  -> (oformat "call fastcc void @insn_store ("
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
	(oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int vlen) ")")
	(oformat "%r" (int target) " = call i8** @make_vector (i64 " id0 ", i8** %r" (int vval) ")")
	))

    (define (emit-array-ref vec index target)
      (let ((id0 (ID)) (id1 (ID)))
	(oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int index) ")")
	(when (not the-context.options.no-range-check)
	      (oformat "call void @vector_range_check (i8** %r" (int vec) ", i64 " id0 ")"))
	(oformat id1 " = add i64 " id0 ", 1")
	(oformat "%r" (int target) " = call i8** @insn_fetch (i8** %r" (int vec) ", i64 " id1 ")")
	))

    (define (emit-array-set vec index val target)
      (let ((id0 (ID)) (id1 (ID)))
	(oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int index) ")")
	(when (not the-context.options.no-range-check)
	      (oformat "call void @vector_range_check (i8** %r" (int vec) ", i64 " id0 ")"))
	(oformat id1 " = add i64 " id0 ", 1")
	(oformat "call fastcc void @insn_store (i8** %r" (int vec) ", i64 " id1 ", i8** %r" (int val) ")")
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
	       'int     -> (begin (oformat arg0 " = call fastcc i64 @insn_unbox (i8** %r" (int arg) ")") (format "i64 " arg0))
	       'bool	-> (begin (oformat arg0 " = call fastcc i64 @irk_is_true (i8** %r" (int arg) ")") (format "i64 " arg0))
	       'string	-> (begin (oformat arg0 " = call fastcc i8** @irk_get_cstring (i8** %r" (int arg) ")") (format "i8* " arg0))
	       'cstring	-> (begin (oformat arg0 " = bitcast i8** to i8*") (format "i8* " arg0))
               'cref    -> (begin (oformat arg0 " = call i8* @get_foreign (i8** %r" (int arg) ")") (format "i8* " arg0))
	       x -> (error1 "wrap-in:" type)
	       ))
	))

    (define (maybe-trunc ctype arg)
      (match ctype with
        (ctype:int cint signed?)
        -> (let ((width (cint-size cint)))
             (if (= width 8) ;; already i64
                 arg
                 (let ((id0 (ID))
                       (lt (ctype->llvm ctype)))
                   (oformat id0 " = trunc " arg " to " lt)
                   (format lt " " id0))))
        _ -> arg))

    (define (wrap-out-int cint signed? val result)
      (let ((width (cint-size cint))
            (iname (format "i" (int (* width 8)))))
        (if (= width 8)
            (begin
              (warning (format "64-bit int fetched into irken int."))
              (oformat result " = call fastcc i8** @insn_box (i64 " val ")"))
            (let ((id0 (ID)))
              (if signed?
                  (oformat id0 " = sext " iname " " val " to i64")
                  (oformat id0 " = zext " iname " " val " to i64"))
              (oformat result " = call fastcc i8** @insn_box (i64 " id0 ")")
              ))))

    (define (wrap-out-ctype ctype val result)
      (match ctype with
        (ctype:int cint signed?)  -> (wrap-out-int cint signed? val result)
        (ctype:pointer _)         -> (oformat result " = call i8** @make_foreign (" (ctype->llvm ctype) " " val ")")
        (ctype:array _ _)         -> (oformat result " = call i8** @make_foreign (" (ctype->llvm ctype) " " val ")")
        x -> (error1 "llvm/wrap-out: unsupported return type" (ctype-repr x))
        ))

    ;; for integer-like results, I think this is similar to the problem that c-get-int solves.

    (define (emit-ffi sig type name args target)
      (match sig with
	(type:pred 'arrow (result-type . arg-types) _)
	-> (let ((args0 (map2 wrap-in arg-types args)))
	     (if (not (= target -1))
		 (oformat "%r" (int target) " = "))
	     ;; XXX wrap-out
	     (oformat "call i8** @" (sym name) "(" (join ", " args0) ")"))
	x -> (error1 "bad ffi type" (type-repr sig))
	))

    ;; XXX next step: we need to emit declarations for each foreign function called.
    (define (emit-ffi2 params args target)
      (match params with
        (sexp:symbol name)
        -> (match (ffi-info.sigs::get name) with
             (maybe:yes (csig:fun name rtype argtypes0))
             -> (let ((argtypes1 (map ctype->irken-type argtypes0))
                      ;; NOTE: wrap-in emits code as a side-effect.
                      (args1 (map2 wrap-in argtypes1 args))
                      (args2 (map2 maybe-trunc argtypes0 args1))
                      (lrtype (ctype->llvm rtype))
                      (id0 (ID)))
                  (ffifuns::add name) ;; so we can declare it later
                  (oformat id0 " = call " lrtype " @" (sym name) "(" (join ", " args2) ")")
                  (wrap-out-ctype rtype id0 (maybe-target target))
                  )
             (maybe:yes (csig:obj name obtype))
             -> (let ((lt (ctype->llvm obtype))
                      (arg (if (string=? lt "i8")
                               (format lt "* @" (sym name))
                               (format "i8* bitcast (" lt "* @" (sym name) " to i8*)"))))
                  (ffifuns::add name)
                  (oformat "%r" (int target) " = call i8** @make_foreign (" arg ")"))
             (maybe:no)
             -> (error1 "ffi2 unknown symbol" name)
             )
        x -> (error1 "bad ffi2 param" x)
        ))

    ;; this will just call the generated C function.
    (define (emit-cexp sig type template args target)
      ;; llvm automatically assigns a target to any non-void function call, so
      ;;  we have to put the result somewhere even if dead.
      (let ((target (if (= target -1)
			(format (ID) " = ")
			(format "%r" (int target) " = "))))
	(match (the-context.cexps::get (:tuple sig template)) with
	  (maybe:yes index)
	  -> (match sig with
	       (type:pred 'arrow _ _)
	       -> (oformat target "call i8** @cexp_" (int index) "("
			   (join (lambda (r) (format "i8** %r" (int r))) ", " args)
			   ")")
	       _ -> (oformat target "load i8**, i8*** @cexp_" (int index))
	       )
	  (maybe:no)
	  -> (error1 "unknown cexp" (format (type-repr sig) " : "template))
	  )))

    (define (emit-new-env size top? types target)
      (oformat "%r" (int target) " = call fastcc i8** @allocate (i64 " (int TC_ENV) ", i64 " (int (+ size 1)) ")")
      (if top?
	  (oformat "store i8** %r" (int target) ", i8*** @top")))

    (define (emit-store off arg tup i)
      ;; XXX rewrite to use @insn_store
      (let ((id0 (ID)) (id1 (ID)))
	(oformat id0 " = getelementptr i8*, i8** %r" (int tup) ", i64 " (int (+ 1 i off)))
	(oformat id1 " = bitcast i8** %r" (int arg) " to i8*")
	(oformat "store i8* " id1 ", i8** " id0)
	))

    (define (safe-known-fun name)
      (let ((var (vars-get-var name)))
        (= 0 var.sets)))

    (define (emit-call* mname funreg)
      (match mname with
        (maybe:no)
        -> (oformat "tail call fastcc void @tail_call (i8** %r" (int funreg) ")")
        (maybe:yes name)
        -> (if (safe-known-fun name)
               (oformat "tail call fastcc void @" (gen-function-cname name 0) "()")
               (oformat "tail call fastcc void @tail_call (i8** %r" (int funreg) ")"))
        ))

    (define (emit-tail name fun args)
      (if (>= args 0)
	  (oformat "call fastcc void @link_env_with_args (i8** %r" (int args) ", i8** %r" (int fun) ")")
	  (oformat "call fastcc void @link_env_noargs (i8** %r" (int fun) ")"))
      (emit-call* name fun)
      (oformat "ret void")
      )

    (define (emit-trcall depth name regs)
      (let ((nargs (length regs))
    	    (npop (- depth 1))
    	    (cname (gen-function-cname name 0)))
    	(if (= nargs 0)
    	    ;; a zero-arg trcall needs an extra level of pop
    	    (set! npop (+ npop 1)))
	(for-range i npop
	    (oformat "call fastcc void @pop_env()"))
    	(for-range i nargs
	    (oformat "call fastcc void @insn_varset (i64 0, i64 "
		     (int i) ", i8** %r" (int (nth regs i)) ")"))
	(oformat "tail call fastcc void @" cname "()")
	(oformat "ret void")
	))

    (define (emit-call name fun args target free k)
      (let ((free (sort < free)) ;; sorting these might improve things
	    (nregs (length free))
	    (kfun (format current-function-cname "_" (int (current-function-part.inc))))
	    (ids (make-idents 2))
	    )
	;; save
	(oformat ids[0] " = call fastcc i8** @allocate (i64 " (int TC_SAVE) ", i64 " (int (+ 3 nregs)) ")")
	(oformat ids[1] " = bitcast void()* @" kfun " to i8**")
	(for-range i nregs
	   (oformat "call fastcc void @insn_store ("
		    "i8** " ids[0]
		    ", i64 " (int (+ i 4))
		    ", i8** %r" (int (nth free i))
		    ")"))
	(oformat "call fastcc void @push_k (i8** " ids[0] ", i8** " ids[1] ")")
	;; push env
	(if (>= args 0)
	    (oformat "call fastcc void @link_env_with_args (i8** %r" (int args) ", i8** %r" (int fun) ")")
	    (oformat "call fastcc void @link_env_noargs (i8** %r" (int fun) ")"))
	;; call
        (emit-call* name fun)
	(oformat "ret void")
	;; emit a new c function to represent the continuation of the current irken function
	(PUSH fun-stack
	      (lambda ()
		(oformat "\ndefine internal fastcc void @" kfun "() {")
		(o.indent)
		;; restore
		(for-range i nregs
		   (oformat "%r" (int (nth free i))
			    " = call fastcc i8** @fetch (i8*** @k, i64 "
			    (int (+ 4 i)) ")"))
		(oformat "call fastcc void @pop_k()")
		(if (>= target 0)
		    (oformat "%r" (int target) " = load i8**, i8*** @result"))
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
	 (oformat "call fastcc void @pop_env()"))
      (let ((jname (format "FAIL_" (int label))))
    	(match (fatbar-free::get label) with
    	  (maybe:yes free)
	  -> (begin
	       (oformat "tail call fastcc void @" jname "(" (build-args free) ")")
	       (o.write "ret void"))
    	  (maybe:no)
	  -> (error1 "emit-fail: failed to lookup fatbar" label) ;; (impossible)
    	  )))

    (define (emit-jump reg trg jn free)
      (begin (oformat "tail call fastcc void @JUMP_" (int jn) "("
		      ;; note: c back end does a move from reg to target, we
		      ;;  achieve the same effect by passing reg as first arg.
		      (build-args (if (= trg -1) free (list:cons reg free)))
		      ")")
	     (oformat "ret void")))

    (define (emit-close name nreg body target)
      (let ((cname (gen-function-cname name 0)))
	(PUSH fun-stack
	      (lambda ()
		(cps->llvm body co o name cname '() #f)))
	(oformat "%r" (int target) " = call fastcc i8** @insn_close (void()* @" cname ")")
	))

    (define (emit-litcon index kind target)
      (when (>= target 0)
	    (litcons::add index)
	    (if (eq? kind 'string)
		(oformat "%r" (int target) " = bitcast i8*** @constructed_" (int index) " to i8**")
		(oformat "%r" (int target) " = load i8**, i8*** getelementptr (i8**, i8*** @constructed_" (int index) ", i64 0)")
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
	       (oformat id0 " = call fastcc i64 @get_case (i8** %r" (int test) ")")
	       (oformat "switch i64 " id0 ", label %" lelse " [")
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
	  (oformat id0 " = call fastcc i64 @get_case (i8** %r" (int test) ")")
	  (oformat "switch i64 " id0 ", label %" lelse " [")
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
	   (oformat "tail call fastcc void @insn_return (i8** %r" (int reg) ")")
	   (o.write "ret void"))

      (insn:literal lit (cont:k reg _ k))
      -> (begin
	   (if (not (= reg -1)) ;; ignore dead literals
	       (oformat "%r" (int reg) " = inttoptr i64 " (int (encode-immediate lit)) " to i8**"))
	   (walk k))

      (insn:varref depth index (cont:k reg _ k))
      -> (begin
	   (if (= depth -1)
	       (oformat "%r" (int reg) " = call fastcc i8** @insn_topref (i64 " (int index) ")")
	       (oformat "%r" (int reg) " = call fastcc i8** @insn_varref "
			"(i64 " (int depth) ", i64 " (int index) ")"))
	   (walk k))

      (insn:varset depth index val (cont:k target _ k))
      -> (begin
	   (if (= depth -1)
	       (oformat "call void @insn_topset (i64 " (int index) ", i8** %r" (int val) ")")
	       (oformat "call fastcc void @insn_varset "
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
      -> (begin (oformat "call fastcc void @push_env (i8** %r" (int r) ")") (walk k))

      (insn:pop r (cont:k target _ k))
      -> (begin
	   (oformat "call fastcc void @pop_env()")
	   (move r target)
	   (walk k))

      (insn:store off arg tup i (cont:k target _ k))
      -> (begin
	   (oformat "call fastcc void @insn_store ("
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
	       (oformat "%r" (int target) " = call fastcc i8** @allocate (i64 " (int tag0) ", i64 " (int size) ")"))
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

    (oformat "\ndefine " (if external? "external" "internal fastcc")
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
			  (join ", " (n-of (length arg-types) "i8**"))
			  ")"))
	 )
    _ -> (begin
	   (co.write (format "const object * cexp_" (int num) " = " (wrap-out sig template) ";"))
	   (o.write (format "@cexp_" (int num) " = external global i8**"))
	   )
    ))

(define (emit-ffi-declarations o)
  (o.write (format ";; declarations for called foreign functions (FFI)"))
  (ffifuns::iterate
   (lambda (name)
     (match (ffi-info.sigs::get name) with
       (maybe:yes (csig:fun name rtype argtypes0))
       -> (let ((argtypes1 (map ctype->llvm argtypes0))
                (lrtype (ctype->llvm rtype)))
            (o.write (format "declare " lrtype " @" (sym name) " (" (join ", " argtypes1) ")")))
       (maybe:yes (csig:obj name obtype))
       ;; XXX all external objects can probably be declared 'i8*'
       -> (o.write (format "@" (sym name) " = external global " (ctype->llvm obtype)))
       _ -> (error1 "emit-ffi-declarations: bad name" name)))))

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
    (cps->llvm cps co o 'toplevel cname '() #t)
    ;; emit litcon externals
    ;; *SOOOO* nice that llvm supports forward references...
    (litcons::iterate
     (lambda (index)
       (oformat "@constructed_" (int index) " = external global i8**")))
    (emit-ffi-declarations o)
    ))
