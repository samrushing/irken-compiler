;; -*- Mode: Irken -*-

;; LLVM backend: targets *only* LP64.  Note that this covers most operating systems
;;   that anyone cares about, *except* for Win64, which is LLP64.

;; TODO:
;;  in order to move toward full JIT capability, we need to remove
;;  all dependency on generated C code.
;;  1) we need to generate our *own* constructed literals. [done]
;;  2) get rid of all external cexp. [done]
;;  3) ***new!*** generate a table for lookup_field. [done]
;;  4) maybe remove callocate-related stuff (replaced with FFI)? [done]

;; XXX shouldn't be global (or should be kept in the-context).
;; [alternatively, we can make a wrapper for cps->llvm that defines these]
(define fatbar-free (map-maker int-cmp))
(define litcons (set2-maker int-cmp))
(define ffifuns (set2-maker symbol-index-cmp))
(define extobjs (set2-maker string-compare))

;; CPS registers are mapped to LLVM idents like this:
;;  r5 -> "%r5"

(defmacro oformat
  (oformat item ...) -> (o.write (format item ...))
  )

;; convert a ctype to its llvm representation (actually, into the int-or-pointer space)
;; XXX simplify this, most of the arms are unused.
(define ctype->llvm
  (ctype:name 'char)  -> "i8"
  (ctype:name x)      -> "i8" ;; e.g. FILE, any random data
  (ctype:int size _)  -> (format "i" (int (* 8 (cint-size size))))
  (ctype:array len t) -> (format "[" (int len) " x " (ctype->llvm t) "]")
  (ctype:struct _)    -> (format "i8") ;; note: `struct *` becomes `i8*`.
  (ctype:union _)     -> (format "i8")
  (ctype:pointer t)   -> "i8*" ;; all pointers are void*
  )

;; when ready, cname will become name, and called first as 'toplevel
(define (cps->llvm cps o name cname args external?)

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
      (PUSH fun-stack (lambda () (cps->llvm cps o name cname args #f))))

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
				    "i8** %r" (int target)
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
	     ;; can't use reg varset with llvm because it re-uses an SSA.
	     (raise (:LLVMRegVarset)))
	    ((and (>= target 0) (not (= target val)))
	     ;; from varref
	     (oformat "%r" (int target) " = bitcast i8** %r" (int val) " to i8** ; reg varref"))))

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
	-> (emit-llvm-call name args target 'fastcc)

	'%llvm-call (sexp:list ((sexp:string name) sig (sexp:symbol cconv))) args
	-> (emit-llvm-call name args target cconv)

        '%llvm-get (sexp:list ((sexp:string name) sig (sexp:bool external?))) args
        -> (begin
             (if external? (extobjs::add name))
             (oformat "%r" (int target) " = load i8**, i8*** " name))

	'%exit _ (arg)
	-> (begin
	     ;;(oformat "store i8** %r" (int arg) ", i8*** @result")
	     ;;(oformat "tail call void @exit_continuation()")
             (oformat "tail call void @exit_continuation (i8** %r" (int arg) ")")
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
			 (oformat "%r" (int target) " = call fastcc i8** @insn_callocate (i64 " id0 ", i64 " id1 ")")
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
        ;; FFI prims
        '%malloc params (count)
        -> (emit-malloc-halloc params count #t target)
        '%halloc params (count)
        -> (emit-malloc-halloc params count #f target)
        '%c-aref params (ref index)
        -> (let ((ctype (parse-ctype params))
                 (size (ctype->size ctype))
                 (id0 (ID))
                 (id1 (ID)))
             (printf "c-aref: ctype= " (ctype-repr ctype) " size = " (int size) "\n")
             (oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int index) ")")
             (oformat id1 " = mul i64 " id0 ", " (int size))
             (oformat (maybe-target target) " = call i8** @offset_foreign (i8** %r" (int ref) ", i64 " id1 ")"))
        '%c-pref params (src)
        -> (let ((id0 (ID))
                 (id1 (ID))
                 (id2 (ID)))
             (oformat id0 " = call i8* @get_foreign (i8** %r" (int src) ")")
             (oformat id1 " = bitcast i8* " id0 " to i8**")
             (oformat id2 " = load i8*, i8** " id1)
             (oformat (maybe-target target) " = call i8** @make_foreign (i8* " id2 ")"))
        '%c-sref refexp (src)
        -> (let ((ref0 (sexp->ref refexp)))
             (oformat (maybe-target target) " = call i8** @offset_foreign (i8** %r" (int src) ", i64 " (int ref0.off) ") ;; sref")
             )
        '%c-sizeof ctexp ()
        -> (let ((ctype (parse-ctype ctexp)))
             (oformat (maybe-target target) " = call fastcc i8** @insn_box (i64 " (int (ctype->size ctype)) ") ;; sizeof"))
        '%c-get-int (sexp:symbol itype) (src)
        -> (emit-c-get-int itype src target)
        '%c-set-int (sexp:symbol itype) (src dst)
        -> (emit-c-set-int itype src dst)
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
	x _ _ -> (error1 "unsupported/malformed llvm primop" (:tuple x params args))
	))

    (define (emit-llvm-call name args target cconv)
      ;; assumes all arguments and retval are irken objects.
      (oformat (maybe-target target) " = call " (sym cconv) " i8** " name " (" (build-args args) ")")
      )

    (define (ambig code)
      (tree/insert! the-context.ambig-rec int-cmp code #u))

    (define (emit-record-get label sig rec target)
      (let ((label-code (lookup-label-code label)))
	(match (guess-record-type sig) with
	  (maybe:yes sig0)
	  -> (oformat "%r" (int target) " = call fastcc i8** @insn_fetch ("
		      "i8** %r" (int rec)
		      ", i64 " (int (+ 1 (index-eq label sig0)))
		      ")")
	  (maybe:no)
	  -> (begin
               (oformat "%r" (int target) " = call i8** @record_fetch ("
                        "i8** %r" (int rec)
                        ", i64 " (int label-code)
                        ")")
               (ambig label-code))
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
	  -> (begin
               (oformat "call void @record_store ("
                        "i8** %r" (int rec)
                        ", i64 " (int label-code)
                        ", i8** %r" (int val)
                        ")")
               (ambig label-code))
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
	(oformat "%r" (int target) " = call fastcc i8** @insn_fetch (i8** %r" (int vec) ", i64 " id1 ")")
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

    ;; given an irken object, place it into an llvm variable of the appropriate type.
    ;; returns: the var name/type holding the result, e.g. "i32 %t23"
    (define (irken->ctype ctype arg)
      (let ((id0 (ID)))
        (match ctype with
          (ctype:int cint signed?)
          -> (let ((width (cint-size cint)))
               (oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int arg) ")")
               (if (= width 8) ;; already i64
                   (format "i64 " id0)
                   (let ((id1 (ID))
                         (lltype (ctype->llvm ctype)))
                     ;; note: just like in C, this cast ignores signedness.
                     (oformat id1 " = trunc i64 " id0 " to " lltype)
                     (format lltype " " id1))))
          _ -> (begin (oformat id0 "= call i8* @get_foreign (i8** %r" (int arg) ")") (format "i8* " id0))
          )))

    ;; given a ctype object, place it into an llvm variable as an irken object.
    ;; returns: the var name/type holding the result.
    (define (ctype->irken ctype val)
      (match ctype with
        (ctype:int cint signed?) -> (cint->irken cint signed? val)
        _ -> (let ((id0 (ID))) ;; anything else is a pointer
               (oformat id0 " = call i8** @make_foreign (" (ctype->llvm ctype) " " val ") ;; ctype->irken")
               (format "i8** " id0))
        ))

    (define (cint->irken cint signed? val)
      (let ((width (cint-size cint))
            (iname (format "i" (int (* width 8))))
            (id0 (ID))
            (id1 (ID)))
        (if (= width 8)
            (oformat id0 " = bitcast i64 " val " to i64")
            (if signed?
                (oformat id0 " = sext " iname " " val " to i64")
                (oformat id0 " = zext " iname " " val " to i64")))
        (oformat id1 " = call fastcc i8** @insn_box (i64 " id0 ")")
        (format "i8** " id1)))

    (define (emit-malloc-halloc params count malloc? target)
      (let ((ctype (parse-ctype params))
            (size (ctype->size ctype))
            (id0 (ID)))
        (if (< target 0)
            (error "dead %halloc target"))
        (oformat id0 " = call fastcc i64 @insn_unbox (i8** %r" (int count) ")")
        (oformat "%r" (int target) " = call i8** @make_"
                 (if malloc? "malloc" "halloc") " (i64 " (int size) ", i64 " id0 ")")))

    (define (emit-c-get-int itype src target)
      (let ((cint (lp64->cint itype))
            (iname (ctype->llvm cint))
            (src0 (ID))
            (src1 (ID))
            (trg0 (ID)))
        (oformat src0 " = call i8* @get_foreign (i8** %r" (int src) ")")
        (oformat src1 " = bitcast i8* " src0 " to " iname "*")
        (oformat trg0 " = load " iname ", " iname "* " src1)
        (match cint with
          (ctype:int cint signed?)
          -> (oformat (maybe-target target) " = bitcast " (cint->irken cint signed? trg0) " to i8**")
          _ -> (impossible)
          )))

    (define (emit-c-set-int itype src dst)
      (let ((cint (lp64->cint itype))
            (iname (ctype->llvm cint))
            (width (ctype->size cint))
            (dst0 (ID))
            (dst1 (ID))
            (src0 (ID))
            (src1 (ID)))
        (printf "c-set-int itype " (sym itype) " width " (int width) "\n")
        (oformat dst0 " = call i8* @get_foreign (i8** %r" (int dst) ")")
        (oformat dst1 " = bitcast i8* " dst0 " to " iname "*")
        (oformat src0 " = call fastcc i64 @insn_unbox (i8** %r" (int src) ")")
        (oformat "store " iname " "
                 (if (< width 8) (begin (oformat src1 " = trunc i64 " src0 " to " iname) src1) src0)
                 ", " iname "* " dst1)
        ))

    ;; XXX next step: we need to emit declarations for each foreign function called.
    (define (emit-ffi2 params args target)
      (match params with
        (sexp:symbol name)
        -> (match (ffi-info.sigs::get name) with
             (maybe:yes (csig:fun name rtype argtypes0))
             -> (let ((args1 (map2 irken->ctype argtypes0 args))
                      (lrtype (ctype->llvm rtype))
                      (id0 (ID)))
                  (ffifuns::add name) ;; so we can declare it later
                  (oformat id0 " = call " lrtype " @" (sym name) "(" (join ", " args1) ")")
                  (oformat (maybe-target target) " = bitcast " (ctype->irken rtype id0) " to i8**")
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

    (define (emit-new-env size top? types target)
      (oformat "%r" (int target) " = call fastcc i8** @allocate (i64 " (int TC_ENV) ", i64 " (int (+ size 1)) ")")
      (if top?
	  (oformat "store i8** %r" (int target) ", i8*** @top")))

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
	(oformat ids[1] " = bitcast void(i8**)* @" kfun " to i8**")
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
		(oformat "\ndefine internal fastcc void @" kfun "(i8** %rr) {")
		(o.indent)
		;; restore
		(for-range i nregs
		   (oformat "%r" (int (nth free i))
			    " = call fastcc i8** @fetch (i8*** @k, i64 "
			    (int (+ 4 i)) ")"))
		(oformat "call fastcc void @pop_k()")
		;;(if (>= target 0)
		;;    (oformat "%r" (int target) " = load i8**, i8*** @result"))
                (oformat "%r" (int target) " = bitcast i8** %rr to i8**")
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
		(cps->llvm body o name cname '() #f)))
	(oformat "%r" (int target) " = call fastcc i8** @insn_close (void()* @" cname ")")
	))

    (define (emit-litcon index kind target)
      (when (>= target 0)
	    (litcons::add index)
            ;;(oformat "%r" (int target) " = call fastcc i8** @insn_fetch (i8** @lits.all.p, i64 " (int (+ 1 index)) ")")
            (oformat "%r" (int target) " = call fastcc i8** @insn_getlit (i64 " (int (+ 1 index)) ")")
            ))

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
	       (oformat "call fastcc void @insn_topset (i64 " (int index) ", i8** %r" (int val) ")")
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
      -> (raise (:LLVMNoCexp "llvm: no cexps"))

      (insn:ffi sig type name args (cont:k target _ k))
      -> (raise (:TempNoFFI "llvm: temp no ffi"))

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
      (let ((ltype (format "[" (int (+ 1 (string-length cname))) " x i8]")))
        (oformat "call void @TRACE (i8* getelementptr ("
                 ltype ", " ltype "* @." cname
                 ", i64 0, i64 0))")))

    (when (eq? name 'toplevel)
      (let ((nlits the-context.literals.count))
        (when (> nlits 0)
          (oformat "call fastcc void @relocate_llvm_literals0()"))))

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

(define llvm-string-safe?
  (char-class
   (string->list
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'()*+,-./:;<=>?@[]^_`{|}~ ")))

;; a 2-pass algorithm might be wanted here, with a string buffer and string-set!
(define (llvm-string s)
  (let loop ((r '())
	     (s (string->list s)))
    (match s with
      () -> (string-concat (reverse r))
      (ch . rest)
      -> (loop
	  (list:cons
           (if (llvm-string-safe? ch)
               (char->string ch)
               ;; note: we need not worry about #\eof here because it
               ;;  cannot be stored in a string.
               (format "\\" (zpad 2 (hex (char->ascii ch)))))
           r) rest)
      )))

;; literals are built in two primary ways:
;; 1) symbols and strings are built directly.
;; 2) complex literals are assembled as a stream of i64,
;;    with relocations encoded in two ways:
;;    a) external relocation: negative int specifies which
;;       literal, by index.  [i.e., @lit.34 is -34]
;;    b) internal relocation: offset is left-shifted 2 bits,
;;       and will be added to the address of the beginning of
;;       the current literal.
;;
;; All literals are built into a top-level irken vector object,
;;   so insn:litcon now uses @insn_fetch like any vector-ref.
;; All symbols are put in a single vector, added to the list of
;;   all literals just before rendering.
;; Finally, the address of the symbols-vector is placed where
;;   %llvm-get can reach it.

;; XXX TODO
;; need to handle no symbol table.
;; 1) [done] assign TC_EMPTY_VECTOR to irk_internal_symbols_p
;; 2) [done] skip adding the table to lits.all
;; 3) adjust size of lits.all, damnit.

(define (llvm-emit-constructed o)
  (let ((lits the-context.literals)
	(output '())
        (oindex 0)
	(symbol-counter 0)
        (info-map (map-maker int-cmp)))

    (define (push-val n)
      (PUSH output n)
      (set! oindex (+ 1 oindex)))

    ;; hacks for datatypes known by the runtime
    (define (uotag dtname altname index)
      (match dtname altname with
	'list 'cons -> TC_PAIR
	'symbol 't  -> TC_SYMBOL
	_ _         -> (+ TC_USEROBJ (<< index 2))))

    (define (uitag dtname altname index)
      (match dtname altname with
	'list 'nil   -> TC_NIL
	'bool 'true  -> immediate-true
	'bool 'false -> immediate-false
	_ _          -> (+ TC_USERIMM (<< index 8))))

    (define (uohead tag len)
      (+ tag (<< len 8)))

    (define (vector-header name n)
      (format name " = global [" (int (+ 1 n)) " x i64*] [\n"
              "  i64* inttoptr (i64 " (int (+ TC_VECTOR (<< n 8))) " to i64*) ,\n"))

    (define (lit-ref i)
      (match (info-map::get i) with
        (maybe:yes (:tuple lltype _))
        -> (format "  i64* bitcast (" lltype "* @lit." (int i) " to i64*)")
        (maybe:no) -> (impossible)
        ))

    (define (walk exp litnum)
      (match exp with
	;; data constructor
	(literal:cons dt variant args)
	-> (let ((dto (alist/get the-context.datatypes dt "no such datatype"))
		 (alt (dto.get variant))
		 (nargs (length args)))
	     (if (> nargs 0)
		 ;; constructor with args
		 (let ((args0 (map (lambda (arg) (walk arg litnum)) args))
                       (oindex0 oindex))
		   (push-val (uohead (uotag dt variant alt.index) nargs))
                   (for-each push-val args0)
                   (<< oindex0 2))
		 ;; nullary constructor - immediate
		 (uitag dt variant alt.index)))
        (literal:vector ())
        -> (encode-immediate exp)
        (literal:vector args)
        -> (let ((args0 (map (lambda (arg) (walk arg litnum)) args))
                 (nargs (length args))
                 (oindex0 oindex))
             (push-val (uohead TC_VECTOR nargs))
             (for-each push-val args0)
             (<< oindex0 2))
        (literal:record tag fields)
        -> (let ((args0 (map (lambda (field)
                               (match field with
                                 (litfield:t name val)
                                 -> (walk val litnum)))
                             fields))
                 (nargs (length args0))
                 (oindex0 oindex))
             (push-val (uohead (+ TC_USEROBJ (<< tag 2)) nargs))
             (for-each push-val args0)
             (<< oindex0 2))
        ;; negative number indicates external pointer.
        (literal:string s)
        -> (- (+ 1 (cmap->index lits exp)))
        (literal:symbol sym)
        -> (- (+ 1 (cmap->index lits exp)))
        ;; NOTE: sexp is missing from here.  without that, no sexp literals.
	_ -> (encode-immediate exp)
	))

    (define (go)
      ;; create a new vector literal of all symbols, add it to the cmap.
      ;; (so it will be the very last literal).
      (when (not (tree/empty? the-context.symbols))
        (let ((syms (tree/keys the-context.symbols))
              (litsyms (map literal:symbol syms)))
          (cmap/add lits (literal:vector litsyms))
          #u))
      (oformat ";; constructed literals")
      (for-map i lit lits.rev
        (set! output '())
        (set! oindex 0)
        (match lit with
          (literal:string s)
          -> (let ((slen (string-length s))
                   (tlen (string-tuple-length slen))
                   (lltype (format "{i64, i32, [" (int slen) " x i8]}")))
               (info-map::add i (:tuple lltype 0))
               (oformat "@lit." (int i) " = local_unnamed_addr global "
                        lltype " {i64 " (int (+ TC_STRING (<< tlen 8)))
                        ", i32 " (int slen) ", [" (int slen) " x i8] c\"" (llvm-string s) "\" }"))
          (literal:symbol sym)
          -> (let ((oindex0 oindex)
                   (lltype (format "{i64, i64, i64}"))
                   (sindex (cmap->index lits (literal:string (symbol->string sym)))))
               (info-map::add i (:tuple lltype 0))
               (oformat "@lit." (int i) " = local_unnamed_addr global "
                        lltype " {i64 " (int (+ TC_SYMBOL (<< 2 8)))
                        ", i64 " (int (- (+ 1 sindex)))
                        ", i64 " (int (logior 1 (<< symbol-counter 1))) "}"
                        )
               (set! symbol-counter (+ 1 symbol-counter)))
          _
          -> (let ((index (walk lit i))
                   (vals (reverse output))
                   (len (length vals))
                   (lltype (format "[" (int len) " x i64]")))
               (info-map::add i (:tuple lltype (>> index 2)))
               (oformat "@lit." (int i) " = local_unnamed_addr global " lltype
                        " [i64 " (join int->string ", i64 " vals) "]")
               )
          ))
      ;; place all literals into a vector object.
      (o.copy (vector-header "@lits.all" lits.count))
      (let ((lines '()))
        (for-map i lit lits.rev
          (PUSH lines (lit-ref i)))
        (oformat (join ",\n" (reverse lines)) "\n  ]"))
      ;; emit table of offsets (into each i64[] literal)
      (o.copy (format "@lits.offsets = global [" (int (+ 1 lits.count)) " x i32] ["))
      (for-map i lit lits.rev
        (match (info-map::get i) with
          (maybe:yes (:tuple _ offset))
          -> (o.copy (format "i32 " (int offset) ", "))
          (maybe:no) -> (impossible)
          ))
      (oformat " i32 0]")
      ;; the only purpose of these two wrapper functions is to avoid having to know the exact
      ;;   size of the literal vector in order to refer to it in the above generated code.
      ;; if there was some way to 'cast away' the size of the vectors and treat them like C pointers,
      ;;   this would be unnecessary. suggestions welcome.
      (let ((LS (format (int (+ 1 lits.count)))))
        (oformat
         "define internal fastcc void @relocate_llvm_literals0() {\n"
         "  %t0 = bitcast [" LS " x i64*]* @lits.all to i8**\n"
         "  %t1 = bitcast [" LS " x i32]* @lits.offsets to i32*\n"
         "  call void @relocate_llvm_literals (i8** %t0, i32* %t1)\n"
         "  ret void\n"
         "}\n"
         "define internal fastcc i8** @insn_getlit (i64 %index) {\n"
         "  %t0 = bitcast [" LS " x i64*]* @lits.all to i8**\n"
         "  %1 = getelementptr i8*, i8** %t0, i64 %index\n"
         "  %2 = load i8*, i8** %1\n"
         "  %3 = bitcast i8* %2 to i8**\n"
         "  ret i8** %3\n"
         "}\n"
         ))
      (if (tree/empty? the-context.symbols)
          (oformat "@irk_internal_symbols_p = global i8** inttoptr (i64 " (int TC_EMPTY_VECTOR) " to i8**)")
          (oformat "@irk_internal_symbols_p = global i8** bitcast (["
                   (int (+ 1 (tree/size the-context.symbols)))
                   " x i64]* @lit." (int (- lits.count 1)) " to i8**)"))
      )
    ;; body of llvm-emit-constructed
    (if (> lits.count 0)
        (go)
        ;; even if there are no lits we still need this...
        (oformat "@irk_internal_symbols_p = global i8** inttoptr (i64 " (int TC_EMPTY_VECTOR) " to i8**)"))
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
       _ -> (error1 "emit-ffi-declarations: bad name" name))))
  (extobjs::iterate
   (lambda (name)
     (oformat name " = external global i8**")))
  )

(define (emit-llvm-lookup-field-hashtables o)
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
    (let-values (((G V) (create-minimal-perfect-hash table)))
      (oformat "@irk_ambig_size = global i32 " (int size))
      (o.copy (format "@G = global [" (int (+ 1 size)) " x i32] ["))
      (for-vector val G
        (o.copy (format "i32 " (int val) ", ")))
      (oformat "i32 0];")
      (o.copy (format "@V = global [" (int (+ 1 size)) " x i32] ["))
      (for-vector val V
        (o.copy (format "i32 " (int val) ", ")))
      (oformat "i32 0];")
      )))

(define (emit-llvm o cname cps)
  (cps->llvm cps o 'toplevel cname '() #t)
  (llvm-emit-constructed o)
  (emit-llvm-lookup-field-hashtables o)
  (emit-ffi-declarations o)
  )

(define (compile-to-llvm base cps)
  (let ((llpath (format base ".ll"))
        (llvm-file (file/open-write llpath #t #o644))
        (ollvm (make-writer llvm-file)))
    (notquiet (printf "\n-- LLVM output --\n : " llpath "\n"))
    (ollvm.copy
     (get-file-contents "include/preamble.ll"))
    (emit-llvm ollvm "toplevel" cps)
    (ollvm.close)
    (notquiet (printf "wrote " (int (ollvm.get-total)) " bytes to " llpath ".\n"))
    ;; XXX header1.c needs something like find-file, but that currently returns
    ;;  an open file, not a valid pathname.
    (LIST llpath "include/header1.c")))

