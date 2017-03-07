;; -*- Mode: Irken -*-

(define (OI name nargs) {code=0 name=name nargs=nargs})

(define opcode-info
  (list->vector
   (LIST
    ;;  name   nargs    args
    (OI 'lit    2)   ;; target index
    (OI 'ret    1)   ;; val
    (OI 'add    3)   ;; target a b
    (OI 'sub    3)   ;; target a b
    (OI 'eq     3)   ;; target a b
    (OI 'lt     3)   ;; target a b
    (OI 'gt     3)   ;; target a b
    (OI 'le     3)   ;; target a b
    (OI 'ge     3)   ;; target a b
    (OI 'tst    2)   ;; val offset
    (OI 'jmp    1)   ;; offset
    (OI 'fun    2)   ;; target offset
    (OI 'tail   2)   ;; closure args
    (OI 'tail0  1)   ;; closure
    (OI 'env    2)   ;; target size
    (OI 'arg    3)   ;; tuple arg index
    (OI 'ref    3)   ;; target depth index
    (OI 'mov    2)   ;; dst src
    (OI 'epush  1)   ;; args
    (OI 'trcall 3)   ;; offset depth nregs reg0 ...
    (OI 'ref0   2)   ;; target index
    (OI 'call   3)   ;; closure args nregs
    (OI 'pop    1)   ;; target
    (OI 'print  2)   ;; target arg
    (OI 'topis  1)   ;; env
    (OI 'topref 2)   ;; target index
    (OI 'topset 2)   ;; index val
    (OI 'set    3)   ;; depth index val
    (OI 'pop0   0)   ;;
    (OI 'epop   0)   ;;
    (OI 'tron   0)   ;;
    (OI 'troff  0)   ;;
    (OI 'gc     0)   ;;
    (OI 'imm    2)   ;; target tag
    (OI 'alloc  4)   ;; target tag nelem elem0 ...
    (OI 'exit   1)   ;; arg
    (OI 'nvcase 5)   ;; ob elabel nalts tag0 off0 tag1 off1 ...
    (OI 'tupref 3)   ;; target ob index
    (OI 'tupset 3)   ;; ob index val
    )))

(for-range i (vector-length opcode-info)
  (let ((info opcode-info[i]))
    (set! info.code i)))

(define (name->info name)
  (let/cc return
    (for-vector op opcode-info
       (if (eq? name op.name)
           (return op)))
    (raise (:NoSuchOpcode name))))

(define (name->opcode name)
  (let ((info (name->info name)))
    info.code
    ))

;; insn stream consists of bytecodes, label defs, and label refs.
(datatype stream
  (:label int)
  (:insn symbol (list int))
  )

(define stream-repr
  (stream:label n)  -> (format "L" (int n) ":")
  (stream:insn op args) -> (format " " (sym op) " " (join int->string " " args))
  )

(defmacro INSN
  (INSN name arg0 ...)
  -> (stream:insn name (LIST arg0 ...))
  )

(defmacro LINSN
  (LINSN arg0 ...)
  -> (LIST (INSN arg0 ...))
  )

(define (compile-to-bytecode base cps)

  (let ((opath (string-append base ".byc"))
        (ofile (file/open-write opath #t #o644))
	(o (make-writer ofile))
        (lits '())
        (nlit 0)
	(label-counter (make-counter 1))
        (used-jumps (find-jumps cps))
        (jump-label-map (map-maker <))
        (fun-label-map (map-maker symbol-index<?))
        (fatbar-map (map-maker <))
        )

    (define (new-label)
      (label-counter.inc)
      )

    (define (make-labels n)
      (let ((v (make-vector n 0)))
	(for-range i n
	   (set! v[i] (new-label)))
	v))

    (define emitk
      acc (cont:k _ _ k) -> (append acc (emit k))
      acc (cont:nil)     -> acc
      )

    (define (emit insn)
      (match insn with
        (insn:literal lit k)			      -> (emitk (emit-literal lit (k/target k)) k)
        (insn:return target)                          -> (emit-return target)
        (insn:cexp sig type template args k)          -> (emitk (emit-cexp sig type template args (k/target k)) k)
        (insn:move dst var k)                         -> (emitk (emit-move dst var (k/target k)) k)
        (insn:test reg jn k0 k1 k)                    -> (emit-test reg jn k0 k1 k)
        (insn:jump reg target jn free)                -> (emit-jump reg target jn free)
        (insn:close name nreg body k)                 -> (emitk (emit-close name nreg body (k/target k)) k)
        (insn:new-env size top? types k)              -> (emitk (emit-new-env size top? types (k/target k)) k)
        (insn:push r k)                               -> (emitk (emit-push r) k)
        (insn:store off arg tup i k)                  -> (emitk (emit-store off arg tup i) k)
        (insn:varref d i k)                           -> (emitk (emit-varref d i (k/target k)) k)
        (insn:tail name fun args)                     -> (emit-tail name fun args)
        (insn:varset d i v k)                         -> (emitk (emit-varset d i v (k/target k)) k)
        (insn:trcall d n args)                        -> (emit-trcall d n args)
        (insn:invoke name fun args k)                 -> (emitk (emit-call name fun args k) k)
        (insn:pop r k)                                -> (emitk (emit-pop r (k/target k)) k)
        (insn:primop name parm t args k)              -> (emitk (emit-primop name parm t args k) k)
        (insn:fatbar lab jn k0 k1 k)                  -> (emit-fatbar lab jn k0 k1 k)
        (insn:fail label npop free)                   -> (emit-fail label npop free)
        (insn:nvcase tr dt tags jn alts ealt k)       -> (emit-nvcase tr dt tags jn alts ealt k)
        (insn:litcon i kind k)                        -> (emitk (emit-litcon i kind (k/target k)) k)
        ;; (insn:testcexp regs sig tmpl jn k0 k1 k)     -> (begin (emit-testcexp regs sig tmpl jn k0 k1 k) (cont:nil))
        ;; (insn:ffi sig type name args k)              -> (error1 "no FFI in C backend" insn)
        ;; (insn:alloc tag size k)                      -> (begin (emit-alloc tag size (k/target k)) k)
        ;; (insn:pvcase tr tags arities jn alts ealt k) -> (begin (emit-pvcase tr tags arities jn alts ealt k) (cont:nil))
        _ -> (error1 "NYI" insn)
        ))

    (define (encode-int n)

      (define E
        0 acc -> (list->string
                  (cons (ascii->char 255)
                        (cons (ascii->char (length acc))
                              acc)))
        n acc -> (E (>> n 8) (cons (ascii->char (logand n #xff)) acc))
        )

      (cond ((< n 0) (raise (:NegativeIntInBytecode n)))
            ((< n 255) (char->string (ascii->char n)))
            (else
             (let ((r (E n '())))
               (if (> (string-length r) 254)
                   (raise (:IntegerTooBig n))
                   r)))))

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

    (define (emit-literal lit target)
      (let ((index nlit))
        (PUSH lits lit)
        (set! nlit (+ 1 nlit))
        (LINSN 'lit target index)))

    (define (emit-return reg)
      (LINSN 'ret reg))

    ;; %%cexp is really more like 'builtin primapp' here. [consider renaming]
    ;; Note: there's a problem with dead targets here.
    ;; "->#u" prims will often (but not always!) be in dead positions,
    ;;  it's their job to ignore the unused target.
    ;; But what do we do with a dead target for a prim that does *not*
    ;;  ignore its target?  Is it always safe to change 'dead' to '0'?

    (define (emit-cexp sig type template args target)
      ;; Note: is it bad that you can use this to call any insn?
      (LIST (stream:insn
             (string->symbol template)
             (cons (if (= target -1) 0 target) args))))

    (define (emit-move var src target)
      ;; MOV <dst-ref> <src-reg>
      (cond ((and (>= src 0) (not (= src var)))
             ;; from varset
             ;; XXX target := #u
             (LINSN 'mov var src))
            ((and (>= target 0) (not (= target var)))
             ;; from varref
             (LINSN 'mov target var))
            (else '())))

    (define (emit-jump-continuation jn k)
      (match (used-jumps::get jn) with
        (maybe:yes free)
        -> (let ((l0 (new-label)))
             (jump-label-map::add jn l0)
             (append
              (LIST (stream:label l0))
              (emit k)))
        (maybe:no)
        -> (list:nil)
        ))

    (define (emit-test val jn k0 k1 cont)
      (let ((l0 (new-label))
            (jcont (emit-jump-continuation jn (k/insn cont))))
        (append
         (LINSN 'tst val l0)
         (emit k0)
         (LINSN 'jmp l0)
         (LIST (stream:label l0))
         (emit k1)
         jcont
         )))

    (define (emit-jump reg target jn free)
      (append
       (LINSN 'mov target reg)
       (LINSN 'jmp (jump-label-map::get-err jn "jump number not in map"))))

    (define (emit-close name nreg body target)
      (let ((l0 (new-label))
            (lfun (new-label))
            (gc (if (vars-get-flag name VFLAG-ALLOCATES)
                    (LINSN 'gc)
                    '())))
        (fun-label-map::add name lfun)
        (append
         (LINSN 'fun target l0)
         (LIST (stream:label lfun))
         gc
         (emit body)
         (LIST (stream:label l0))
         )))

    (define (emit-new-env size top? types target)
      (append
       (LINSN 'env target size)
       (if top?
           (LINSN 'topis target)
           '())))

    (define (emit-push target)
      (LINSN 'epush target)
      )

    (define (emit-store off arg tup i)
      ;; XXX can we compute if tup is also top and emit topset?
      (LINSN 'arg tup arg i))

    (define (emit-varref depth index target)
      (match depth with
         0 -> (LINSN 'ref0 target index)
        -1 -> (LINSN 'topref target index)
         _ -> (LINSN 'ref target depth index)
        ))

    (define (emit-varset depth index val target)
      (match depth with
         0 -> (LINSN 'set0 index val)
        -1 -> (LINSN 'topset index val)
         _ -> (LINSN 'set depth index val)
        ))

    (define (emit-tail name fun args)
      (if (= -1 args)
          (LINSN 'tail0 fun)
          (LINSN 'tail fun args)))

    (define (emit-trcall depth name args)
      ;; TRCALL <label> <depth> <nregs> <reg0> ...
      (LIST (stream:insn
             'trcall
             (append
              (LIST
               (fun-label-map::get-err name "unknown function")
               (- depth 1)
               (length args))
              args))))

    (define (emit-call name fun args k)
      (let ((free (sort < (k/free k)))
	    (nregs (length free))
	    (target (k/target k)))
        (printf "emit-call: free=" (join int->string " " free) "\n")
        (append
         (LINSN 'call fun args nregs)
         (if (= target -1)
             (LINSN 'pop0)
             (LINSN 'pop target)))))

    (define (emit-pop src target)
      (if (= target -1)
          (LINSN 'epop)
          (append
           (LINSN 'mov target src)
           (LINSN 'epop))))

    (define (emit-primop name parm type args k)

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
                           (LINSN 'imm target (get-uitag dtname altname alt.index)))
                          (else
                           (if (>= target 0)
                               (LIST (stream:insn
                                      'alloc
                                      (cons target
                                            (cons (get-uotag dtname altname alt.index)
                                                  (cons (length args) args)))))
                               (begin
                                 (warning (format "dead target in primop " (sym name) "\n"))
                                 '())))
                          )))
          _ -> (primop-error))

        (define (prim-exit args)
          (LINSN 'exit (car args)))

        (define prim-nvget
          (sexp:list (_ (sexp:int index) _)) (reg)
          -> (LINSN 'tupref target reg index)
          _ _ -> (primop-error))

;;;        (define prim-make-vector
;;;          (vlen vval)
;;;          -> (begin
;;;               ;; since we cannot know the size at compile-time, there should
;;;               ;; always be a call to ensure_heap() before any call to %make-vector
;;;               (o.write (format "O r" (int target) ";"))
;;;               (o.write (format "if (unbox(r" (int vlen) ") == 0) { r" (int target) " = (object *) TC_EMPTY_VECTOR; } else {"))
;;;               (o.write (format "  O t = alloc_no_clear (TC_VECTOR, unbox(r" (int vlen) "));"))
;;;               (o.write (format "  for (int i=0; i<unbox(r" (int vlen) "); i++) { t[i+1] = r" (int vval) "; }"))
;;;               (o.write (format "  r" (int target) " = t;"))
;;;               (o.write "}"))
;;;          _ -> (primop-error))
;;;
;;;        (define prim-array-ref
;;;          (vec index)
;;;          -> (begin
;;;               (o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
;;;               (o.write (format "O r" (int target) " = ((pxll_vector*)r" (int vec) ")->val[unbox(r" (int index) ")];")))
;;;          _ -> (primop-error))
;;;
;;;        (define prim-array-set
;;;          (vec index val)
;;;          -> (begin
;;;               (o.write (format "range_check (GET_TUPLE_LENGTH(*(object*)r" (int vec) "), unbox(r" (int index)"));"))
;;;               (o.write (format "((pxll_vector*)r" (int vec) ")->val[unbox(r" (int index) ")] = r" (int val) ";"))
;;;               (when (> target 0)
;;;                 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
;;;          _ -> (primop-error))
;;;
;;;        (define prim-record-get
;;;          (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg)
;;;          -> (let ((label-code (lookup-label-code label)))
;;;               (match (guess-record-type sig) with
;;;                 (maybe:yes sig0)
;;;                 -> (o.write (format "O r" (int target) ;; compile-time lookup
;;;                                     " = ((pxll_vector*)r" (int rec-reg)
;;;                                     ")->val[" (int (index-eq label sig0))
;;;                                     "];"))
;;;                 (maybe:no)
;;;                 -> (o.write (format "O r" (int target) ;; run-time lookup
;;;                                     " = ((pxll_vector*)r" (int rec-reg)
;;;                                     ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
;;;                                     ")-TC_USEROBJ)>>2," (int label-code)
;;;                                     ")]; // label=" (sym label)))))
;;;          _ _ -> (primop-error))
;;;
;;;        ;; XXX very similar to record-get, maybe some way to collapse the code?
;;;        (define prim-record-set
;;;          (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg arg-reg)
;;;          -> (let ((label-code (lookup-label-code label)))
;;;               (match (guess-record-type sig) with
;;;                 (maybe:yes sig0)
;;;                 -> (o.write (format "((pxll_vector*)r" (int rec-reg) ;; compile-time lookup
;;;                                     ")->val[" (int (index-eq label sig0))
;;;                                     "] = r" (int arg-reg) ";"))
;;;                 (maybe:no)
;;;                 -> (o.write (format "((pxll_vector*)r" (int rec-reg) ;; run-time lookup
;;;                                     ")->val[lookup_field((GET_TYPECODE(*r" (int rec-reg)
;;;                                     ")-TC_USEROBJ)>>2," (int label-code)
;;;                                     ")] = r" (int arg-reg) ";")))
;;;               (when (>= target 0)
;;;                 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
;;;          _ _ -> (primop-error))
;;;
;;;        (define (prim-callocate parm args)
;;;          (let ((type (parse-type parm))) ;; gets parsed twice, convert to %%cexp?
;;;            ;; XXX maybe make alloc_no_clear do an ensure_heap itself?
;;;            (if (>= target 0)
;;;                (o.write (format "O r" (int target) " = alloc_no_clear (TC_BUFFER, HOW_MANY (sizeof (" (irken-type->c-type type)
;;;                                 ") * unbox(r" (int (car args)) "), sizeof (object)));"))
;;;                (error1 "%callocate: dead target?" type))))
;;;
;;;
;;;        (define prim-cget
;;;          (rbase rindex)
;;;          ;; XXX range-check (probably need to add a length param to TC_BUFFER)
;;;          -> (let ((cexp (format "(((" (type-repr type) "*)((pxll_int*)r" (int rbase) ")+1)[" (int rindex) "])")))
;;;               (o.write (format "O r" (int target) " = " (wrap-out type cexp) ";")))
;;;          _ -> (primop-error))
;;;
;;;        (define prim-cset
;;;          (rbase rindex rval) (type:pred 'arrow (to-type from-type) _)
;;;          ;; XXX range-check (probably need to add a length param to TC_BUFFER)
;;;          -> (let ((rval-exp (lookup-cast to-type from-type (format "r" (int rval))))
;;;                   (lval (format "(((" (type-repr to-type) "*)((pxll_int*)r" (int rbase) ")+1)[" (int rindex) "])")))
;;;               (o.write (format lval " = " rval-exp ";"))
;;;               (when (> target 0)
;;;                 (o.write (format "O r" (int target) " = (object *) TC_UNDEFINED;"))))
;;;          _ _ -> (primop-error))
;;;
;;;        (define prim-getcc
;;;          () -> (o.write (format "O r" (int target) " = k; // %getcc"))
;;;          _  -> (primop-error))
;;;
;;;        (define prim-putcc
;;;          (rk rv) -> (begin
;;;                       (o.write (format "k = r" (int rk) "; // %putcc"))
;;;                       (move rv target))
;;;          _ -> (primop-error))

          (match name with
            '%dtcon       -> (prim-dtcon parm)
            '%nvget       -> (prim-nvget parm args)
            ;; '%make-vector -> (prim-make-vector args)
            ;; '%array-ref   -> (prim-array-ref args)
            ;; '%array-set   -> (prim-array-set args)
            ;; '%record-get  -> (prim-record-get parm args)
            ;; '%record-set  -> (prim-record-set parm args)
            ;; '%callocate   -> (prim-callocate parm args)
            '%exit        -> (prim-exit args)
            ;; '%cget        -> (prim-cget args)
            ;; '%cset        -> (prim-cset args type)
            ;; '%getcc       -> (prim-getcc args)
            ;; '%putcc       -> (prim-putcc args)
            ;; '%ensure-heap -> (emit-check-heap (k/free k) (format "unbox(r" (int (car args)) ")"))
            _ -> (primop-error))))

    ;; we emit insns for k0, which may or may not jump to fail continuation in k1
    (define (emit-fatbar label jn k0 k1 k)
      (let ((lfail (new-label)))
        ;; k0
        ;; Lfail:
        ;; k1:
        ;; Ljump:
        ;; k
        (fatbar-map::add label lfail)
        (append
         (emit k0)
         (LIST (stream:label lfail))
         (emit k1)
         (emit-jump-continuation jn (k/insn k))
         )))

    (define (emit-fail label npop free)
      (append
       (n-of npop (stream:insn 'epop0 '()))
       (LINSN 'jmp (fatbar-map::get-err label "unknown label"))
       ))

    ;; XXX these two funs are also in llvm.scm, maybe move them to backend.scm?
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

    ;; NVCASE ob elabel nalts tag0 label0 tag1 label1 ...
    (define (emit-nvcase test dtname tags jump-num subs ealt k)
      (let-values (((tags subs elsek) (nvcase-frob-else tags subs ealt)))
	(match (alist/lookup the-context.datatypes dtname) with
	  (maybe:no) -> (error1 "emit-nvcase" dtname)
	  (maybe:yes dt)
	  -> (let ((ntags (length tags))
		   (lelse (new-label))
		   (labs (make-labels ntags))
                   (result (cons (stream:label lelse) (emit elsek)))
                   (pairs '())
                   )
	       (for-range i ntags
                 (let ((label (nth tags i))
                       (alt (dt.get label))
                       (tag (if (= alt.arity 0) ;; immediate/unit constructor
                                (get-uitag dtname label alt.index)
                                (get-uotag dtname label alt.index))))
                   (set! pairs (append pairs (LIST tag labs[i])))
                   (set! result (append 
                                 result
                                 (LIST (stream:label labs[i]))
                                 (emit (nth subs i))))))
               (append 
                (LIST (stream:insn 'nvcase (append (LIST test lelse ntags) pairs)))
                result
                (emit-jump-continuation jump-num (k/insn k)))
               ))))

    ;; (define (emit-litcon index kind target)
    ;;   (LINSN 'lit target index))

    ;; --------------------------------------------------------------------------------

    (define (emit-literals)
      (for-list lit (reverse lits)
        (match lit with
          (literal:int n)
          -> (if (< n 0)
                 (o.copy (format "-" (encode-int (- 0 n))))
                 (o.copy (format "+" (encode-int n))))
          (literal:char ch) -> (o.copy (format "c" (encode-int (char->ascii ch))))
          (literal:undef)   -> (o.copy "u")
          (literal:cons 'bool 'true _) -> (o.copy "T")
          (literal:cons 'bool 'false _) -> (o.copy "F")
          _ -> (error1 "non-immediate literal?" lit)
          )
        )
      (o.copy ".")
      )

    (define (resolve-labels s)
      (let ((pc 0)
            (r '())
            (label-map (map-maker <)))

        (define (resolve index)
          (match (label-map::get index) with
            (maybe:yes val) -> val
            (maybe:no)      -> (raise (:BadLabel index))
            ))

        (define resolve-tag-pairs
          (tag lab . rest)
          -> (cons tag (cons (resolve lab) (resolve-tag-pairs rest)))
          () -> '()
          x -> (error1 "odd-length tag pairs" x)
          )

        ;; first pass - compute label offsets
        (for-list insn s
          (match insn with
            (stream:insn name args)
            -> (set! pc (+ pc 1 (length args)))
            (stream:label index)
            -> (label-map::add index pc)
            ))

        ;; second pass - replace label index with label offsets
        (for-list insn s
          (match insn with
            ;; ignore label defs
            (stream:label _)
            -> #u
            (stream:insn 'tst (target index))
            -> (PUSH r (INSN 'tst target (resolve index)))
            (stream:insn 'jmp (index))
            -> (PUSH r (INSN 'jmp (resolve index)))
            (stream:insn 'fun (target index))
            -> (PUSH r (INSN 'fun target (resolve index)))
            (stream:insn 'trcall (index depth nregs . args))
            -> (PUSH r (stream:insn 'trcall (append (LIST (resolve index) depth nregs) args)))
            (stream:insn 'nvcase (ob elabel nalts . pairs))
            -> (let ((pairs0 (resolve-tag-pairs pairs)))
                 (PUSH r (stream:insn 'nvcase (append (LIST ob (resolve elabel) nalts) pairs0))))
            _ -> (PUSH r insn)
            ))

        ;; modified insns
        (reverse r)

        ))

    (define (encode-insn name args)
      (let ((info (name->info name)))
        (if (>= (length args) info.nargs)
            (string-concat (map encode-int (cons info.code args)))
            (raise (:BadArity name)))))

    (define (emit-stream s)
      (for-list item s
        (match item with
          (stream:insn name args)
          -> (o.copy (encode-insn name args))
          _ -> (error1 "unresolved label?" item)
          )))

    (define (peephole s)
      ;; only one optimization so far...
      (match s with
        (a b . tl)
        -> (match a b with
             (stream:insn 'jmp (n0)) (stream:label n1)
             -> (if (= n0 n1)
                    ;; remove jmp to following label
                    (list:cons b (peephole tl))
                    (list:cons a (peephole (list:cons b tl))))
             _ _ -> (list:cons a (peephole (list:cons b tl))))
        (hd . tl)
        -> (list:cons hd (peephole tl))
        () -> '()
        ))

    (define (print-stream s)
      (let ((pc 0))
        (for-list item s
          (printf (rpad 5 (int pc)) " " (stream-repr item) "\n")
          (match item with
            (stream:insn name args)
            -> (set! pc (+ pc 1 (length args)))
            _ -> #u
            )
          )))

    (let ((s (peephole (emit cps))))
      (printf "lits:\n")
      (for-list lit lits
        (printn lit))
      (printf "labels:\n")
      (print-stream s)
      (emit-literals)
      (let ((resolved (resolve-labels s)))
        (printf "resolved:\n")
        (print-stream resolved)
        (emit-stream resolved))
      (o.close)
      )
    #u
    ))
