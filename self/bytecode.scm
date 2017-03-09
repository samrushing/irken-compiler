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
    (OI 'stor   3)   ;; tuple arg index
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
    (OI 'make   4)   ;; target tag nelem elem0 ...
    (OI 'exit   1)   ;; arg
    (OI 'nvcase 5)   ;; ob elabel nalts tag0 off0 tag1 off1 ...
    (OI 'tupref 3)   ;; target ob index
    (OI 'vref   3)   ;; target vec index-reg
    (OI 'vset   3)   ;; vec index-reg val
    (OI 'vmake  3)   ;; target size val
    (OI 'alloc  3)   ;; target tag size
    (OI 'rref   3)   ;; target rec label-code
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
        (insn:alloc tag size k)                       -> (emitk (emit-alloc tag size (k/target k)) k)
        ;; (insn:testcexp regs sig tmpl jn k0 k1 k)     -> (begin (emit-testcexp regs sig tmpl jn k0 k1 k) (cont:nil))
        ;; (insn:ffi sig type name args k)              -> (error1 "no FFI in C backend" insn)
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
      (LINSN 'lit target 
             (cmap->index the-context.literals lit)))

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
             (cons
              (stream:label l0)
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
      (LINSN 'stor tup i arg))

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
             (prepend
               (fun-label-map::get-err name "unknown function")
               (- depth 1)
               (length args)
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
                                      'make
                                      (prepend
                                       target
                                       (get-uotag dtname altname alt.index) 
                                       (length args)
                                       args)
                                      ))
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

        (define prim-make-vector
          (vlen vval)
          -> (LINSN 'vmake target vlen vval)
          _ -> (primop-error))

       (define prim-array-ref
         (vec index-reg)
         -> (LINSN 'vref target vec index-reg)
         _ -> (primop-error))

       (define prim-array-set
         (vec index-reg val)
         -> (LINSN 'vset vec index-reg val)
         _ -> (primop-error))

       (define prim-record-get
         (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg)
         -> (let ((label-code (lookup-label-code label)))
              (match (guess-record-type sig) with
                (maybe:yes sig0)
                -> (LINSN 'tupref target rec-reg (index-eq label sig0))
                (maybe:no)
                -> (LINSN 'rref target rec-reg label-code)
                ))
         _ _ -> (primop-error))
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
            '%make-vector -> (prim-make-vector args)
            '%array-ref   -> (prim-array-ref args)
            '%array-set   -> (prim-array-set args)
            '%record-get  -> (prim-record-get parm args)
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
                (LIST (stream:insn 'nvcase (prepend test lelse ntags pairs)))
                result
                (emit-jump-continuation jump-num (k/insn k)))
               ))))

    (define (emit-litcon index kind target)
      (LINSN 'lit target index))

    (define (emit-alloc tag size target)
      ;; XXX get rid of `tag:bare`
      (let ((v (match tag with
                 (tag:bare v) -> v
                 (tag:uobj v) -> (if (= size 0) (UITAG v) (UOTAG v)))))
        (LINSN 'alloc target v size)
        ))

    ;; --------------------------------------------------------------------------------

    (define (build-field-lookup-table)
      ;; Note: this is built as a literal, which at runtime will
      ;;  have the type `(vector (vector int))`
      (let ((recs (reverse the-context.records))
            (nrecs (length recs)))
        (if (> nrecs 0)
            (let ((v0 '()))
              (for-list pair recs
                (match pair with
                  (:pair sig index)
                  -> (let ((v1 '()))
                       (for-list x sig
                         (PUSH v1 (literal:int (lookup-label-code x))))
                       (PUSH v0 (literal:vector (reverse v1))))
                  ))
              (literal:vector (reverse v0)))
            ;; no records used in this program.
            (literal:vector '())
            )))

    (define (emit-literals)
      ;; encode literals into the bytecode stream.

      (define (walk ob)
        (match ob with
          (literal:int n)
          -> (if (< n 0)
                 (o.copy (format "-" (encode-int (- 0 n))))
                 (o.copy (format "+" (encode-int n))))
          (literal:char ch) -> (o.copy (format "c" (encode-int (char->ascii ch))))
          (literal:undef)   -> (o.copy "u")
          (literal:string s)
          -> (begin
               (o.copy (format "S" (encode-int (string-length s))))
               (o.copy s))
          (literal:cons 'bool 'true _) -> (o.copy "T")
          (literal:cons 'bool 'false _) -> (o.copy "F")
          (literal:cons dt variant args)
          -> (let ((dto (alist/get the-context.datatypes dt "no such datatype"))
                   (alt (dto.get variant))
                   (nargs (length args)))
               (if (= nargs 0)
                   ;; immediate constructor
                   (o.copy (format "I" (encode-int (get-uitag dt variant alt.index))))
                   ;; constructor with args
                   (begin
                     (o.copy (format "C" 
                                     (encode-int (get-uotag dt variant alt.index))
                                     (encode-int nargs)))
                     (for-list arg args
                       (walk arg)))))
          (literal:vector args)
          -> (begin
               (o.copy (format "V" (encode-int (length args))))
               (for-list arg args
                 (walk arg)))
          ;; XXX sexp, symbol.
          _ -> (error1 "NYI literal type" (literal->string ob))
          ))

      ;; emit constructed literals as a vector
      (let ((lits0 the-context.literals)
            (nlits lits0.count))
        (printf "emitting vector of " (int nlits) " literals\n")
        (o.copy (format "V" (encode-int nlits)))
        (for-range i nlits
          (let ((lit (cmap->item lits0 i)))
            (printf "adding lit pos=" (int i) " lit=" (literal->string lit) "\n")
            (flush)
            (walk lit))))
      )

    (define (resolve-labels s)

      ;; XXX think about using relative offsets rather than absolute ones.
      ;;     this will matter more when code size gets larger, and will allow
      ;;     us to cling to uint16_t longer.

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
            -> (PUSH r (stream:insn 'trcall (prepend (resolve index) depth nregs args)))
            (stream:insn 'nvcase (ob elabel nalts . pairs))
            -> (let ((pairs0 (resolve-tag-pairs pairs)))
                 (PUSH r (stream:insn 'nvcase (prepend ob (resolve elabel) nalts pairs0))))
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
      ;; first, compute the length (in words) of this stream.
      (let ((len 0))
        (for-list item s
          (match item with
            (stream:insn name args)
            -> (set! len (+ len 1 (length args)))
            _ -> (error1 "unresolved label?" item)
            ))
        ;; let the VM know how many words are coming...
        (o.copy (encode-int len)))
      (for-list item s
        (match item with
          (stream:insn name args)
          -> (o.copy (encode-insn name args))
          _ -> (impossible)
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

    (define (find-immediate-literals cps)
      (walk-insns
       (lambda (insn depth)
         (match insn with
           (insn:literal lit _)
           -> (cmap/add the-context.literals lit)
           _ -> 0))
       cps))

    ;; the-context.literals is accumulated in reverse, and the index
    ;;   in insn:litcon refers to the position in that list.
    ;; we need to also treat immediate literals the same way.

    (find-immediate-literals cps)
    ;; place the field lookup table as the last literal.
    (let ((field-table (build-field-lookup-table)))
      (printf "field lookup table:\n")
      (printf (literal->string field-table) "\n")
      (cmap/add the-context.literals field-table)
      )
    (emit-literals)
    (let ((s (peephole (emit cps))))
      (printf "labels:\n")
      (print-stream s)
      (let ((resolved (resolve-labels s)))
        (printf "resolved:\n")
        (print-stream resolved)
        (emit-stream resolved))
      (o.close)
      )
    #u
    ))
