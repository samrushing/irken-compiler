;; -*- Mode: Irken -*-

(require "self/backend.scm")
(require "self/byteops.scm")

(define opmap
  (let ((m (map-maker symbol-index-cmp)))
    (for-vector info opcode-info
      (m::add info.name info))
    m))

(define (name->info name)
  (match (opmap::get name) with
    (maybe:yes info) -> info
    (maybe:no) -> (raise (:NoSuchOpcode name))
    ))

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
  -> (stream:insn name (list arg0 ...))
  )

(defmacro LINSN
  (LINSN arg0 ...)
  -> (list (INSN arg0 ...))
  )

(define (compile-to-bytecode base cps)

  (let ((opath (string-append base ".byc"))
        (ofile (file/open-write opath #t #o644))
	(o (make-writer ofile))
	(label-counter (make-counter 1))
        (used-jumps (find-jumps cps))
        (jump-label-map (map-maker int-cmp))
        (fun-label-map (map-maker symbol-index-cmp))
        (fatbar-map (map-maker int-cmp))
        (sizeoff-map (cmap/make magic-cmp))
        (lit-already (map-maker magic-cmp))
        ;; remember where the metadata is
        (metadata-index the-context.literals.count)
        )

    (define (new-label)
      (label-counter.inc)
      )

    (define (make-labels n)
      (let ((v (make-vector n 0)))
	(for-range i n
	   (set! v[i] (new-label)))
	v))

    ;; XXX this append call is probably expensive, duh.
    (define (emitk acc k)
      (if (null-cont? k)
          acc
          (append acc (emit k.insn))))

    (define (emit insn)
      (match insn with
        (insn:literal lit k)			      -> (emitk (emit-literal lit k.target) k)
        (insn:return target)                          -> (emit-return target)
        (insn:cexp sig type template args k)          -> (emitk (emit-cexp sig type template args k.target) k)
        (insn:move dst var k)                         -> (emitk (emit-move dst var k.target) k)
        (insn:test reg jn k0 k1 k)                    -> (emit-test reg jn k0 k1 k)
        (insn:jump reg target jn free)                -> (emit-jump reg target jn free.val)
        (insn:close name nreg body k)                 -> (emitk (emit-close name nreg body k.target) k)
        (insn:new-env size top? types k)              -> (emitk (emit-new-env size top? types k.target) k)
        (insn:push r k)                               -> (emitk (emit-push r) k)
        (insn:store off arg tup i k)                  -> (emitk (emit-store off arg tup i) k)
        (insn:varref d i k)                           -> (emitk (emit-varref d i k.target) k)
        (insn:tail name fun args)                     -> (emit-tail name fun args)
        (insn:varset d i v k)                         -> (emitk (emit-varset d i v k.target) k)
        (insn:trcall d n args)                        -> (emit-trcall d n args)
        (insn:invoke name fun args k)                 -> (emitk (emit-call name fun args k) k)
        (insn:pop r k)                                -> (emitk (emit-pop r k.target) k)
        (insn:primop name parm t args k)              -> (emitk (emit-primop name parm t args k) k)
        (insn:fatbar lab jn k0 k1 k)                  -> (emit-fatbar lab jn k0 k1 k)
        (insn:fail label npop free)                   -> (emit-fail label npop free.val)
        (insn:nvcase tr dt tags jn alts ealt k)       -> (emit-nvcase tr dt tags jn alts ealt k)
        (insn:pvcase tr tags arities jn alts ealt k)  -> (emit-pvcase tr tags arities jn alts ealt k)
        (insn:litcon i kind k)                        -> (emitk (emit-litcon i kind k.target) k)
        (insn:alloc tag size k)                       -> (emitk (emit-alloc tag size k.target) k)
        (insn:testcexp regs sig tmpl jn k0 k1 k)      -> (impossible)
        (insn:ffi sig type name args k)               -> (error1 "ffi being redesigned." insn)
        (insn:label label next)                       -> (emit next)
        ))

    (define (encode-int n)

      (define E
        0 acc -> (list->string
                  (cons (ascii->char 255)
                        (cons (ascii->char (length acc))
                              acc)))
        n acc -> (E (>> n 8) (cons (ascii->char (logand n #xff)) acc))
        )

      ;; encoding:
      ;; n  < 254 := n
      ;; n == 254 := -encoded
      ;; n == 255 := <nbytes> <byte n> <byte n-1> ... <byte 0>

      (cond ((< n 0) (format "\xfe" (encode-int (- n))))
            ((< n 254) (char->string (ascii->char n)))
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
      (if (= target -1)
          '() ;; pointless dead literal.
          (let ((enc (encode-immediate lit)))
            (if (and (> enc 0) (< enc #x3fffffff)) ;; will it fit in a bytecode_t?
                (LINSN 'imm target enc)
                (LINSN 'lit
                       target
                       (cmap->index the-context.literals lit))))))

    (define (emit-return reg)
      (LINSN 'ret reg))

    ;; %%cexp is really more like 'builtin primapp' here. [consider renaming]

    (define (emit-cexp sig type template args target)
      ;; Note: is it bad that you can use this to call any insn?
      (let ((opcode (string->symbol template))
            (info (name->info opcode)))
        (list (stream:insn
               (string->symbol template)
               (if info.target
                   (cons target args)
                   args)))
        ))

    (define (move src dst)
      (if (and (>= dst 0) (not (= src dst)))
          (LINSN 'mov dst src)
          '()))

    (define (emit-move var src target)
      ;; MOV <dst-ref> <src-reg>
      (cond ((and (>= src 0) (not (= src var)))
             ;; from varset
             ;; XXX target := #u
             (move src var))
            ((and (>= target 0) (not (= target var)))
             ;; from varref
             (move var target))
            (else '())))

    (define (emit-jump-continuation jn k)
      (match (used-jumps::get jn) with
        (maybe:yes free)
        -> (cons
            (stream:label (jump-label-map::get-err jn "jump label not in map"))
            (emit k))
        (maybe:no)
        -> (list:nil)
        ))

    (define (emit-test val jn k0 k1 cont)
      (let ((l0 (new-label))
            (jcont (emit-jump-continuation jn cont.insn)))
        (append
         (LINSN 'tst val l0)
         (emit k0)
         (LINSN 'jmp l0)
         (list (stream:label l0))
         (emit k1)
         jcont
         )))

    (define (emit-jump reg target jn free)
      (append
       (move reg target)
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
         (list (stream:label lfun))
         gc
         (emit body)
         (list (stream:label l0))
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
      (LINSN 'stor tup (+ i off) arg))

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
      (let ((label (fun-label-map::get-err name "unknown function"))
            (npop (- depth 1))
            (nargs (length args)))
        (if (= 0 nargs)
            (LINSN 'trcall0 label (+ 1 npop))
            (list (stream:insn
                   'trcall
                   (prepend label npop (length args) args)
                   )))))

    (define (emit-call name fun args k)
      (let ((free (sort < k.free))
	    (nregs (length free))
	    (target k.target))
        (append
         (if (= args -1)
             (LINSN 'call0 fun nregs)
             (LINSN 'call fun args nregs))
         (if (= target -1)
             (LINSN 'pop0)
             (LINSN 'pop target)))))

    (define (emit-pop src target)
      (append (move src target) (LINSN 'epop)))

    (define get-sizeoff
      ;; XXX handle all 'sorta known' sizes like pointer, int, char, etc.
      (sexp:symbol 'char) -> 1
      (sexp:symbol 'i8)   -> 1
      (sexp:symbol 'u8)   -> 1
      (sexp:symbol 'i16)  -> 2
      (sexp:symbol 'u16)  -> 2
      (sexp:symbol 'i32)  -> 4
      (sexp:symbol 'u32)  -> 4
      (sexp:symbol 'i64)  -> 8
      (sexp:symbol 'u64)  -> 8
      (sexp:list ((sexp:symbol 'cref) _)) -> 50 ;; (cref 'a) == pointer
      (sexp:symbol 'short)                -> 51
      (sexp:symbol 'int)                  -> 52
      (sexp:symbol 'long)                 -> 53
      (sexp:symbol 'longlong)             -> 54
      sexp
      -> (+ 55 (cmap/add sizeoff-map sexp))
      )

    (define (emit-primop name parm type args k)

      (define (primop-error)
	(error1 "primop" name))

      (let ((target k.target)
	    (nargs (length args)))

        (define prim-dtcon
          (sexp:cons dtname altname)
          -> (match (alist/lookup the-context.datatypes dtname) with
               (maybe:no) -> (error1 "emit-primop: no such datatype" dtname)
               (maybe:yes dt)
               -> (let ((alt (dt.get altname)))
                    (cond ((= nargs 0)
                           (if (= target -1)
                               '()
                               (LINSN 'imm target (get-uitag dtname altname alt.index))))
                          (else
                           (if (>= target 0)
                               (list (stream:insn
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

       (define (ambig code)
         (tree/insert! the-context.ambig-rec int-cmp code #u))

       (define prim-record-get
         (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg)
         -> (let ((label-code (lookup-label-code label)))
              (match (guess-record-type sig) with
                (maybe:yes sig0)
                -> (LINSN 'tupref target rec-reg (index-eq label sig0))
                (maybe:no)
                -> (begin
                     (ambig label-code)
                     (LINSN 'rref target rec-reg label-code))
                ))
         _ _ -> (primop-error))

       (define prim-record-set
         (sexp:list ((sexp:symbol label) (sexp:list sig))) (rec-reg arg-reg)
         -> (let ((label-code (lookup-label-code label)))
              (match (guess-record-type sig) with
                (maybe:yes sig0)
                -> (LINSN 'stor rec-reg (index-eq label sig0) arg-reg)
                (maybe:no)
                -> (begin
                     (ambig label-code)
                     (LINSN 'rset rec-reg label-code arg-reg))))
         _ _ -> (primop-error))

       (define prim-getcc
          () -> (LINSN 'getcc target)
          _  -> (primop-error))

       (define prim-putcc
         (rk rv) -> (LINSN 'putcc target rk rv)
         _ -> (primop-error))

       (define prim-heap
         (rsize) free
         -> (LINSN 'heap rsize (length free))
         _ _ -> (primop-error))

       (define (prim-malloc parm args)
         (let ((sindex (get-sizeoff parm)))
           ;;(printf "adding %malloc call, sindex=" (int sindex) " parm = " (repr parm) "\n")
           (LINSN 'malloc target sindex (car args))))

       (define (prim-halloc parm args)
         (let ((sindex (get-sizeoff parm)))
           ;;(printf "adding %halloc call, sindex=" (int sindex) " parm = " (repr parm) "\n")
           (LINSN 'halloc target sindex (car args))))

       (define (prim-free args)
         (LINSN 'free (car args)))

       (define (prim-string->cref args)
         (LINSN 'sgetp target (car args)))

       ;; this is a limited-remit function needed only to 'fill in' missing
       ;;   params for a handful of FFI-related primops.
       (define irken-type->sexp
         (type:pred sym () _)   -> (sexp:symbol sym)
         (type:pred sym subs _) -> (sexp1 sym (map irken-type->sexp subs))
         x                      -> (raise (:Types/IrkenType2Sexp (type-repr x)))
         )

       (define prim-c-aref
         ;;parm (src index)
         _ (src index)
         -> (let ((tsexp (irken-type->sexp (un-cref type)))
                  (sindex (get-sizeoff tsexp)))
              (printf "c-aref parm = " (repr parm) " sizeoff " (int sindex) "\n"
                      "       type = " (type-repr type) "\n"
                      "       sexp = " (repr tsexp) "\n"
                      "     sindex = " (int sindex) "\n"
                      )
              (LINSN 'caref target src sindex index))
         _ _ -> (primop-error))

       (define prim-c-sfromc
         (src len)
         -> (LINSN 'sfromc target src len)
         _ -> (primop-error))

       (define (irken-type->bytecode t)
         (char->int (ctype->code (irken-type->ctype type))))

       (define prim-cget-int
         ;;(sexp:symbol itype) (src)
         _ (src)
         ;; CGET target src code
         -> (begin
              (printf "c-get-int type = " (type-repr type) "\n"
                      "          code = " (char (int->char (irken-type->bytecode type))) "\n")
              (LINSN 'cget target src (irken-type->bytecode type)))
         _ _ -> (primop-error)
         )

       (define prim-cset-int
         ;;(sexp:symbol itype) (src dst)
         _ (dst src)
         ;; CSET dst code val
         -> (begin
              (printf "c-set-int type = " (type-repr type) "\n"
                      "          code = " (char (int->char (irken-type->bytecode type))) "\n")
              (LINSN 'cset dst (irken-type->bytecode type) src))
         _ _ -> (primop-error)
         )

       (define prim-c-sref
         srefexp (src)
         ;; SREF target src sindex
         -> (let ((sindex (get-sizeoff parm)))
              ;;(printf "c-sref, sindex=" (int sindex) " parm = " (repr parm) "\n")
              (LINSN 'csref target src sindex))
         _ _ -> (primop-error)
         )

       (define (prim-c-sizeof parm)
         (LINSN 'csize target (get-sizeoff parm)))

       (define prim-cref->int
         (src)
         -> (LINSN 'cref2int target src)
         _ -> (primop-error)
         )

       (define prim-int->cref
         (src)
         -> (LINSN 'int2cref target src)
         _ -> (primop-error)
         )

       (match name with
         '%dtcon       -> (prim-dtcon parm)
         '%nvget       -> (prim-nvget parm args)
         '%make-vector -> (prim-make-vector args)
         '%array-ref   -> (prim-array-ref args)
         '%array-set   -> (prim-array-set args)
         '%record-get  -> (prim-record-get parm args)
         '%record-set  -> (prim-record-set parm args)
         '%exit        -> (prim-exit args)
         '%getcc       -> (prim-getcc args)
         '%putcc       -> (prim-putcc args)
         '%ensure-heap -> (prim-heap args k.free)
         ;; -------------------- FFI --------------------
         ;; currently done with %%cexp (needs to be fixed)
         ;; '%ffi2         ->
         '%malloc       -> (prim-malloc parm args)
         '%halloc       -> (prim-halloc parm args)
         '%free         -> (prim-free args)
         '%c-aref       -> (prim-c-aref parm args)
         '%cref->string -> (prim-c-sfromc args)
         '%string->cref -> (prim-string->cref args)
         '%c-get-int    -> (prim-cget-int parm args)
         '%c-set-int    -> (prim-cset-int parm args)
         '%c-sref       -> (prim-c-sref parm args)
         '%c-sizeof     -> (prim-c-sizeof parm)
         '%cref->int    -> (prim-cref->int args)
         '%int->cref    -> (prim-int->cref args)
         ;; -------------------- FFI --------------------

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
         (list (stream:label lfail))
         (emit k1)
         (emit-jump-continuation jn k.insn)
         )))

    (define (emit-fail label npop free)
      (append
       (n-of npop (stream:insn 'epop (list:nil)))
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
	-> (let (((tags0 tagn) (split-last tags '()))
                 ((subs0 subn) (split-last subs '())))
	     (:tuple tags0 subs0 subn))
	))

    ;; NVCASE ob elabel nalts tag0 label0 tag1 label1 ...
    (define (emit-nvcase test dtname tags jump-num subs ealt k)
      (let (((tags subs elsek) (nvcase-frob-else tags subs ealt)))
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
                   (set! pairs (append pairs (list tag labs[i])))
                   (set! result (append
                                 result
                                 (list (stream:label labs[i]))
                                 (emit (nth subs i))))))
               (append
                (list (stream:insn 'nvcase (prepend test lelse ntags pairs)))
                result
                (emit-jump-continuation jump-num k.insn))
               ))))

    (define (emit-pvcase test tags arities jump-num subs ealt k)
      (let (((tags subs elsek) (nvcase-frob-else tags subs ealt))
            (ntags (length tags))
            (lelse (new-label))
            (labs (make-labels ntags))
            (result (cons (stream:label lelse) (emit elsek)))
            (pairs '()))
        (for-range i ntags
          (let ((label (nth tags i))
                (tag0 (match (alist/lookup the-context.variant-labels label) with
                        (maybe:yes v) -> v
                        (maybe:no) -> (error1 "variant constructor never called" label)))
                (tag1 (if (= (nth arities i) 0) (UITAG tag0) (UOTAG tag0))))
            (set! pairs (append pairs (list tag1 labs[i])))
            (set! result (append
                          result
                          (list (stream:label labs[i]))
                          (emit (nth subs i))))))
        (append
         (list (stream:insn 'nvcase (prepend test lelse ntags pairs)))
         result
         (emit-jump-continuation jump-num k.insn)
         )))

    (define (emit-litcon index kind target)
      (LINSN
       (match kind with
         'vector -> 'litc
         'record -> 'litc
         _       -> 'lit)
       target index))

    (define (emit-alloc tag size target)
      (let ((v (if (= size 0) (UITAG tag) (UOTAG tag))))
        (if (= size 0)
            (LINSN 'imm target v)
            (LINSN 'alloc target v size)
            )))

    ;; --------------------------------------------------------------------------------

    (define (build-field-lookup-table)
      (let ((ambig (build-ambig-table))
            (size (tree/size ambig))
            (table (make-vector size {k0=0 k1=0 v=0}))
            (i 0))
        (for-map k v ambig
          (let (((tag label) k))
            (set! table[i] {k0= tag k1=label v=v})
            (inc! i)))
        ;; Note: this is built as a literal, which at runtime will
        ;;  have the type `(vector (vector int))`
        (let (((G V) (create-minimal-perfect-hash table)))
          (literal:vector (list (literal:vector (map literal:int (vector->list G)))
                                (literal:vector (map literal:int (vector->list V))))))))

    ;; XXX why did I do this with a quasi-readable encoding?  seems like it would
    ;;   have made more sense to just use the runtime encoding?  i.e., instead of
    ;;   'T'==#t, use #106?

    ;; XXX once we have an 'official' streaming encoding for random datatypes, consider
    ;;   switching to that encoding for literals/etc.

    (define get-dtcon-tag
      'nil label -> (alist/get the-context.variant-labels label "unknown variant label")
      dt variant -> (let ((dtob (alist/get the-context.datatypes dt "no such datatype"))
                          (alt (dtob.get variant)))
                      alt.index))

    (define (emit-one-literal ob)
      (match (lit-already::get ob) with
        (maybe:yes index)
        -> (o.copy (format "P" (encode-int index)))
        (maybe:no)
        -> (match ob with
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
             (literal:bool #t) -> (o.copy "T")
             (literal:bool #f) -> (o.copy "F")
             (literal:cons dt variant args)
             -> (let ((tag (get-dtcon-tag dt variant))
                      (nargs (length args)))
                  (if (= nargs 0)
                      ;; immediate constructor
                      (o.copy (format "I" (encode-int (get-uitag dt variant tag))))
                      ;; constructor with args
                      (begin
                        (o.copy (format "C"
                                        (encode-int (get-uotag dt variant tag))
                                        (encode-int nargs)))
                        (for-list arg args
                          (emit-one-literal arg)))))
             (literal:vector args)
             -> (begin
                  (o.copy (format "V" (encode-int (length args))))
                  (for-list arg args
                    (emit-one-literal arg)))
             (literal:record tag fields)
             -> (begin
                  (o.copy (format "C"
                                  (encode-int (+ TC_USEROBJ (<< tag 2)))
                                  (encode-int (length fields))))
                  (for-list field fields
                    (match field with
                      (litfield:t _ val)
                      -> (emit-one-literal val))))
             (literal:symbol s)
             -> (let ((s0 (symbol->string s)))
                  (o.copy (format "Y" (encode-int (string-length s0))))
                  (o.copy s0))
             ;; Note: sexp is done via literal:cons
             _ -> (error1 "NYI literal type" (literal->string ob))
             )
        ))

    (define (emit-literals)
      ;; encode literals into the bytecode stream.
      ;; emit constructed literals as a vector
      (let ((lits0 the-context.literals)
            (nlits lits0.count))
        (o.copy (format "V" (encode-int nlits)))
        (for-range i nlits
          (let ((item (cmap->item lits0 i)))
            (emit-one-literal item)
            (lit-already::add item i))
          )
        ))

    (define (resolve-labels s)

      ;; we use pc-relative offsets here, trying to keep the code size small.

      (let ((pc 0)
            (r '())
            (label-map (map-maker int-cmp)))

        (define (resolve index)
          (match (label-map::get index) with
            (maybe:yes val) -> (- val pc)
            (maybe:no)      -> (raise (:BadLabel index))
            ))

        (define resolve-tag-pairs
          (tag lab . rest)
          -> (cons tag (cons (resolve lab) (resolve-tag-pairs rest)))
          () -> '()
          x -> (error1 "odd-length tag pairs" x)
          )

        ;; first pass - compute label offsets (and make sure no negative args)
        (for-list insn s
          (match insn with
            (stream:insn name args)
            -> (begin
                 (set! pc (+ pc 1 (length args)))
                 (if (some? <0 args)
                     (raise (:BadBytecodeArg pc))))
            (stream:label index)
            -> (label-map::add index pc)
            ))

        (set! pc 0)
        ;; second pass - replace label index with label offsets
        (for-list insn s
          (match insn with
            ;; ignore label defs
            (stream:label _)
            -> #u
            (stream:insn 'tst (target index))
            -> (push! r (INSN 'tst target (resolve index)))
            (stream:insn 'jmp (index))
            -> (push! r (INSN 'jmp (resolve index)))
            (stream:insn 'fun (target index))
            -> (push! r (INSN 'fun target (resolve index)))
            (stream:insn 'trcall (index depth nregs . args))
            -> (push! r (stream:insn 'trcall (prepend (resolve index) depth nregs args)))
            (stream:insn 'trcall0 (index depth))
            -> (push! r (INSN 'trcall0 (resolve index) depth))
            (stream:insn 'nvcase (ob elabel nalts . pairs))
            -> (let ((pairs0 (resolve-tag-pairs pairs)))
                 (push! r (stream:insn 'nvcase (prepend ob (resolve elabel) nalts pairs0))))
            _ -> (push! r insn)
            )
          ;; bump pc *after* offsets resolved.
          (match insn with
            (stream:label _)
            -> #u
            (stream:insn _ args)
            -> (set! pc (+ pc 1 (length args)))
            ))

        ;; modified insns
        (reverse r)

        ))

    (define (encode-insn name args)
      (let ((info (name->info name)))
        (match (int-cmp (length args) info.nargs) info.varargs with
          (cmp:<)  _ -> (raise (:BadArity name))
          (cmp:>) #f -> (raise (:BadArity name))
          _ _        -> (string-concat (map encode-int (cons info.code args)))
          )))

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

    (define peephole
      ;; only one optimization so far
      acc (a b . tl)
      -> (match a b with
           (stream:insn 'jmp (n0)) (stream:label n1)
           -> (if (= n0 n1)
                  ;; remove jmp to following label
                  (peephole (list:cons b acc) tl)
                  (peephole (list:cons a acc) (list:cons b tl)))
           _ _ -> (peephole (list:cons a acc) (list:cons b tl)))
      acc (hd . tl)
      -> (peephole (list:cons hd acc) tl)
      acc () -> (reverse acc)
      )

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

    ;; some immediate literals we cannot encode in the bytecode
    ;;  stream, so we refer to them by index.

    ;; XXX now that we have *most* immediates as immediates again,
    ;;   we should probably trim this collection.
    (define (find-immediate-literals cps)
      (walk-insns
       (lambda (insn depth)
         (match insn with
           (insn:literal lit _)
           -> (cmap/add the-context.literals lit)
           _ -> 0))
       cps))

    (define sizeoff-prims '(%malloc '%halloc %c-aref %c-sref))

    (define (find-sizeoff-prims cps)
      (walk-insns
       (lambda (insn depth)
         (match insn with
           (insn:primop name parm _ _ _)
           -> (if (and (member-eq? name sizeoff-prims) (not (eq? parm (sexp:bool #f))))
                  (let ((sindex (get-sizeoff parm)))
                    ;;(printf "find-sizeoff-prims parm = " (repr parm) " sindex = " (int sindex) "\n")
                    (if (not (< sindex 55))
                        (cmap/add sizeoff-map parm)
                        0))
                  0)
           _ -> 0))
       cps))

    (define (build-sizeoff-literal)
      (let ((r '()))
        (printf "build sizeoff vector...")
        (for-range i sizeoff-map.count
          (printf "  " (int i) " " (repr (cmap->item sizeoff-map i)) "\n")
          (push! r (unsexp (cmap->item sizeoff-map i))))
        (printf "done...\n")
        (reverse r)))

    (define (walk-literal lit p)
      (match lit with
        (literal:cons _ _ l) -> (begin (p lit) (for-list x l (walk-literal x p)))
        (literal:vector l)   -> (begin (p lit) (for-list x l (walk-literal x p)))
        _                    -> (begin (p lit) #u)
        ))

    (define (find-symbols lit)
      (walk-literal
       lit
       (lambda (x)
         (match x with
           (literal:symbol s)
           -> (cmap/add the-context.literals x)
           _ -> 0))))

    (define sizeoff-sentinel
      (literal:vector (list (literal:cons 'sexp 'symbol (list (literal:symbol '&&sizeoff-sentinel&&))))))

    (define (get-sizeoff-sentinel-index)
      (if (cmap/present? the-context.literals sizeoff-sentinel)
          (cmap->index the-context.literals sizeoff-sentinel)
          -1))

    ;; --------------------------------------------------------------------------------

    (notquiet (printf "bytecode output...\n"))

    (o.copy "IRKVM0") ;; oh oh it's magic.

    ;; assign a label to every used jump.
    (for-list jn (used-jumps::keys)
      (jump-label-map::add jn (new-label)))

    ;; append 'immediate' literals
    (find-immediate-literals cps)

    ;; find prims that generate sizeoff info.
    (find-sizeoff-prims cps)

    (let ((s (peephole '() (emit cps)))
          ;; record the index of the sizeoff sentinel (so it can be replaced)
          (index (get-sizeoff-sentinel-index))
          (sizeoff-literal (build-sizeoff-literal)))
      ;;(printf "sizeoff sentinel index = " (int index) "\n")
      ;; ensure that all symbols used in sizeoff are singletons
      (for-list lit sizeoff-literal (find-symbols lit))
      (verbose (printf "emit literals...\n"))
      (emit-literals)
      (verbose (printf "done. (" (int the-context.literals.count) " literals).\n"))
      (emit-one-literal (build-field-lookup-table))
      (emit-one-literal (literal:int index))
      (emit-one-literal (literal:vector sizeoff-literal))
      (emit-one-literal (literal:int metadata-index))
      (set! cps (insn:return 0))
      (verbose
       (printf "labels:\n")
       (print-stream s))
      (emit-stream (resolve-labels s))
      (set! s '())
      (o.close)
      (notquiet (printf "wrote " (int (o.get-total)) " bytes to " opath ".\n"))
      )
    (list:nil)
    ))
