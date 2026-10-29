;; -*- Mode: Irken -*-

(require "self/backend.scm")
(require "self/asmlits.scm")

;; peephole optimization ideas:
;; combine str, str into stp?
;; same with ldr, ldr into ldp
;; elide branch to following code
;; elide labels never referenced

;; register usage on arm64:
;;
;;    x0-x7   parameter/return/scratch
;;    x8-x15  scratch
;;    x16-x17 intra-procedure-call scratch (may be usable?)
;;    x18     platform register, no touching
;; *  x19-x28 scratch
;; *  x29 fp
;; *  x30 lr
;; *  x31 sp/zero [depends on insn]
;; [* == callee-saved]

(define arm-register-names
  #("x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15" "x16"
    "x17" "x18" "x19" "x20" "x21" "x22" "x23" "x24" "x25" "x26" "x27" "x28" "x29" "fp" "lr" "sp"))

;; this is the mapping from cps registers to arm registers. we (currently) need 20.
(define arm-register-file
  #(19 20 21 22 23 24 25 ;; these are callee-saved, safe to use
       ;; start using caller-saved registers, backwards,
       ;; in order to preserve the ability to call C funs.
       ;; we may have to nix these two, investigate
       17 16
       15 14 13 12 11 10 9 8
       ;; digging into C params/scratch here...
       7 6 5
       ))

(assert (= (vector-length arm-register-file) *max-bytecode-registers*))

;; runtime register assignments
;;                         -currently-
(define Rk        -1) ;; x28
(define Rlenv     -2) ;; x27
(define Rfreep    -3) ;; x26
(define Rreturn   -4) ;; x0
(define Rscratch  -5) ;; x1
(define Rscratch2 -6) ;; x2
(define Rscratch3 -7) ;; x3
(define (Rcarg n)
  (assert (and (< n 8) (>= n 0))
          (format "arm Rcarg out of range: " (int n)))
  (- (+ 4 n)) ;; note: matches runtime-register-map below
  )

;; indicated with a negative register number
(define runtime-register-map
  #(-1 ;; placeholder
    28 ;; k
    27 ;; lenv
    26 ;; freep
    0  ;; return    carg0
    1  ;; scratch   carg1
    2  ;; scratch2  carg2
    3  ;; scratch3  carg3
    4  ;;           carg4
    ;; carg5-7 are also irken registers 19-17 (see arm-register-file),
    ;;   so emit-c-call has to be careful when it loads them.
    5  ;;           carg5
    6  ;;           carg6
    7  ;;           carg7
    ))

(define (register->name n)
  (let ((num (if (< n 0)
                 runtime-register-map[(- n)]
                 arm-register-file[n])))
    arm-register-names[num]))

(define (register->num n)
  (if (< n 0)
      runtime-register-map[(- n)]
      arm-register-file[n]))

;; XXX this is a bit overkill, datatype not really needed.
(datatype armcc
  (:eq) ;; Equal Z
  (:ne) ;; Not equal !Z
  (:cs) ;; Carry set, Unsigned higher or same C
  (:cc) ;; Carry clear, Unsigned lower !C
  (:mi) ;; Minus, Negative N
  (:pl) ;; Plus, Positive or zero !N
  (:vs) ;; Overflow V
  (:vc) ;; No overflow !V
  (:hi) ;; Unsigned higher C & !Z
  (:ls) ;; Unsigned lower or same !C | Z
  (:ge) ;; Signed greater than or equal N = V
  (:lt) ;; Signed less than N 6= V
  (:gt) ;; Signed greater than !Z & N = V
  (:le) ;; Signed less than or equal Z | N 6= V
  )

(define armcc->name
  (armcc:eq) -> 'eq (armcc:ne) -> 'ne (armcc:cs) -> 'cs (armcc:cc) -> 'cc
  (armcc:mi) -> 'mi (armcc:pl) -> 'pl (armcc:vs) -> 'vs (armcc:vc) -> 'vc
  (armcc:hi) -> 'hi (armcc:ls) -> 'ls (armcc:ge) -> 'ge (armcc:lt) -> 'lt
  (armcc:gt) -> 'gt (armcc:le) -> 'le
  )

(define name->armcc
  'eq -> (armcc:eq) 'ne -> (armcc:ne) 'cs -> (armcc:cs) 'cc -> (armcc:cc)
  'mi -> (armcc:mi) 'pl -> (armcc:pl) 'vs -> (armcc:vs) 'vc -> (armcc:vc)
  'hi -> (armcc:hi) 'ls -> (armcc:ls) 'ge -> (armcc:ge) 'lt -> (armcc:lt)
  'gt -> (armcc:gt) 'le -> (armcc:le)
  'hs -> (armcc:cs) 'lo -> (armcc:cc) ;; aliases
  x -> (raise (:ArmCC/Unknown x))
  )

(datatype arm
  (:comment string)           ;; for inline comments
  (:label string)             ;; local label
  (:global string)            ;; global label
  (:insn symbol (list armop)) ;; instruction
  (:verb string)              ;; verbatim (e.g. ".quad 1234")
  )

(define (format-arm insn)
  (match insn with
    (arm:comment s)          -> (format the-context.options.asm-comment s)
    (arm:insn op ())         -> (format (sym op)) ;; e.g. 'return'
    (arm:insn op args)       -> (format (sym op) " " (join format-armop ", " args))
    (arm:label name)         -> (format name ":")
    (arm:global name)        -> (if the-context.options.asm-underscores
                                    (format ".global _" name "\n_" name ":")
                                    (format ".global " name "\n" name ":"))
    (arm:verb s)             -> (format s)
    ))

;; inline assembly.

;; operands for arm insns.

(datatype armop
  (:trg)      ;; target
  (:k)        ;; Rk
  (:lenv)     ;; Rlenv
  (:freep)    ;; Rfreep
  (:retval)   ;; Rreturn
  (:xzr)      ;; zero register
  (:sp)       ;; stack pointer
  (:n int)    ;; register by 'irken number'
  (:r string) ;; register by name
  (:s int)    ;; scratch by number
  (:a int)    ;; argument by number
  (:x int)    ;; absolute register by number 0=x0, 1=x1, ...
  (:i int)    ;; immediate
  (:c int)    ;; C argument
  (:l string) ;; label
  (:lsl int)  ;; shift/rotate imm
  (:lsr int)  ;; ...
  (:asr int)
  (:ror int)
  (:cc armcc) ;; condition code
  (:w armop)  ;; change to 'W' version of register.
  (:ind (list armop)) ;; [x0, 16]
  )

(define parse-armop
  (sexp:symbol 'trg _)                                     -> (armop:trg)
  (sexp:symbol 'k _)                                       -> (armop:k)
  (sexp:symbol 'lenv _)                                    -> (armop:lenv)
  (sexp:symbol 'freep _)                                   -> (armop:freep)
  (sexp:symbol 'retval _)                                  -> (armop:retval)
  (sexp:symbol 'xzr _)                                     -> (armop:xzr)
  (sexp:symbol 'sp _)                                      -> (armop:sp)
  (sexp:string label)                                      -> (armop:l (clabel label))
  (sexp:list ((sexp:symbol 'r _) (sexp:symbol name _)))    -> (armop:r (symbol->string name))
  (sexp:list ((sexp:symbol 'r _) (sexp:string name)))      -> (armop:r name)
  (sexp:list ((sexp:symbol 's _) (sexp:int num)))          -> (armop:s num)
  (sexp:list ((sexp:symbol 'a _) (sexp:int num)))          -> (armop:a num)
  (sexp:list ((sexp:symbol 'i _) (sexp:int num)))          -> (armop:i num)
  (sexp:list ((sexp:symbol 'c _) (sexp:int num)))          -> (armop:c num)
  (sexp:list ((sexp:symbol 'lsl _) (sexp:int num)))        -> (armop:lsl num)
  (sexp:list ((sexp:symbol 'lsr _) (sexp:int num)))        -> (armop:lsr num)
  (sexp:list ((sexp:symbol 'asr _) (sexp:int num)))        -> (armop:asr num)
  (sexp:list ((sexp:symbol 'ror _) (sexp:int num)))        -> (armop:ror num)
  (sexp:list ((sexp:symbol 'cc _) (sexp:symbol cc _)))     -> (armop:cc (name->armcc cc))
  (sexp:list ((sexp:symbol 'ind _) . args))                -> (armop:ind (map parse-armop args))
  (sexp:list ((sexp:symbol 'w _) arg))                     -> (armop:w (parse-armop arg))
  ;; we leave out (armop:n) on purpose, since that information shouldn't
  ;;  be user-observable.
  x -> (raise (:ArmAsm/BadExpression (repr x)))
  )

(define parse-arm-inline
  (sexp:list ((sexp:symbol 'com _) (sexp:string comment)))
  -> (arm:comment comment)
  (sexp:list ((sexp:symbol rator _) . rands))
  -> (arm:insn rator (map parse-armop rands))
  ;; this is a hack to handle mnemonics with dots, which look like attribute references to the sexp parser.
  (sexp:list ((sexp:attr (sexp:symbol left _) right) . rands))
  -> (arm:insn
      (string->symbol
       (format (symbol->string left) "." (symbol->string right)))
      (map parse-armop rands))
  x
  -> (raise (:ArmAsm/BadExpression (repr x)))
  )

;; argument must be an 'xN' register.
(define (frob-armop-to-word rand)
  (let ((frand (format-armop rand)))
    (if (eq? #\x (string-ref frand 0))
        (format "w" (substring frand 1 (string-length frand)))
        (raise (:Arm/CannotWordOperand rand)))))

(define format-armop
  (armop:r name)   -> name
  (armop:k)        -> (register->name Rk)
  (armop:lenv)     -> (register->name Rlenv)
  (armop:freep)    -> (register->name Rfreep)
  (armop:retval)   -> (register->name Rreturn)
  (armop:xzr)      -> "xzr"
  (armop:sp)       -> "sp"
  (armop:s 0)      -> (register->name Rscratch)
  (armop:s 1)      -> (register->name Rscratch2)
  (armop:s 2)      -> (register->name Rscratch3)
  (armop:n n)      -> (register->name n)
  (armop:x n)      -> arm-register-names[n]
  (armop:i n)      -> (format (int n))
  (armop:c n)      -> (register->name (Rcarg n))
  (armop:l name)   -> (format name)
  (armop:lsl n)    -> (format "LSL " (int n))
  (armop:lsr n)    -> (format "LSR " (int n))
  (armop:asr n)    -> (format "ASR " (int n))
  (armop:ror n)    -> (format "ROR " (int n))
  (armop:cc cc)    -> (format (sym (armcc->name cc)))
  (armop:ind args) -> (format "[" (join format-armop ", " args) "]")
  (armop:w arg)    -> (frob-armop-to-word arg)
  (armop:a n)      -> (format "a" (int n)) ;; TEMPORARY: let assembler complain
  x                -> (raise (:ArmAsm/BadOperand x))
  )

;; DSL for asm in the code below.
;; (intent is to be identical to sexp version)

;; one armop
(defmacro ARMOP
  (ARMOP <k>)         -> (armop:k)
  (ARMOP <lenv>)      -> (armop:lenv)
  (ARMOP <freep>)     -> (armop:freep)
  (ARMOP <retval>)    -> (armop:retval)
  (ARMOP <xzr>)       -> (armop:xzr)
  (ARMOP <sp>)        -> (armop:sp)
  (ARMOP (<r> n))     -> (armop:r n)
  ;;(ARMOP (<a> n)) -> (armop:a n)  ;; NO! these are for inline asm only
  ;;(ARMOP <trg>    -> (armop:a n)  ;; NO!
  (ARMOP (<s> n))     -> (armop:s n)
  (ARMOP (<n> n))     -> (armop:n n)
  (ARMOP (<x> n))     -> (armop:x n) ;; used by peephole
  (ARMOP (<i> n))     -> (armop:i n)
  (ARMOP (<c> n))     -> (armop:c n)
  (ARMOP (<l> l))     -> (armop:l l)
  ;; 'g' for 'global symbol'
  (ARMOP (<g> l))     -> (armop:l (clabel l))
  (ARMOP (<lsl> n))   -> (armop:lsl n)
  (ARMOP (<lsr> n))   -> (armop:lsr n)
  (ARMOP (<asr> n))   -> (armop:asr n)
  (ARMOP (<ror> n))   -> (armop:ror n)
  (ARMOP (<cc> cc))   -> (armop:cc (cc->armcc cc))
  (ARMOP (<w> sub))   -> (armop:w (ARMOP sub))
  (ARMOP (<ind> op ...)) -> (armop:ind (ARMOPS op ...))
  )

;; list of armops
(defmacro ARMOPS
  (ARMOPS)            -> (list:nil)
  (ARMOPS op ops ...) -> (list:cons (ARMOP op) (ARMOPS ops ...))
  )

;; one arminsn
(defmacro ARMINSN
  ;; could do format here ...
  (ARMINSN <com> s ...) -> (arm:comment (format s ...))
  (ARMINSN <label> l)   -> (arm:label l)
  (ARMINSN <global> l)  -> (arm:global l)
  (ARMINSN op args ...) -> (arm:insn op (ARMOPS args ...))
  )

;; list of arminsns
(defmacro ARM
  (ARM)                        -> (list:nil)
  ;; verbatim insert
  (ARM <=> x insns ...)        -> (list:cons (arm:verb x) (ARM insns ...))
  ;; '@' = splice in place (similar to UNQUOTE-SPLICING in Scheme)
  (ARM <@> sub)                -> sub
  (ARM <@> sub insns ...)      -> (append2 sub (ARM insns ...))
  (ARM (insn ...) insns ...)  -> (list:cons (ARMINSN insn ...) (ARM insns ...))
  )

(define (cps->arm cps)

  (let ((used-jumps (find-jumps cps))
        (ltorg-flag #f))

    (define new-label
      (let ((counter 0))
        (lambda (prefix)
          (inc! counter)
          (format "L" prefix (int counter)))))

    (define (emitk acc k)
      (if (null-cont? k)
          acc
          (append acc (emit k.insn))))

    ;; XXX these should really go in backend.scm and not be duplicated
    ;;     by each backend.

    (define (UITAG n) (+ TC_USERIMM (<< n TAGSIZE)))
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

    (define (emit-return reg)
      (ARM ('mov retval (n reg))
           ('return (s 0))))

    (define (emit-immediate imm trg)
      ;; immediates are tricky on arm, because of the fixed-size insns
      ;; there are several different ways to load immediates, depending
      ;; on their size (and even bit patterns).
      (if (< imm (<< 1 16))
          (ARM ('mov (n trg) (i imm)))
          (if (< imm (<< 1 32))
              (let ((lo (logand #xffff imm))
                    (hi (>> imm 16)))
                (ARM ('movz (n trg) (i lo))
                     ('movk (n trg) (i hi) (lsl 16))))
              (begin
                (set! ltorg-flag #t)
                (ARM
                 ('ldr (n trg) (l (format "=0x" (hex imm))))))
              )))

    (define (emit-literal lit trg)
      (if (= trg -1)
          (list:nil) ;; dead literal
          (emit-immediate (encode-immediate lit) trg)))

    ;; I think obsolete now.
    ;; llvm -> arm64 arithmetic insns
    (define llop->armop
      'shl  -> 'lsl
      'ashr -> 'asr
      'or   -> 'orr
      'xor  -> 'eor
      other -> other
      )

    ;; wtf
    (define emit-arith1
      'srem arg0 arg1 target
      -> (ARM ;; arm64 has no rem/mod op, instead uses fused mul-sub in combo.
          ('udiv (s 0) (n arg0) (n arg1))
          ('msub (n target) (s 0) (n arg1) (n arg0)))
      op arg0 arg1 target
      -> (ARM ((llop->armop op) (n target) (n arg0) (n arg1)))
      )

    (define (emit-arith op arg0 arg1 target)
      ;; XXX think about ways to avoid untag/tag, depending on <op>
      ;; e.g. (2a+1)+(2b+1) == 2a + 2b + 2 == 2(a+b)+2 so leave tagged and sub1 from result.
      (ARM
       ('untag (n arg0))
       ('untag (n arg1))
       @ (emit-arith1 op arg0 arg1 target)
       ('tag (n target))
       ))

    ;; we can do this all in one go using the udiv/fused-mulsub
    ;; note: can't be done with inline asm until it supports :tuple returns.
    (define (emit-divmod arg0 arg1 target)
      ;; we need the tag for `(:tuple x y)`.
      (let ((tuple-tag (alist/get the-context.variant-labels 'tuple "no :tuple polyvar?")))
        (ARM
         ('untag (n arg0))
         ('untag (n arg1))
         ('udiv (s 0) (n arg0) (n arg1))
         ('msub (s 2) (s 0) (n arg1) (n arg0))
         ('alloc (n target) (i (UOTAG tuple-tag)) (i 2))
         ('tag (s 0))
         ('str (s 0) (ind (n target) (i 8)))
         ('tag (s 2))
         ('str (s 2) (ind (n target) (i 16)))
        )))

    (define cc->armcc
      ;; translate between llvm CC and arm CC
      ;; there are more condition codes, e.g. PL/MI for pos/neg,
      ;; might be useful.
      'eq  -> (armcc:eq)
      'slt -> (armcc:lt)
      'sle -> (armcc:le)
      'sgt -> (armcc:gt)
      'sge -> (armcc:ge)
      'ugt -> (armcc:hi)
      'ult -> (armcc:cc)
      'ule -> (armcc:ls)
      'uge -> (armcc:cs)
      cc -> (raise (:UnknownConditionCode cc))
      )

    (define (emit-icmp cc arg0 arg1 target)
      ;; we use CSEL here, which needs the two values in registers
      ;; let's put #t in target, and #f in scratch and csel from there?
      (ARM
       ('cmp (n arg0) (n arg1))
       ;; we avoid needing another scratch register by putting the cmp
       ;; before wiping out target (which is likely to be arg0 or arg1)
       @ (emit-literal (literal:bool #t) target)
       @ (emit-literal (literal:bool #f) Rscratch)
       ('csel (n target) (n target) (s 0) (cc cc))))

    (define (llcc-suffix op cc)
      (string->symbol (format (sym op) (sym (armcc->name (cc->armcc cc))))))

    (define (emit-test-cmp cc arg0 arg1 reg jn k0 k1 k)
      (let ((skip-label (new-label "L"))
            (jcont (emit-jump-continuation jn k.insn)))
        (ARM
         (com "test-cmp")
         ('cmp (n arg0) (n arg1))
         ((llcc-suffix 'b cc) (l skip-label))
         @ (emit k1)
         (label skip-label)
         @ (emit k0)
         @ jcont
         )))

    (define (emit-dtcon dtname altname args target)
      (match (alist/lookup the-context.datatypes dtname) with
        (maybe:no)
        -> (raise (:NoSuchDatatype "emit-dtcon" dtname))
        (maybe:yes dt)
        -> (let ((alt (dt.get altname))
                 (nargs (length args)))
             (if (= nargs 0) ;; immediate constructor
                 (emit-immediate (get-uitag dtname altname alt.index) target)
                 ;; we can't build directly into target because it's likely target is in args.
                 (ARM
                  (com "dtcon " (sym dtname) "." (sym altname))
                  ('alloc (s 0) (i (get-uotag dtname altname alt.index)) (i nargs))
                  @ (map-range j nargs (ARMINSN 'str (n (nth args j)) (ind (s 0) (i (* 8 (+ j 1))))))
                  ('mov (n target) (s 0)))))))

    (define (emit-nvget target reg index)
      (ARM
       (com "nvget " (int target) " = r" (int reg) "[" (int index) "]")
       ('ldr (n target) (ind (n reg) (i (* 8 (+ index 1)))))
       ))

    (define (ambig code)
      (tree/insert! the-context.ambig-rec int-cmp code #u))

    (define (emit-record-get label sig rec trg free)
      (let ((label-code (lookup-label-code label)))
	(match (guess-record-type sig) with
	  (maybe:yes sig0)
          -> (ARM
              (com "%record-get " (int rec) "." (sym label))
              ;; this probably needs an lsl
              ('ldr (n trg) (ind (n rec) (i (* 8 (+ 1 (index-eq label sig0)))))))
	  (maybe:no)
          -> (begin
               (ambig label-code)
               (ARM
                (com "ambig %record-get " (sym label) " code " (int label-code))
                ('mov (c 0) (n rec))
                ('mov (c 1) (i label-code))
                @ (bl-with-save-restore "record_fetch" free)
                ('mov (n trg) (c 0))
                ))
          )))

    (define (emit-record-set label sig rec val free)
      (let ((label-code (lookup-label-code label)))
	(match (guess-record-type sig) with
	  (maybe:yes sig0)
          -> (ARM
              (com "%record-set " (int rec) "." (sym label))
              ;; this probably needs an lsl
              ('str (n val) (ind (n rec) (i (* 8 (+ 1 (index-eq label sig0)))))))
	  (maybe:no)
          -> (begin
               (ambig label-code)
               (ARM
                (com "ambig %record-set " (sym label) " code " (int label-code))
                ('mov (c 0) (n rec))
                ('mov (c 1) (i label-code))
                ('mov (c 2) (n val))
                @ (bl-with-save-restore "record_store" free)
                ))
          )))

    (define (frob-arg i type)
      (match type with
        (type:tvar id _) -> (list:nil)
        (type:pred name predargs _)
        -> (match name with
             'int          -> (ARM ('untag (c i)))
             'bool         -> (ARM ('unbool (c i)))
             'string       -> (ARM ('unstring (c i)))
             'cref         -> (ARM ('unforeign (c i)))
             'char         -> (list:nil) ;; seems wrong.
             'arrow        -> (list:nil)
             'vector       -> (list:nil) ;; should be raw pointer?
             'symbol       -> (list:nil)
             'continuation -> (list:nil)
             'raw          -> (list:nil)
             kind          -> (if (member-eq? kind c-int-types)
                                  (ARM ('untag (c i)))
                                  (error1 "arm frob-arg:" (type-repr type)))
             )))

    ;; since the args are already in Carg 0..n, we just need to know
    ;; how many there are.
    (define (frob-inputs sig nargs)
      (match sig with
        (type:pred 'arrow (result-type . arg-types) _)
        -> (let ((code (list:nil)))
             (for-range i nargs
               (push! code (frob-arg i (nth arg-types i))))
             (flatten (reverse code)))
        _ ;; some constant type
        -> (list:nil)
        ))

    (define (frob-retval type)
      (match type with
        (type:pred 'long _ _)    -> (ARM ('tag (c 0)))
        (type:pred 'short _ _)   -> (ARM ('tagh (c 0) (w (c 0))))
        (type:pred 'int _ _)     -> (ARM ('tagw (c 0) (w (c 0))))
        (type:pred 'cref _ _)    -> (ARM ('mov (s 0) (c 0)) ('make_foreign (c 0) (s 0)))
        (type:pred '* _ _)       -> (ARM ('mov (s 0) (c 0)) ('make_foreign (c 0) (s 0)))
        (type:pred 'void _ _)    -> (ARM ('mov (c 0) (i TC_UNDEFINED)))
        (type:pred kind _ _)     -> (if (member-eq? kind c-int-types)
                                        (ARM ('tag (c 0)))
				        (list:nil))
        _                        -> (list:nil)
        ))

    (define (frob-output sig)
      (match sig with
        (type:pred 'arrow (result-type . arg-types) _)
        -> (frob-retval result-type)
        other
        -> (frob-retval other)
        ))

    ;; irken registers that live in caller-saved machine registers
    ;;  (x5-x17), which any C function may clobber.
    (define caller-saves-set
      (let ((r '()))
        (for-range i (vector-length arm-register-file)
          (when (< arm-register-file[i] 19)
            (push! r i)))
        r))

    ;; code to save/restore the registers in `free` that a C call may
    ;;   clobber, as (:tuple saves restores).  `free` should include any
    ;;   register the insn itself still reads after the call.  sp must
    ;;   stay 16-byte aligned, so the save area is rounded up.
    (define (save-restore free)
      (let ((regs '()))
        (for-list reg free
          (when (and (member-eq? reg caller-saves-set)
                     (not (member-eq? reg regs)))
            (push! regs reg)))
        (let ((nregs (length regs))
              (nbytes (* 8 (+ nregs (logand nregs 1)))))
          (if (= nregs 0)
              (:tuple (list:nil) (list:nil))
              (:tuple
               (ARM
                (com "save caller-saved: [" (join int->string ", " regs) "]")
                ('sub sp sp (i nbytes))
                @ (map-range i nregs (ARMINSN 'str (n (nth regs i)) (ind sp (i (* 8 i))))))
               (ARM
                @ (map-range i nregs (ARMINSN 'ldr (n (nth regs i)) (ind sp (i (* 8 i)))))
                ('add sp sp (i nbytes))))))))

    (define (bl-with-save-restore name free)
      (let (((saves restores) (save-restore free)))
        (ARM
         @ saves
         ('bl (g name))
         @ restores)))

    ;; arguments past the eighth go on the stack.  AAPCS64 gives each one
    ;;  an 8-byte slot; Apple's arm64 ABI packs them at their natural size
    ;;  and alignment instead (an int takes 4 bytes).
    (define (stack-arg-size type)
      (if (eq? the-context.options.platform 'Darwin)
          (match type with
            (maybe:yes (type:pred kind _ _))
            -> (match kind with
                 'int -> 4 'uint -> 4 'int32_t -> 4 'uint32_t -> 4
                 'short -> 2 'ushort -> 2 'int16_t -> 2 'uint16_t -> 2
                 'int8_t -> 1 'uint8_t -> 1
                 _ -> 8)
            _ -> 8)
          8))

    ;; the C type of argument <i>, if the signature says.
    (define (sig-arg-type sig i)
      (match sig with
        (type:pred 'arrow (result-type . arg-types) _)
        -> (if (< i (length arg-types)) (maybe:yes (nth arg-types i)) (maybe:no))
        _ -> (maybe:no)))

    ;; -> (:tuple code nbytes): store args 8.. into a new stack area,
    ;;   converting each one on the way through x0 (which is loaded last).
    (define (store-stack-args sig args convert? csizes)
      (let ((nargs (length args))
            (code '())
            (off 0))
        (for-range i nargs
          (when (>= i 8)
            (let ((type (sig-arg-type sig i))
                  (size (cond ((not (eq? the-context.options.platform 'Darwin)) 8)
                              ((< i (length csizes)) (nth csizes i))
                              (else (stack-arg-size type)))))
              ;; natural alignment
              (set! off (* size (/ (+ off (- size 1)) size)))
              (push! code (ARM ('mov (c 0) (n (nth args i)))))
              (when convert?
                (match type with
                  (maybe:yes t) -> (push! code (frob-arg 0 t))
                  (maybe:no)    -> #u))
              (push! code
                     (match size with
                       8 -> (ARM ('str (c 0) (ind sp (i off))))
                       4 -> (ARM ('str (w (c 0)) (ind sp (i off))))
                       2 -> (ARM ('strh (w (c 0)) (ind sp (i off))))
                       _ -> (ARM ('strb (w (c 0)) (ind sp (i off))))))
              (set! off (+ off size)))))
        (let ((nbytes (* 16 (/ (+ off 15) 16))))
          (:tuple (ARM
                   ('sub sp sp (i nbytes))
                   @ (flatten (reverse code)))
                  nbytes))))

    ;; x0-x7 carry the first eight arguments.  two hazards:
    ;;  * x5-x7 are also irken registers 19-17, so loading the args in
    ;;    order could overwrite one before it's read (args 5..7 may live
    ;;    in x5..x7).  [the arm64 twin of the amd64 r8/r9 bug.]
    ;;  * converting a cref (unforeign) needs scratch registers (x2/x3),
    ;;    which are also argument registers.
    ;; so in either case each arg is converted in x0 and stored to the
    ;;  stack, and then x0..x7 are loaded from there.
    (define (load-c-args sig args convert?)
      (let ((nargs (min 8 (length args)))
            (types (map (lambda (i) (sig-arg-type sig i)) (range nargs)))
            (cref? (and convert?
                        (some? (lambda (t)
                                 (match t with
                                   (maybe:yes (type:pred 'cref _ _)) -> #t
                                   _ -> #f))
                               types))))
        (if (and (<= nargs 5) (not cref?))
            (ARM
             @ (map-range i nargs (ARMINSN 'mov (c i) (n (nth args i))))
             @ (if convert? (frob-inputs sig nargs) (list:nil)))
            (let ((nbytes (* 16 (/ (+ nargs 1) 2)))) ;; keep sp 16-byte aligned
              (ARM
               ('sub sp sp (i nbytes))
               @ (flatten
                  (map-range
                   i nargs
                   (if convert?
                       (ARM
                        ('mov (c 0) (n (nth args i)))
                        @ (match (nth types i) with
                            (maybe:yes t) -> (frob-arg 0 t)
                            (maybe:no)    -> (list:nil))
                        ('str (c 0) (ind sp (i (* 8 i)))))
                       (ARM ('str (n (nth args i)) (ind sp (i (* 8 i))))))))
               @ (map-range i nargs (ARMINSN 'ldr (c i) (ind sp (i (* 8 i)))))
               ('add sp sp (i nbytes)))))))

    (define (emit-c-call sig name args target free convert?)
      (emit-c-call* sig name args target free convert? '()))

    ;; csizes: the C size of each argument, if known.
    (define (emit-c-call* sig name args target free convert? csizes)
      (let ((nargs (length args))
            ((stack-code stack-bytes)
             (if (> nargs 8)
                 (store-stack-args sig args convert? csizes)
                 (:tuple (list:nil) 0))))
        ;; save caller-saved irken registers *before* loading the args:
        ;;  x5-x7 are both, and must be saved with their irken values.
        (let (((saves restores) (save-restore free)))
          (ARM
           @ saves
           ;; stack args first: they go through x0, before x0-x7 are loaded.
           @ stack-code
           ;; put the args, converted (irken->c) if need be, into x0..x7
           @ (load-c-args sig args convert?)
           ;; call the function
           ('bl (g name))
           @ (if (> stack-bytes 0) (ARM ('add sp sp (i stack-bytes))) (list:nil))
           @ restores
           @ (if (= target -1)
                 (list:nil)
                 (ARM
                  @ (if convert? (frob-output sig) (list:nil))
                    ('mov (n target) retval)))
             ))))

    (define (emit-arm-inline inline args target)

      (define frob-armop
        ;; convert these into irken register number
        (armop:trg)      -> (armop:n target)
        (armop:a n)      -> (armop:n (nth args n))
        (armop:ind args) -> (armop:ind (map frob-armop args))
        x                -> x
        )

      (define frob-arminsn
        (arm:insn name args) -> (arm:insn name (map frob-armop args))
        other                -> other
        )

      (let ((parsed (map parse-arm-inline inline)))
        (list:cons
         (arm:comment "arm inline")
         (map frob-arminsn parsed))
        ))

    (define (emit-array-ref vec index target)
      (ARM
       ('untag (n index))
       ('rangecheck (n vec) (n index))
       ('add (s 0) (n vec) (n index) (lsl 3))
       ('ldr (n target) (ind (s 0) (i 8)))))

    (define (emit-array-set vec index val)
      (ARM
       ('untag (n index))
       ('rangecheck (n vec) (n index))
       ('add (s 0) (n vec) (n index) (lsl 3))
       ('str (n val) (ind (s 0) (i 8)))))

    (define (emit-exit arg)
      (ARM
       ('mov retval (n arg))
       ('b  (l "Lreturn"))
       ))

    (define (emit-putcc rk rv target)
      (ARM
       (com "putcc")
       ('mov k (n rk))
       ('mov (n target) (n rv))))

    (define (emit-getcc target)
      (ARM
       (com "getcc")
       ('mov (n target) k)))

    (define (emit-ensure-heap size free)
      (let ((L0 (new-label "EH"))
            (nfree (length free)))
        (ARM
         (com "%ensure-heap: size in " (int size) " nfree = " (int nfree))
         ('ldr_addr (s 0) (g "limit"))
         ('sub (s 0) (s 0) (n size) (lsl 2)) ;; N.B. hidden 'untag', nearly.
         ('cmp freep (s 0))
         ('blt (l L0))
         ;; need to gc
         ;; add free vars as roots
         ('ldr_addr (s 0) (g "heap1"))
         @ (map-range i nfree (ARMINSN 'str (n (nth free i)) (ind (s 0) (i (* 8 (+ i 3))))))
         ('mov (c 0) (i nfree))
         ;; invoke GC
         ('bl (l "irk_gc_for_ensure_heap"))
         ;; restore free vars
         ('ldr_addr (s 0) (g "heap0"))
         @ (map-range i nfree (ARMINSN 'ldr (n (nth free i)) (ind (s 0) (i (* 8 (+ i 3))))))
         (label L0)
         )))

    ;; these three convert in a scratch register rather than in place:
    ;;  the source may be a variable that's still live.
    ;; %string->cref makes TC_FOREIGN <string> <offset> (see make_string_foreign
    ;;  in header1.c), not a raw pointer into the heap: the pointer has to
    ;;  follow the string when the collector moves it.  the characters are
    ;;  at offset 4 from the end of the header (after the 32-bit length).
    (define (emit-string->cref s target)
      (ARM
       (com "%string->cref")
       ('alloc (s 0) (i #x24) (i 2))
       ('str (n s) (ind (s 0) (i 8)))
       ('mov (s 1) (i 9)) ;; (4<<1)|1
       ('str (s 1) (ind (s 0) (i 16)))
       ('mov (n target) (s 0))
       ))

    (define (emit-cref->int src target)
      (ARM
       ('mov (s 0) (n src))
       ('unforeign (s 0))
       ('mov (n target) (s 0))
       ('tag (n target))))

    (define (emit-int->cref src target)
      (ARM
       (com "%int->cref")
       ('mov (s 0) (n src))
       ('untag (s 0))
       ('make_foreign (n target) (s 0))))

    (define (emit-c-aref type src index target free)
      (let ((ctype (irken-type->ctype (un-cref type)))
            (size (ctype->size ctype)))
        ;; note: this allocates, so we must save/restore freep.
        (ARM
         ('untag (n index))
         ('str_addr (s 0) freep (g "freep"))
         ('mov (c 0) (n src))
         ('mov (c 1) (i size))
         ('mul (c 1) (c 1) (n index))
         @ (bl-with-save-restore "offset_foreign" free)
         ('mov (n target) (c 0))
         ('ldr_addr freep (g "freep"))
         )))

    (define (emit-halloc-malloc type count target fun-name free)
      (let ((ctype (parse-ctype type))
            (size (ctype->size ctype)))
        (if (< target 0)
            (error "dead %halloc?")
            (ARM
             ('str_addr (s 0) freep (g "freep"))
             ('untag (n count))
             ('mov (c 0) (i size))
             ('mov (c 1) (n count))
             @ (bl-with-save-restore fun-name free)
             ('ldr_addr freep (g "freep"))
             ('mov (n target) (c 0))))))

    (define (emit-halloc type count target free)
      (emit-halloc-malloc type count target "make_halloc" free))

    (define (emit-malloc type count target free)
      (emit-halloc-malloc type count target "make_malloc" free))

    ;; the syntax for load/store changes depending on data size.
    (define (str-by-size size dst src)
      (match size with
        1 -> (ARM ('strb (w (n src)) (ind (n dst))))
        2 -> (ARM ('strh (w (n src)) (ind (n dst))))
        4 -> (ARM ('str  (w (n src)) (ind (n dst))))
        8 -> (ARM ('str      (n src) (ind (n dst))))
        x -> (error (format "str-by-size - bad size: " (int x)))
        ))

    ;; note: this relies on a hack in cps.scm that sets the type of this insn to the type of the rval.
    (define (emit-c-set-int type dst src free)
      (let ((ctype (irken-type->ctype type))
            (size (ctype->size ctype)))
        ;; set target? e.g (let ((_ (set! thing...))))
        (ARM
         (com "c-set-int " (type-repr type))
         ('mov (c 0) (n dst))
         ;; src is read after the call, so it must survive it too.
         @ (bl-with-save-restore "get_foreign" (list:cons src free))
         ('untag (n src))
         @ (str-by-size size (Rcarg 0) src)
         )))

    ;; signed C types must be sign-extended: e.g. a `short` kevent
    ;;   filter of -1 would otherwise read as 65535.
    (define (ldr-by-size size signed? src trg)
      (match size signed? with
        1 #t -> (ARM ('ldrsb (n trg)     (ind (n src))))
        2 #t -> (ARM ('ldrsh (n trg)     (ind (n src))))
        4 #t -> (ARM ('ldrsw (n trg)     (ind (n src))))
        1 #f -> (ARM ('ldrb (w (n trg)) (ind (n src))))
        2 #f -> (ARM ('ldrh (w (n trg)) (ind (n src))))
        4 #f -> (ARM ('ldr  (w (n trg)) (ind (n src))))
        8 _  -> (ARM ('ldr  (n trg)     (ind (n src))))
        x _  -> (error (format "ldr-by-size - bad size: " (int x)))
        ))

    (define (emit-c-get-int type src trg free)
      (let ((ctype (irken-type->ctype type))
            (size (ctype->size ctype)))
        (ARM
         (com "c-get-int " (type-repr type))
         ('mov (c 0) (n src))
         @ (bl-with-save-restore "get_foreign" free)
         @ (ldr-by-size size (ctype-signed? ctype) (Rcarg 0) trg)
         ('tag (n trg))
         )))

    (define (emit-cref->string src len trg free)
      (ARM
       (com "%cref->string")
       ('str_addr (s 0) freep (g "freep"))
       ('mov (c 0) (n src))
       ('mov (c 1) (n len))
       @ (bl-with-save-restore "irk_cref_2_string" free)
       ('mov (n trg) (c 0))
       ('ldr_addr freep (g "freep"))))

    (define (emit-sref refexp src trg free)
      (let ((ref0 (sexp->ref refexp)))
        (ARM
         (com "%c-sref")
         ('str_addr (s 0) freep (g "freep"))
         ('mov (c 0) (n src))
         ('mov (c 1) (i ref0.off))
         @ (bl-with-save-restore "offset_foreign" free)
         ('mov (n trg) (c 0))
         ('ldr_addr freep (g "freep")))))

    ;; %c-get-ptr allocates a new foreign, so we must save/restore freep.
    (define (emit-c-get-ptr src trg free)
      (ARM
       (com "%c-get-ptr")
       ('str_addr (s 0) freep (g "freep"))
       ('mov (c 0) (n src))
       @ (bl-with-save-restore "irk_c_get_ptr" free)
       ('mov (n trg) (c 0))
       ('ldr_addr freep (g "freep"))))

    (define (emit-c-set-ptr dst src free)
      (ARM
       (com "%c-set-ptr")
       ('mov (c 0) (n dst))
       ('mov (c 1) (n src))
       @ (bl-with-save-restore "irk_c_set_ptr" free)))

    (define (emit-free ref free)
      (ARM
       (com "%free")
       ('mov (c 0) (n ref))
       @ (bl-with-save-restore "free_foreign" free)))

    (define (emit-c-sizeof ctexp trg)
      (let ((ctype (parse-ctype ctexp)))
        (ARM
         (com "%c-sizeof")
         @ (emit-immediate (ctype->size ctype) trg)
         ('tag (n trg)))))

    (define (emit-primop name params type args k)
      (match name params args with
        '%llarith (sexp:symbol op _) (arg0 arg1)           -> (emit-arith op arg0 arg1 k.target)
        '%divmod  _                  (arg0 arg1)           -> (emit-divmod arg0 arg1 k.target)
        '%llicmp  (sexp:symbol cc _) (arg0 arg1)           -> (emit-icmp cc arg0 arg1 k.target)
        '%lleq    _                  (arg0 arg1)           -> (emit-icmp 'eq arg0 arg1 k.target)
        '%dtcon   (sexp:cons dtname altname) args          -> (emit-dtcon dtname altname args k.target)
	'%nvget   (sexp:list (_ (sexp:int index) _)) (reg) -> (emit-nvget k.target reg index)
        '%arm (sexp:list (sig . inline)) args              -> (emit-arm-inline inline args k.target)
	'%array-ref _ (vec index)                          -> (emit-array-ref vec index k.target)
	'%array-set _ (vec index val)                      -> (emit-array-set vec index val)
        '%exit _ (arg)                                     -> (emit-exit arg)
        '%getcc _ ()                                       -> (emit-getcc k.target)
        '%putcc _ (k0 v0)                                  -> (emit-putcc k0 v0 k.target)
        '%ensure-heap _ (size)                             -> (emit-ensure-heap size k.free)
        '%string->cref _     (s)                           -> (emit-string->cref s k.target)
        '%cref->int    _   (src)                           -> (emit-cref->int src k.target)
        '%int->cref    _   (src)                           -> (emit-int->cref src k.target)
        '%c-aref _ (ref index)                             -> (emit-c-aref type ref index k.target k.free)
        '%halloc type (count)                              -> (emit-halloc type count k.target k.free)
        '%malloc type (count)                              -> (emit-malloc type count k.target k.free)
        '%c-set-int _    (dst src)                         -> (emit-c-set-int type dst src k.free)
        '%c-get-int _    (src)                             -> (emit-c-get-int type src k.target k.free)
        '%cref->string _ (src len)                         -> (emit-cref->string src len k.target k.free)
        '%c-sref refexp (src)                              -> (emit-sref refexp src k.target k.free)
        '%c-get-ptr _    (src)                             -> (emit-c-get-ptr src k.target k.free)
        '%c-set-ptr _    (dst src)                         -> (emit-c-set-ptr dst src k.free)
        '%free _ (ref)                                     -> (emit-free ref k.free)
        '%c-sizeof ctexp ()                                -> (emit-c-sizeof ctexp k.target)

	'%record-get (sexp:list ((sexp:symbol label _) (sexp:list sig))) (rec)
        -> (emit-record-get label sig rec k.target k.free)

        '%record-set (sexp:list ((sexp:symbol label _) (sexp:list sig))) (rec val)
        -> (emit-record-set label sig rec val k.free)

        '%asm-c-call (sexp:list ((sexp:string name) sig)) args
        -> (emit-c-call (parse-type sig) name args k.target k.free #t)

        '%asm-c-call (sexp:list ((sexp:string name) sig (sexp:bool convert?))) args
        -> (emit-c-call (parse-type sig) name args k.target k.free convert?)

        _ _ _
        -> (begin
             (printf "unknown primop: " (sym name) "\n")
             (raise (:Arm/UnknownPrimop)))
        ))

    (define (emit-jump-continuation jn k)
      (match (used-jumps::get jn) with
        (maybe:yes free) -> (emit k)
        (maybe:no)       -> (list:nil)
        ))

    (define (emit-test reg jn k0 k1 k)
      (let ((skip-label (new-label "L"))
            (jcont (emit-jump-continuation jn k.insn)))
        (ARM
         @ (emit-literal (literal:bool #f) Rscratch)
         ('cmp (s 0) (n reg))
         ('beq (l skip-label))
         @ (emit k0)
         (label skip-label)
         @ (emit k1)
         @ jcont
         )))

    (define (emit-label jn next)
      (ARM
       (label (format "J" (int jn)))
       @ (emit next)
       ))

    (define (emit-jump reg target jn free)
      (ARM
       @ (if (not (= target -1))
             (ARM ('mov (n target) (n reg)))
             (ARM))
       ('b (l (format "J" (int jn))))
       ))

    ;; insn:move has two different meanings/uses,
    ;;  from either varref or varset.
    (define (emit-move var src target)
      ;; MOV <dst-ref> <src-reg>
      (cond ((and (>= src 0) (not (= src var)))
             ;; from varset
             (ARM ('mov (n var) (n src))))
            ((and (>= target 0) (not (= target var)))
             ;; from varref
             (ARM ('mov (n target) (n var))))
            (else (list:nil))))

    (define (emit-alloc tag size target)
      (if (= size 0)
          (emit-immediate (UITAG tag) target)
          (ARM ('alloc (n target) (i (UOTAG tag)) (i size)))))

    (define (emit-store off arg tup index)
      (ARM
       (com "store off=" (int off) " arg=" (int arg) " tup=" (int tup) " index=" (int index))
       ('str (n arg) (ind (n tup) (i (* 8 (+ 1 off index)))))))

    ;; emit an '.ltorg' directive to tell the assembler to drop a literal pool
    ;; here, since a 64-bit literal was used recently.
    ;; we do this just before the beginning of a function.
    (define (ltorg-hack)
      (if ltorg-flag
          (begin
            (set! ltorg-flag #f)
            (ARM = ".ltorg"))
          (ARM)))

    (define (emit-close name nreg body target)
      (let ((l0 (new-label "L"))
            (flabel (safe-function-name name))
            (gc-check (if (vars-get-flag name VFLAG-ALLOCATES)
                          (ARM ('heapcheck (s 0)))
                          '())))
        (ARM
         ('b (l l0))
         @ (ltorg-hack)
         (label flabel)
         @ gc-check
         @ (emit body)
         (label l0)
         ('alloc (n target) (i TC_CLOSURE) (i 2))
         ('adr (s 0) (l flabel))
         ('str (s 0) (ind (n target) (i 8)))
         ('str lenv (ind (n target) (i 16)))
         )))

    ;; this happens with (let ((_ (set! x y))) ...)
    (define (dead-set target)
      (if (not (= target -1))
          (ARM ('mov (n target) (i TC_UNDEFINED)))
          (ARM)))

    ;; to think about: high-depth varref could be:
    ;; 1) a 'normal' funcall to varref()
    ;; 2) a bl to a scratch-only function to get the right rib?
    ;; 3) same, but with different labels for different depths [only one copy]

    (define (emit-varref depth index target)
      (match depth with
        0 -> (ARM ('ldr (n target) (ind lenv (i (* 8 (+ index 2))))))
        -1 -> (ARM ('topref (n target) (i index)))
        _  -> (ARM
               (com "varref " (int depth) ", " (int index))
               ('mov (s 0) lenv)
               @ (n-of depth (ARMINSN 'ldr (s 0) (ind (s 0) (i 8))))
               ('ldr (n target) (ind (s 0) (i (* 8 (+ index 2))))))
        ))

    (define (emit-varset* depth index val)
      (match depth with
         0 -> (ARM ('str (n val) (ind lenv (i (* 8 (+ index 2))))))
        -1 -> (ARM ('topset (s 0) (n val) (i index)))
        _  -> (ARM
               (com "varset " (int depth) ", " (int index) " = " (int val))
               ('mov (s 0) lenv)
               @ (n-of depth (ARMINSN 'ldr (s 0) (ind (s 0) (i 8))))
               ('str (n val) (ind (s 0) (i (* 8 (+ index 2))))))
        ))

    (define (emit-varset depth index val target)
      (ARM @ (emit-varset* depth index val)
           @ (dead-set target)))

    (define (emit-new-env size top? types target)
      ;; types are just for comment metadata [for now]
      (ARM
       (com "new-env size=" (int size))
       ('alloc (n target) (i TC_ENV) (i (+ size 1)))
       @ (if top?
             (ARM (com "set top")
                  ('str_addr (s 0) (n target) (g "top")))
             (list:nil))))

    (define (emit-push reg)
      (ARM
       (com "push " (int reg))
       ('str lenv (ind (n reg) (i 8))) ;; r[1] = lenv
       ('mov lenv (n reg))))           ;; lenv = r

    (define (emit-pop src target)
      (ARM
       (com "pop " (int src) ", " (int target))
       ('ldr lenv (ind lenv (i 8))) ;; lenv = lenv[1]
       @ (if (and (>= target 0) (not (= target src)))
             (ARM ('mov (n target) (n src))) ;; target := src
             (list:nil))
         ))

    (define (safe-function-name name)
      (format "F" (frob-name (symbol->string name))))

    (define (funcall mname funreg)
      (match mname with
        (maybe:yes name)
        -> (ARM ('b (l (safe-function-name name)))) ;; known function
        (maybe:no)
        -> (ARM
            ('ldr (s 0) (ind (n funreg) (i 8)))  ;; address via closure
            ('br (s 0)))
        ))

    (define (emit-tail mname fun args)
      (let ((call (funcall mname fun)))
        (match args with
          -1 -> (ARM
                 (com "tail " (maybe mname symbol->string "none") " noargs")
                 ('ldr lenv (ind (n fun) (i 16))) ;; lenv = fun[2]
                 @ call) ;; no args
          _  -> (ARM
                 (com "tail " (maybe mname symbol->string "none") " Rargs " (int args))
                 ('ldr (s 0) (ind (n fun) (i 16))) ;; s0 = fun[2]
                 ('str (s 0) (ind (n args) (i 8))) ;; args[1] = s0
                 ('mov lenv (n args))
                 @ call)
          )))

    (define (emit-trcall depth name regs)
      (let ((nargs (length regs))
            (npop (if (= nargs 0) depth (- depth 1)))
            (name (safe-function-name name))
            (pops (n-of npop (ARMINSN 'ldr lenv (ind lenv (i 8)))))
            (stores (map-range
                        i nargs
                        (ARMINSN 'str (n (nth regs i))
                                 (ind lenv (i (* 8 (+ 2 i))))))))
        (ARM
         (com "trcall " name " depth " (int depth) " regs " (join int->string "," regs) " npop " (int npop))
         @ pops
         @ stores
         ('b (l name)))))

    ;; we emit insns for k0, which may or may not jump to fail continuation in k1
    (define (emit-fatbar label jn k0 k1 k)
      (let ((lfail (format "Fail" (int label))))
        (ARM
         @ (emit k0)
         (label lfail)
         @ (emit k1)
         @ (emit-jump-continuation jn k.insn)
         )))

    (define (emit-fail label npop free)
      (ARM
       @ (n-of npop (ARMINSN 'ldr lenv (ind lenv (i 8))))
       ('b (l (format "Fail" (int label))))))

    (define (emit-call mname fun args k)
      (let ((free (sort < k.free))
	    (nregs (length free))
	    (target k.target)
            (lreturn (new-label "R")))
        (ARM
         (com "build continuation")
         ('alloc (s 0) (i TC_SAVE) (i (+ nregs 3)))
         ('stp k lenv (ind (s 0) (i 8)))
         ('mov k (s 0))
         ('adr (s 0) (l lreturn))
         ('str (s 0) (ind k (i 24)))
         (com "save free registers: [" (join int->string ", " free) "]")
         ;; could use a stp/ldp xform for these as well.
         ;; [or a peephole pass]
         @ (map-range j nregs (ARMINSN 'str (n (nth free j)) (ind k (i (* 8 (+ 4 j))))))
         @ (if (>= args 0)
               (ARM
                (com "call with args")
                ('ldr (s 0) (ind (n fun) (i 16)))
                ('str (s 0) (ind (n args) (i 8)))
                ('mov lenv (n args)))
               (ARM
                (com "call without args")
                ('ldr lenv (ind (n fun) (i 16)))))
         @ (funcall mname fun)
         (com "label for return")
         (label lreturn)
         (com "restore free registers: [" (join int->string ", " free) "]")
         @ (map-range j nregs (ARMINSN 'ldr (n (nth free j)) (ind k (i (* 8 (+ 4 j))))))
         (com "pop k")
         ('ldp k lenv (ind k (i 8))) ;; tricky!
         @ (if (not (= target -1))
               (ARM
                (com "target = Rreturn")
                ('mov (n target) retval))
               (ARM))
           )))

    (define (make-labels n)
      (let ((v (make-vector n "")))
	(for-range i n
	   (set! v[i] (new-label "NV")))
	v))

    ;; ((0 1 2) (3 4 5) (6 7 8)) => (0 1 2 3 4 5 6 7 8)
    (define flatten
      (one)     -> one
      (hd . tl) -> (append hd (flatten tl))
      ()        -> (list:nil)
      )

    ;; jump table version.
    ;; idea: datatype has 0..n indices.  some are imm, some are tup.
    ;; we call irk_get_user_tag, index into a table of labels for each
    ;; sub.  the default for otherwise empty entries in the table will
    ;; be 'ealt', otherwise an error?

    (define (format-jump-table labels tb-label)
      ;; .quad LNV2-TB0, LEA3-TB0, ...
      (format
       ".quad "
       (join (lambda (x) (format x "-" tb-label))
             ","
             (vector->list labels))))

    (define (emit-nvcase-table src dtname alts jump-num subs mealt k)
      ;; (printf "nvcase " (sym dtname) "\n")
      (match (alist/lookup the-context.datatypes dtname) with
	(maybe:no)
        -> (error1 "emit-nvcase" dtname)
	(maybe:yes dt)
	-> (let ((nalts (length alts))
                 (dtnalts (dt.get-nalts))
		 (labels (make-labels nalts))
                 (ealt-label (new-label "EA")) ;; default jump table entry.
                 (jump-table (make-vector dtnalts ealt-label))
                 (jt-label (new-label "JT"))
                 (tb-label (new-label "TB"))
                 (code (list:nil)))
	     (for-range i nalts
               (let ((altname (nth alts i))
                     (alt (dt.get altname))
                     (index alt.index))
                 (set! jump-table[index] labels[i])
                 (push! code (ARM (label labels[i]) @ (emit (nth subs i))))))
             (ARM
              (com "nvcase: " (sym dtname) " in " (int src) " nalts " (int nalts))
              ('mov (c 0) (n src))
              ((if (eq? dtname 'list) 'getltag 'getutag) (c 0))
              ('lld_addr (s 0) (l jt-label))
              ('adr (s 1) (l labels[0])) ;; label is local, thus adr is ok?
              ('ldr (s 0) (ind (s 0) (c 0) (lsl 3)))
              ('add (s 1) (s 1) (s 0))
              ('br (s 1))
              = (format ".p2align 3,0")
              (label jt-label)
              = (format-jump-table jump-table tb-label)
              (label tb-label)
              @ (flatten (reverse code))
              (label ealt-label)
              @ (if-maybe ealt mealt (emit ealt) (list:nil))
              @ (emit-jump-continuation jump-num k.insn)))
        ))

    (define (emit-nvcase src dtname alts jump-num subs mealt k)
      ;; (printf "nvcase " (sym dtname) " (" (join symbol->string " " alts) ") #subs=" (int (length subs)) "\n")
      (if (eq? dtname 'bool)
          (match alts mealt with
            ;; we handle bool specially. because it's a builtin type, get_user_tag won't work on
            ;; it.  but since there are exactly two alts, we can just translate it into a test insn.
            ('true 'false) (maybe:no) -> (emit-test src jump-num (nth subs 0) (nth subs 1) k)
            ('false 'true) (maybe:no) -> (emit-test src jump-num (nth subs 1) (nth subs 0) k)
            ('true) (maybe:yes ealt)  -> (emit-test src jump-num (nth subs 0) ealt k)
            ('false) (maybe:yes ealt) -> (emit-test src jump-num ealt (nth subs 0) k)
            _ _ -> (impossible))
          ;; 'normal' TC_USEROBJ datatypes use a jump table.
          (emit-nvcase-table src dtname alts jump-num subs mealt k)
          ))

    (define (label->tag label)
      (match (alist/lookup the-context.variant-labels label) with
        (maybe:yes tag) -> tag
        (maybe:no)      -> (error1 "variant constructor never called" label)
        ))

    (define (emit-pvcase src names arities jump-num alts mealt k)
      ;; pvcase can't be done with a jump table, because the values are
      ;; non-continuous, sparse.  (i.e., not 0,1,2... but 34, 19)
      (let ((ntags (length names))
            (labels (make-labels ntags))
            (tags (map label->tag names)))
        (ARM
         (com "pvcase: names = (" (join symbol->string ", " names) ") #alts = " (int (length alts)))
         ('mov (s 0) (n src))
         ('getutag (s 0))
         @ (flatten
            (map-range
                i ntags
                (ARM ('cmp (s 0) (i (nth tags i)))
                     ('beq (l labels[i])))))
         @ (if-maybe ealt mealt (emit ealt) (list:nil))
         @ (flatten
            (map-range
                i ntags
                (ARM (label labels[i])
                     @ (emit (nth alts i)))))
         @ (emit-jump-continuation jump-num k.insn))
        ))

    (define (emit-copy-lit index target)
      (ARM
       ('ldr_addr (s 0) (l (format "lit_" (int index))))
       ('bl (l "arm_copy_tuple_to"))
       ('mov (n target) retval)))

    (define (emit-litcon index kind target free)
      (if (>= target 0)
          (match kind with
            'string -> (ARM ('lld_addr (n target) (l (format "lit_" (int index)))))
            'symbol -> (ARM ('lld_addr (n target) (l (format "lit_" (int index)))))
            'record -> (emit-copy-lit index target)
            'vector -> (emit-copy-lit index target)
            _       -> (ARM ('ldr_addr (n target) (l (format "lit_" (int index))))))
          (list:nil)))

    (define (emit-ffi-call name rtype argtypes args target free)
      (let ((sig (arrow rtype argtypes)))
        (emit-c-call sig (symbol->string name) args target free #t)))

    ;; NOTE this is specific to macos' treatment of variadic funs.
    ;; will need to be tweaked for other platforms.
    (define (emit-variadic-ffi name nfixed rtype argtypes args target free)
      (let ((nvar (- (length argtypes) nfixed))
            ;; round up stack adjustment to 16 bytes.
            (nstack (* 8 (if (odd? nvar) (+ nvar 1) nvar)))
            (sig (arrow rtype argtypes))
            ((saves restores) (save-restore free))
            (varcode (list:nil)))
        ;; tasks:
        ;; 1) compute how much 16-bit-aligned extra stack we need.
        ;; 2) sub sp, sp, #48 (e.g.)
        ;; 3) convert each vararg and push it onto the stack
        ;; 4) convert each fixed arg and put it in its register
        ;; 5) make the call
        ;; 6) add sp, sp, #48
        (for-range i nvar
          ;; we stage each vararg into x0, convert it, and then push onto the stack.
          (push!
           varcode
           (ARM
            ('mov (c 0) (n (nth args (+ nfixed i))))
            @ (frob-arg 0 (nth argtypes (+ nfixed i)))
            ('str (c 0) (ind sp (i (* i 8)))))))
        (ARM
         (com "variadic ffi:  nfixed " (int nfixed) " nvar " (int nvar) " stack adjust " (int nstack))
         ;; the save area goes above the stacked varargs, which must be at sp.
         @ saves
         ('sub sp sp (i nstack))
         @ (flatten (reverse varcode))
         ;; put fixed args into the proper registers
         @ (load-c-args sig (take args nfixed) #t)
         ('bl (g (symbol->string name)))
         @ (if (= target -1)
               (list:nil)
               (ARM
                @ (frob-output sig)
                  ('mov (n target) retval)))
         ('add sp sp (i nstack))
         @ restores
         )))

    (define (emit-ffi csig rtype argtypes name args target free)
      ;; rtype/argtypes are predicates, not s-expressions.
      ;; csig is a parsed ctype, and indicates the signature from the FFI data.
      ;; these should be 'identical' unless varargs.  so we really do need both.
      ;;(printf "emit-ffi " (sym name) " csig: " (csig-repr csig) " argtypes: (" (join type-repr " " argtypes) ")\n")
      (match csig with
        (csig:fun name frtype fargtypes #t) ;; varargs
        -> (if (eq? the-context.options.platform 'Darwin)
               ;; macos uses its own custom version of variadic args.
               (emit-variadic-ffi name (length fargtypes) rtype argtypes args target free)
               ;; other platforms will follow the standard ABI?
               (emit-ffi-call name rtype argtypes args target free))
        (csig:fun name frtype fargtypes #f)
        -> (emit-ffi-call name rtype argtypes args target free)
        (csig:obj name obtype)
        -> (begin
             (warning "ffi object reference NYI")
             (ARM (com "object reference here")))
        ))

    (define (emit insn)
      (match insn with
        ;; similar to testcexp, avoids silly round trip through boolean object.
        (insn:primop '%llicmp (sexp:symbol cc _) _ (arg0 arg1) {target=_ free=_ insn=(insn:test reg jn k0 k1 k)})
        -> (emit-test-cmp cc arg0 arg1 reg jn k0 k1 k)

        (insn:literal lit k)                         -> (emitk (emit-literal lit k.target) k)
        (insn:return target)                         -> (emit-return target)
        (insn:primop name params type args k)        -> (emitk (emit-primop name params type args k) k)
        (insn:test reg jn k0 k1 k)                   -> (emit-test reg jn k0 k1 k)
        (insn:label jn next)                         -> (emit-label jn next)
        (insn:jump reg target jn free)               -> (emit-jump reg target jn free.val)
        (insn:move dst var k)                        -> (emitk (emit-move dst var k.target) k)
        (insn:alloc tag size k)                      -> (emitk (emit-alloc tag size k.target) k)
        (insn:store off arg tup i k)                 -> (emitk (emit-store off arg tup i) k)
        (insn:close name nreg body k)                -> (emitk (emit-close name nreg body k.target) k)
        (insn:varref d i k)                          -> (emitk (emit-varref d i k.target) k)
        (insn:varset d i v k)                        -> (emitk (emit-varset d i v k.target) k)
        (insn:new-env size top? types k)             -> (emitk (emit-new-env size top? types k.target) k)
        (insn:push r k)                              -> (emitk (emit-push r) k)
        (insn:pop r k)                               -> (emitk (emit-pop r k.target) k)
        (insn:tail name fun args)                    -> (emit-tail name fun args)
        (insn:trcall d n args)                       -> (emit-trcall d n args)
        (insn:fatbar lab jn k0 k1 k)                 -> (emit-fatbar lab jn k0 k1 k)
        (insn:fail label npop free)                  -> (emit-fail label npop free.val)
        (insn:invoke name fun args k)                -> (emitk (emit-call name fun args k) k)
        (insn:nvcase tr dt tags jn alts ealt k)      -> (emit-nvcase tr dt tags jn alts ealt k)
        (insn:pvcase tr tags arities jn alts ealt k) -> (emit-pvcase tr tags arities jn alts ealt k)
        (insn:litcon i kind k)                       -> (emitk (emit-litcon i kind k.target k.free) k)
        (insn:ffi sig type ats name args k)          -> (emitk (emit-ffi sig type ats name args k.target k.free) k)

         _ -> (begin
                (print-insn insn)
                (raise (:Arm/CPSNotSupported)))
        ))

    ;; used by %divmod, must be present.
    (remember-variant-label 'tuple)

    (emit cps)
    ))

;; --- last-minute optimizations ---

(define (arm-normalize-regs armgen)

  ;; convert all registers to 'x' representation so the peephole
  ;; optimizers can easily compare register names.

  (define N
    (armop:k)        -> (armop:x (register->num Rk))
    (armop:lenv)     -> (armop:x (register->num Rlenv))
    (armop:freep)    -> (armop:x (register->num Rfreep))
    (armop:retval)   -> (armop:x (register->num Rreturn))
    (armop:s 0)      -> (armop:x (register->num Rscratch))
    (armop:s 1)      -> (armop:x (register->num Rscratch2))
    (armop:s 2)      -> (armop:x (register->num Rscratch3))
    (armop:n n)      -> (armop:x (register->num n))
    (armop:ind args) -> (armop:ind (map N args))
    other            -> other
    )

  (makegen emit
    (for item armgen
      (match item with
        (arm:insn rator rands) -> (emit (arm:insn rator (map N rands)))
        other                  -> (emit other)
        ))))

(define (arm-peep armgen)

  (makegen emit
    (let loop ((ma (armgen))
               (mb (armgen)))
      (match ma mb with
        (maybe:yes a) (maybe:yes b)
        -> (match a b with
             (arm:insn 'str ((armop:x src0) (armop:ind ((armop:x dst0) (armop:i off0)))))
             (arm:insn 'str ((armop:x src1) (armop:ind ((armop:x dst1) (armop:i off1)))))
             -> (if (and (= dst0 dst1) (= off0 (- off1 8)))
                    (begin
                      (emit (ARMINSN 'stp (x src0) (x src1) (ind (x dst0) (i off0))))
                      (loop (armgen) (armgen)))
                    (begin
                      (emit a)
                      (loop mb (armgen))))
             (arm:insn 'ldr ((armop:x src0) (armop:ind ((armop:x dst0) (armop:i off0)))))
             (arm:insn 'ldr ((armop:x src1) (armop:ind ((armop:x dst1) (armop:i off1)))))
             -> (if (and (= dst0 dst1) (= off0 (- off1 8)) (not (= src0 dst0))) ;; note extra condition!
                    (begin
                      (emit (ARMINSN 'ldp (x src0) (x src1) (ind (x dst0) (i off0))))
                      (loop (armgen) (armgen)))
                    (begin
                      (emit a)
                      (loop mb (armgen))))
             _ _ -> (begin
                      (emit a)
                      (loop mb (armgen))))
        (maybe:yes a) (maybe:no)
        -> (emit a)
        _ _
        -> #u
        )))
  )

(define (emit-arm-insn o insn)
  (match insn with
    (arm:label name)
    -> (begin (o.dedent) (o.write (format name ":")) (o.indent))
    (arm:global name)
    -> (if the-context.options.asm-underscores
           (begin (o.write (format ".global _" name)) (o.dedent) (oformat "_" name ":") (o.indent))
           (begin (o.write (format ".global " name)) (o.dedent) (oformat name ":") (o.indent)))
    _ -> (o.write (format-arm insn))
    ))

(define (emit-arm-insns o insns)
  (for-list insn insns
    (emit-arm-insn o insn)))

(define (emit-arm-gen o gen)
  (for insn gen
    (emit-arm-insn o insn)))

;; --- file output ---

(define (emit-arm o cname arm)
  (verbose (printf "emit-arm...\n") (flush))
  (emit-arm-insns
   o
   (ARM
    (com "-*- Mode: asm -*-")
    = ""
    = (format
       "\t.include \""
       (match the-context.options.platform with
         'Darwin   -> "arm-mach-preamble.s"
         otherwise -> "arm-elf-preamble.s")
       "\"")
    = "\t.data"
    = "\t.p2align 3, 0x0"
    ))
  (verbose (printf "emit constructed literals...\n") (flush))
  (asm-emit-constructed o)
  (verbose (printf "emit lookup-field hashtables...\n"))
  (emit-asm-lookup-field-hashtables o)
  (o.indent)
  (emit-arm-insns
   o
   (ARM
    = ".text"
    (com "arm64 output for " (sym the-context.options.platform))
    (global "toplevel")
    ;; NB: currently no way in the DSL to do the ! version.
    = "stp x25, x26, [sp, -16]!"
    = "stp x27, x28, [sp, -16]!"
    ('ldr_addr (x 26) (g "freep"))
    ('ldr_addr (x 27) (g "lenv"))
    ('ldr_addr (x 28) (g "k"))
    ('lld_addr (x 25) (l "Lreturn"))
    ('str (x 25) (ind (x 28) (i 24)))
    ))
  (let ((gen0 (list-generator arm))
        (gen1 (if the-context.options.no-peephole gen0 (arm-peep (arm-normalize-regs gen0)))))
    (emit-arm-gen o gen1))
  (emit-arm-insns
   o
   (ARM
    (label "Lreturn")
    = "ldp x27, x28, [sp], 16"
    = "ldp x25, x26, [sp], 16"
    ('b (g "exit_continuation"))
    (global "irk_get_metadata")
    ('ldr_addr (x 0) (l (format "lit_" (int (- the-context.literals.count 1)))))
    ('ret)
    ))
  (o.dedent)
  (o.close)
  )

(define (compile-to-arm base cps)
  (let ((arm (cps->arm cps)))
    ;; do stuff with armcps
    (let ((armpath (format base ".s"))
          (arm-file (file/open-write armpath #t #o644))
          (oarm (make-writer arm-file)))
      (oarm.set-indent "\t")
      (notquiet (printf "\n-- ARM output --\n : " armpath "\n"))
      (emit-arm oarm "toplevel" arm)
      (notquiet (printf "wrote " (int (oarm.get-total)) " bytes to " armpath ".\n"))
      (let (((path0 ignore-file) (find-file the-context.options.include-dirs "include/header1.c")))
        (file/close ignore-file)
        (list armpath path0)))))
