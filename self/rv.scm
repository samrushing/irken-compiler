;; -*- Mode: Irken -*-

(require "self/backend.scm")
(require "self/asmlits.scm")

;; register usage on rv64:
;;
;;                                                    caller  callee
;; register    ABI name   Description                 saved   saved
;; x0          zero       always zero
;; x1          ra         return address                X
;; x2          sp         stack pointer                         X
;; x3          gp         global pointer
;; x4          tp         thread pointer
;; x5-7        t0-2       temporaries                   X
;; x8          s0/fp      saved reg / frame pointer             X
;; x9          s1         saved reg                             X
;; x10-11      a0-1       arguments/return              X
;; x12-17      a2-7       arguments                     X
;; x18-27      s2-11      saved                                 X
;; x28-31      t3-6       temporaries                   X

(define RV/register-names
  #('zero 'ra 'sp 'gp 'tp
    't0 't1 't2
    'fp 's1
    'a0 'a1
    'a2 'a3 'a4 'a5 'a6 'a7
    's2 's3 's4 's5 's6 's7 's8 's9 's10 's11
    't3 't4 't5 't6
    ))

(define (RV/register->name n)
  RV/register-names[n]
  )

;; this is the mapping from cps registers to rv registers. we (currently) need 20.
(define RV/register-file
  #(24 23 22 21 20 19 18 ;; s8-s2: these are callee-saved, safe to use
    9 ;; s1
    8 ;; s0/fp should/can?
    ;; start using caller-saved registers, backwards,
    ;; in order to preserve the ability to call C funs.
    31 30 29 28 ;; t6-t3
    ;; digging into args here...
    17 16 15 14 13 12 11 ;; a7-a1
    ))

(define RV/carg-registers
  #(10 11 12 13 14 15 16 17))

(assert (= (vector-length RV/register-file) *max-bytecode-registers*))

;; runtime register assignments
;; let's not do this with these globals.
;;                         -currently-
(define RV/k        27) ;; x27 s11
(define RV/lenv     26) ;; x26 s10
(define RV/freep    25) ;; x25 s9
(define RV/return   10) ;; x10 a0
(define RV/scratch  5)  ;; x5 t0
(define RV/scratch2 6)  ;; x6 t1
(define RV/scratch3 7)  ;; x7 t2

(datatype rv
  (:comment string)           ;; for inline comments
  (:label string)             ;; local label
  (:global string)            ;; global label
  (:insn symbol (list rvop))  ;; instruction
  (:verb string)              ;; verbatim (e.g. ".quad 1234")
  )

(define (format-rv insn)
  (match insn with
    (rv:comment s)          -> (format "# " s)
    (rv:insn op ())         -> (format (sym op)) ;; e.g. 'ret'
    (rv:insn op args)       -> (format (sym op) " " (join format-rvop ", " args))
    (rv:label name)         -> (format name ":")
    (rv:global name)        -> (format ".global " name "\n" name ":")
    (rv:verb s)             -> (format s)
    ))

;; inline assembly.

;; operands for rv64 insns.

(datatype rvop
  (:trg)      ;; target
  (:k)        ;;
  (:lenv)     ;;
  (:freep)    ;;
  (:retval)   ;;
  (:zero)     ;; zero register
  (:sp)       ;; stack pointer
  (:n int)    ;; register by 'irken number'
  (:r symbol) ;; register by name
  (:s int)    ;; scratch by number
  (:a int)    ;; irken argument
  (:i int)    ;; immediate
  (:c int)    ;; C argument
  (:l string) ;; label
  (:ind int rvop) ;; 40(sp)
  )

(define parse-rvop
  (sexp:symbol 'trg _)                                     -> (rvop:trg)
  (sexp:symbol 'k _)                                       -> (rvop:k)
  (sexp:symbol 'lenv _)                                    -> (rvop:lenv)
  (sexp:symbol 'freep _)                                   -> (rvop:freep)
  (sexp:symbol 'retval _)                                  -> (rvop:retval)
  (sexp:symbol 'zero _)                                    -> (rvop:zero)
  (sexp:symbol 'sp _)                                      -> (rvop:sp)
  (sexp:string label)                                      -> (rvop:l label)
  (sexp:list ((sexp:symbol 'r _) (sexp:symbol name _)))    -> (rvop:r name)
  (sexp:list ((sexp:symbol 'r _) (sexp:string name)))      -> (rvop:r (string->symbol name))
  (sexp:list ((sexp:symbol 's _) (sexp:int num)))          -> (rvop:s num)
  (sexp:list ((sexp:symbol 'a _) (sexp:int num)))          -> (rvop:a num)
  (sexp:list ((sexp:symbol 'i _) (sexp:int num)))          -> (rvop:i num)
  (sexp:list ((sexp:symbol 'c _) (sexp:int num)))          -> (rvop:c num)
  (sexp:list ((sexp:symbol 'ind _) (sexp:int off) arg))    -> (rvop:ind off (parse-rvop arg))
  ;; we leave out (rvop:n) on purpose, since that information shouldn't
  ;;  be user-observable.
  x -> (raise (:RVAsm/BadExpression x))
  )

(define parse-rv-inline*
  (sexp:list ((sexp:symbol 'com _) (sexp:string comment)))
  -> (rv:comment comment)
  (sexp:list ((sexp:symbol 'label _) (sexp:string name)))
  -> (rv:label name)
  (sexp:list ((sexp:symbol rator _) . rands))
  -> (rv:insn rator (map parse-rvop rands))
  x
  -> (raise (:RVAsm/BadExpression x))
  )

(define (print-inline-error what exp sub)
  (printf "error in inline asm expression: " (string what) "\n")
  (printf (src-repr (guess-location exp) the-context.options.src-limit) "\n")
  (printf (repr exp) "\n")
  (printf (repr sub) "\n")
  (error "error in inline asm")
  )

(define (parse-rv-inline exp)
  (try
   (parse-rv-inline* exp)
   except
   (:RVAsm/BadExpression x)
   -> (print-inline-error "bad expression" exp x)
   ))

(define format-rvop
  (rvop:r name)      -> (format (sym name))
  (rvop:k)           -> (format (sym (RV/register->name RV/k)))
  (rvop:lenv)        -> (format (sym (RV/register->name RV/lenv)))
  (rvop:freep)       -> (format (sym (RV/register->name RV/freep)))
  (rvop:retval)      -> (format (sym (RV/register->name RV/return)))
  (rvop:zero)        -> "zero"
  (rvop:sp)          -> "sp"
  (rvop:s 0)         -> (format (sym (RV/register->name RV/scratch)))
  (rvop:s 1)         -> (format (sym (RV/register->name RV/scratch2)))
  (rvop:s 2)         -> (format (sym (RV/register->name RV/scratch3)))
  (rvop:r name)      -> (format (sym name))
  (rvop:n n)         -> (format (sym (RV/register->name RV/register-file[n])))
  (rvop:i n)         -> (format (int n))
  (rvop:c n)         -> (format (sym (RV/register->name RV/carg-registers[n])))
  (rvop:l name)      -> (format name)
  (rvop:ind off arg) -> (format (int off) "(" (format-rvop arg) ")")
  x                  -> (raise (:RVAsm/BadOperand x))
  )

;; DSL for asm in the code below.
;; (intent is to be identical to sexp version)

;; one rvop
(defmacro RVOP
  (RVOP <k>)            -> (rvop:k)
  (RVOP <lenv>)         -> (rvop:lenv)
  (RVOP <freep>)        -> (rvop:freep)
  (RVOP <retval>)       -> (rvop:retval)
  (RVOP <zero>)         -> (rvop:zero)
  (RVOP <sp>)           -> (rvop:sp)
  (RVOP (<r> name))     -> (rvop:r name)
  ;;(RVOP (<a> n)) -> (rvop:a n)  ;; NO! these are for inline asm only
  ;;(RVOP <trg>    -> (rvop:a n)  ;; NO!
  (RVOP (<s> n))        -> (rvop:s n)
  (RVOP (<n> n))        -> (rvop:n n)
  (RVOP (<i> n))        -> (rvop:i n)
  (RVOP (<c> n))        -> (rvop:c n)
  (RVOP (<l> l))        -> (rvop:l l)
  (RVOP (<ind> off op)) -> (rvop:ind off (RVOP op))
  )

;; list of rvops
(defmacro RVOPS
  (RVOPS)            -> (list:nil)
  (RVOPS op ops ...) -> (list:cons (RVOP op) (RVOPS ops ...))
  )

;; one rvinsn
(defmacro RVINSN
  ;; could do format here ...
  (RVINSN <com> s ...) -> (rv:comment (format s ...))
  (RVINSN <label> l)   -> (rv:label l)
  (RVINSN <global> l)  -> (rv:global l)
  (RVINSN op args ...) -> (rv:insn op (RVOPS args ...))
  )

;; list of rvinsns
(defmacro RV
  (RV)                        -> (list:nil)
  ;; verbatim insert
  (RV <=> x insns ...)        -> (list:cons (rv:verb x) (RV insns ...))
  ;; '@' = splice in place (similar to UNQUOTE-SPLICING in Scheme)
  (RV <@> sub)                -> sub
  (RV <@> sub insns ...)      -> (append2 sub (RV insns ...))
  (RV (insn ...) insns ...)  -> (list:cons (RVINSN insn ...) (RV insns ...))
  )

(define (cps->rv cps)

  (let ((used-jumps (find-jumps cps))
        (ltorg-flag #f))

    (define new-label
      (let ((counter 0))
        (lambda (prefix)
          (inc! counter)
          (format ".L" prefix (int counter)))))

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
      (RV
       ('mv retval (n reg))
       ('kreturn)))

    (define (emit-immediate imm trg)
      ;; as with arm, immediates are tricky on riscv.
      ;; we will start by assuming 'li just works. (it won't.)
      (RV
       ('li (n trg) (i imm))
       ))

    (define (emit-literal lit trg)
      (if (= trg -1)
          (list:nil) ;; dead literal
          (if (wide-literal? lit)
              (RV ('li (n trg) (l (encode-immediate-hex lit))))
              (emit-immediate (encode-immediate lit) trg))))

    ;; llvm -> rv64 arithmetic insns
    (define llop->rvop
      'shl  -> 'sll
      'ashr -> 'sra  ;; arithmetic (srl is logical: wrong for negatives)
      'sdiv -> 'div
      other -> other
      )

    (define emit-arith1
      'srem arg0 arg1 target
      -> (RV ('rem (n target) (n arg0) (n arg1))) ;; (signed remainder)
      op arg0 arg1 target
      -> (RV ((llop->rvop op) (n target) (n arg0) (n arg1)))
      )

    (define (emit-arith op arg0 arg1 target)
      ;; XXX think about ways to avoid untag/tag, depending on <op>
      ;; e.g. (2a+1)+(2b+1) == 2a + 2b + 2 == 2(a+b)+2 so leave tagged and sub1 from result.
      (RV
       ('untag (n arg0))
       ('untag (n arg1))
       @ (emit-arith1 op arg0 arg1 target)
       ('tag (n target))
       ))

    (define (emit-divmod arg0 arg1 target)
      ;; we need the tag for `(:tuple x y)`.
      (let ((tuple-tag (alist/get the-context.variant-labels 'tuple "no :tuple polyvar?")))
        (RV
         ('untag (n arg0))
         ('untag (n arg1))
         ('div (s 1) (n arg0) (n arg1)) ;; signed (divu: nonsense for negatives)
         ('mul (s 2) (s 1) (n arg1))
         ('sub (s 2) (n arg0) (s 2))
         ('alloc (n target) (i (UOTAG tuple-tag)) (i 2))
         ('tag (s 1))
         ('sd (s 1) (ind 8 (n target)))
         ('tag (s 2))
         ('sd (s 2) (ind 16 (n target)))
         )))

    ;; future: check out the Zicond extension.
    ;; XXX what about slt/slti/sltu/sltiu - these are apparently meant for carry
    ;;     calculations but could they not make a conditional select of sorts?

    (define cc->rvbcc
      ;; translate between llvm CC and rv BCC
      'eq  -> 'beq
      'slt -> 'blt
      'sle -> 'ble ;; pseudo
      'sgt -> 'bgt ;; pseudo
      'sge -> 'bge
      'ult -> 'bltu
      'ule -> 'bleu ;; pseudo
      'ugt -> 'bgtu ;; pseudo
      'uge -> 'bgeu
      cc -> (raise (:UnknownConditionCode cc))
      )

    (define (emit-icmp cc arg0 arg1 trg)
      (let ((true-label (new-label "L"))
            (after-label (new-label "L")))
        (RV
         ((cc->rvbcc cc) (n arg0) (n arg1) (l true-label))
         ('li (n trg) (i 6)) ;; bool #f
         ('j (l after-label))
         (label true-label)
         ('lui (n trg) (i 16))
         ('addiw (n trg) (n trg) (i 6)) ;; bool #t
         (label after-label))))

    (define (emit-test-cmp cc arg0 arg1 reg jn k0 k1 k)
      (let ((skip-label (new-label "L"))
            (jcont (emit-jump-continuation jn k.insn)))
        (RV
         (com "test-cmp")
         ((cc->rvbcc cc) (n arg0) (n arg1) (l skip-label))
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
                 (RV
                  (com "dtcon " (sym dtname) "." (sym altname))
                  ('alloc (s 0) (i (get-uotag dtname altname alt.index)) (i nargs))
                  @ (map-range j nargs (RVINSN 'sd (n (nth args j)) (ind (* 8 (+ j 1)) (s 0))))
                  ('mv (n target) (s 0)))))))

    (define (emit-nvget target reg index)
      (RV
       (com "nvget " (int target) " = r" (int reg) "[" (int index) "]")
       ('ld (n target) (ind (* 8 (+ index 1)) (n reg)))
       ))

    (define (ambig code)
      (tree/insert! the-context.ambig-rec int-cmp code #u))

    (define (emit-record-get label sig rec trg free)
      (let ((label-code (lookup-label-code label)))
        (match (guess-record-type sig) with
          (maybe:yes sig0)
          -> (RV
              (com "%record-get " (int rec) "." (sym label))
              ('ld (n trg) (ind (* 8 (+ 1 (index-eq label sig0))) (n rec))))
          (maybe:no)
          -> (begin
               (ambig label-code)
               (RV
                (com "ambig %record-get " (sym label) " code " (int label-code))
                @ (rv-helper "record_fetch" (list (rvop:n rec) (rvop:i label-code)) free)
                ('mv (n trg) (c 0))
                ))
          )))

    (define (emit-record-set label sig rec val free)
      (let ((label-code (lookup-label-code label)))
        (match (guess-record-type sig) with
          (maybe:yes sig0)
          -> (RV
              (com "%record-set " (int rec) "." (sym label))
              ('sd (n val) (ind (* 8 (+ 1 (index-eq label sig0))) (n rec))))
          (maybe:no)
          -> (begin
               (ambig label-code)
               (RV
                (com "ambig %record-set " (sym label) " code " (int label-code))
                @ (rv-helper "record_store" (list (rvop:n rec) (rvop:i label-code) (rvop:n val)) free)
                ))
          )))

    (define (frob-arg i type)
      (match type with
        (type:tvar id _) -> (list:nil)
        (type:pred name predargs _)
        -> (match name with
             'int          -> (RV ('untag (c i)))
             'bool         -> (RV ('unbool (c i)))
             'string       -> (RV ('unstring (c i)))
             'cref         -> (RV ('unforeign (c i)))
             'char         -> (list:nil) ;; seems wrong.
             'arrow        -> (list:nil)
             'vector       -> (list:nil) ;; should be raw pointer?
             'symbol       -> (list:nil)
             'continuation -> (list:nil)
             'raw          -> (list:nil)
             kind          -> (if (member-eq? kind c-int-types)
                                  (RV ('untag (c i)))
                                  (error1 "rv frob-arg:" (type-repr type)))
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
        ;; RV64 keeps 32-bit values sign-extended in registers, even unsigned
        ;;  ones: zero-extend those.
        (type:pred 'uint _ _)    -> (RV ('slli (c 0) (c 0) (i 32)) ('srli (c 0) (c 0) (i 32)) ('tag (c 0)))
        (type:pred 'u32 _ _)     -> (RV ('slli (c 0) (c 0) (i 32)) ('srli (c 0) (c 0) (i 32)) ('tag (c 0)))
        (type:pred 'cref _ _)    -> (RV ('mv (s 1) (c 0)) ('make_foreign (c 0) (s 1)))
        (type:pred '* _ _)       -> (RV ('mv (s 1) (c 0)) ('make_foreign (c 0) (s 1)))
        (type:pred 'void _ _)    -> (RV ('li (c 0) (i TC_UNDEFINED)))
        (type:pred kind _ _)     -> (if (member-eq? kind c-int-types)
                                        (RV ('tag (c 0)))
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

    ;; --- calling C ---
    ;;
    ;; a0-a7 carry C arguments, but a1-a7 are also irken registers 19-13
    ;;  (see RV/register-file), and nothing preserves t3-t6/a1-a7 across a
    ;;  call.  so, for every call into C (FFI or runtime helper):
    ;;  1) live irken registers that C may clobber are saved on the stack
    ;;     first, and restored after;
    ;;  2) if loading the args in order would overwrite one before it's read
    ;;     (arg j sits in a register that arg i < j is loaded into), they go
    ;;     through the stack: each is converted in a0 and stored, then
    ;;     a0..a7 are loaded from there;
    ;;  3) args past the eighth go on the stack, in 8-byte slots at sp.

    (define rv-caller-saved?
      ;; t0-t2, a0-a7, t3-t6 (and ra, which irken doesn't allocate)
      (lambda (mreg)
        (or (= mreg 1)
            (and (>= mreg 5) (<= mreg 7))
            (and (>= mreg 10) (<= mreg 17))
            (>= mreg 28))))

    (define caller-saves-set
      (let ((r '()))
        (for-range i (vector-length RV/register-file)
          (when (rv-caller-saved? RV/register-file[i])
            (push! r i)))
        r))

    ;; -> (:tuple saves restores), keeping sp 16-byte aligned.
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
               (RV
                (com "save caller-saved: [" (join int->string ", " regs) "]")
                ('addi sp sp (i (- 0 nbytes)))
                @ (map-range i nregs (RVINSN 'sd (n (nth regs i)) (ind (* 8 i) sp))))
               (RV
                @ (map-range i nregs (RVINSN 'ld (n (nth regs i)) (ind (* 8 i) sp)))
                ('addi sp sp (i nbytes))))))))

    ;; the machine register an operand reads, or -1.
    (define rvop->mreg
      (rvop:n r) -> RV/register-file[r]
      _          -> -1)

    ;; would loading these operands into a0, a1, ... in order clobber one
    ;;  before it's read?
    (define (load-hazard? ops)
      (let loop ((i 0) (ops ops) (written '()))
        (match ops with
          () -> #f
          (op . rest)
          -> (if (member-eq? (rvop->mreg op) written)
                 #t
                 (loop (+ i 1) rest (list:cons RV/carg-registers[i] written))))))

    ;; load an operand (register, scratch, or immediate) into C arg <i>.
    (define (load-carg i op)
      (match op with
        (rvop:i k) -> (RV ('li (c i) (i k)))
        _          -> (list (rv:insn 'mv (list (rvop:c i) op)))))

    ;; the C type of argument <i>, if the signature says.
    (define (sig-arg-type sig i)
      (match sig with
        (type:pred 'arrow (result-type . arg-types) _)
        -> (if (< i (length arg-types)) (maybe:yes (nth arg-types i)) (maybe:no))
        _ -> (maybe:no)))

    (define (frob-arg-maybe i mtype)
      (match mtype with
        (maybe:yes t) -> (frob-arg i t)
        (maybe:no)    -> (list:nil)))

    ;; put up to eight operands into a0..a7, converting (irken->c) if
    ;;  convert?.  conversions only use t0-t2, never another arg register.
    (define (load-c-args sig ops convert?)
      (let ((nargs (min 8 (length ops)))
            (ops (take ops nargs)))
        (if (not (load-hazard? ops))
            (RV
             @ (flatten (map-range i nargs (load-carg i (nth ops i))))
             @ (if convert?
                   (flatten (map-range i nargs (frob-arg-maybe i (sig-arg-type sig i))))
                   (list:nil)))
            (let ((nbytes (* 16 (/ (+ nargs 1) 2))))
              (RV
               (com "args through the stack")
               ('addi sp sp (i (- 0 nbytes)))
               @ (flatten
                  (map-range
                   i nargs
                   (RV
                    @ (load-carg 0 (nth ops i))
                    @ (if convert? (frob-arg-maybe 0 (sig-arg-type sig i)) (list:nil))
                    ('sd (c 0) (ind (* 8 i) sp)))))
               @ (map-range i nargs (RVINSN 'ld (c i) (ind (* 8 i) sp)))
               ('addi sp sp (i nbytes)))))))

    ;; args 8.. go on the stack, converted on the way through a0 (which is
    ;;  loaded afterwards).  -> (:tuple code nbytes)
    (define (store-stack-args sig ops convert?)
      (let ((nargs (length ops))
            (nstack (- nargs 8))
            (nbytes (* 16 (/ (+ nstack 1) 2))))
        (:tuple
         (RV
          ('addi sp sp (i (- 0 nbytes)))
          @ (flatten
             (map-range
              j nstack
              (let ((i (+ j 8)))
                (RV
                 @ (load-carg 0 (nth ops i))
                 @ (if convert? (frob-arg-maybe 0 (sig-arg-type sig i)) (list:nil))
                 ('sd (c 0) (ind (* 8 j) sp)))))))
         nbytes)))

    ;; call C function <name> with <ops>; the result is left in a0.
    ;;  <free> are the registers live after the call.
    (define (rv-call sig name ops free convert?)
      (let ((nargs (length ops))
            ((saves restores) (save-restore free))
            ((stack-code stack-bytes)
             (if (> nargs 8)
                 (store-stack-args sig ops convert?)
                 (:tuple (list:nil) 0))))
        (RV
         @ saves
         @ stack-code
         @ (load-c-args sig ops convert?)
         ('call (l name))
         @ (if (> stack-bytes 0) (RV ('addi sp sp (i stack-bytes))) (list:nil))
         @ restores)))

    ;; a runtime helper: no conversions.
    (define (rv-helper name ops free)
      (rv-call (pred 'raw '()) name ops free #f))

    (define (emit-c-call sig name args target free convert?)
      (RV
       @ (rv-call sig name (map (lambda (r) (rvop:n r)) args) free convert?)
       @ (if (= target -1)
             (RV)
             (RV
              @ (if convert? (frob-output sig) (list:nil))
                ('mv (n target) retval)))))

    (define (emit-rv-inline inline args target)

      (let ((inline-label (new-label "I")))

        (define (llabel name)
          (if (starts-with name "@")
              (format inline-label "." (substring name 1 (string-length name)))
              name))

        (define frob-rvop
          ;; convert these into irken register number
          (rvop:trg)         -> (rvop:n target)
          (rvop:a n)         -> (rvop:n (nth args n))
          (rvop:ind off arg) -> (rvop:ind off (frob-rvop arg))
          (rvop:l name)      -> (rvop:l (llabel name))
          x                  -> x
          )

        (define frob-rvinsn
          (rv:insn name args) -> (rv:insn name (map frob-rvop args))
          (rv:label name)     -> (rv:label (llabel name))
          other                -> other
          )

        (let ((parsed (map parse-rv-inline inline)))
          (list:cons
           (rv:comment "rv inline")
           (map frob-rvinsn parsed))
          )))

    (define (emit-array-ref vec index target)
      (RV
       ('untag (n index))
       ('rangecheck (n vec) (n index))
       ('slli (s 0) (n index) (i 3))
       ('add (s 0) (n vec) (s 0))
       ('ld (n target) (ind 8 (s 0)))))

    (define (emit-array-set vec index val)
      (RV
       ('untag (n index))
       ('rangecheck (n vec) (n index))
       ('slli (s 0) (n index) (i 3))
       ('add (s 0) (n vec) (s 0))
       ('sd (n val) (ind 8 (s 0)))))

    (define (emit-exit arg)
      (RV
       ('mv retval (n arg))
       ('tail (l "Lreturn"))
       ))

    (define (emit-putcc rk rv target)
      (RV
       (com "putcc")
       ('mv k (n rk))
       ('mv (n target) (n rv))))

    (define (emit-getcc target)
      (RV
       (com "getcc")
       ('mv (n target) k)))

    (define (emit-ensure-heap size free trg)
      (let ((L0 (new-label "EH"))
            (nfree (length free)))
        (RV
         (com "%ensure-heap: size in " (int size) " nfree = " (int nfree))
         ('ldr_addr (s 0) (l "limit"))
         ('slli (s 1) (n size) (i 2)) ;; N.B. hidden 'untag', nearly.
         ('sub (s 0) (s 0) (s 1))
         ('blt freep (s 0) (l L0))
         ;; need to gc.  say how much (in words), so the heap can grow to fit.
         ('srai (s 1) (n size) (i 1))
         ('str_addr (s 1) (l "heap_request"))
         ;; add free vars as roots
         ('ldr_addr (s 0) (l "heap1"))
         @ (map-range i nfree (RVINSN 'sd (n (nth free i)) (ind (* 8 (+ i 3)) (s 0))))
         ('li (c 0) (i nfree))
         ;; invoke GC
         ('call (l "irk_gc_for_ensure_heap"))
         ;; restore free vars
         ('ldr_addr (s 0) (l "heap0"))
         @ (map-range i nfree (RVINSN 'ld (n (nth free i)) (ind (* 8 (+ i 3)) (s 0))))
         (label L0)
         @ (dead-set trg)
         )))

    ;; %string->cref makes TC_FOREIGN <string> <offset> (see make_string_foreign
    ;;  in header1.c), not a raw pointer into the heap: the pointer has to
    ;;  follow the string when the collector moves it.  the characters are
    ;;  at offset 4 from the end of the header (after the 32-bit length).
    (define (emit-string->cref s target)
      (RV
       (com "%string->cref")
       ('alloc (s 1) (i #x24) (i 2)) ;; (alloc uses t0 = s0)
       ('sd (n s) (ind 8 (s 1)))
       ('li (s 2) (i 9)) ;; (4<<1)|1
       ('sd (s 2) (ind 16 (s 1)))
       ('mv (n target) (s 1))
       ))

    ;; (in a scratch register: the source may be a live variable.)
    (define (emit-cref->int src target)
      (RV
       ('mv (s 2) (n src))   ;; (unforeign uses t0/t1, not t2)
       ('unforeign (s 2))
       ('mv (n target) (s 2))
       ('tag (n target))))

    (define (emit-int->cref src target)
      (RV
       (com "%int->cref")
       ('mv (s 2) (n src))
       ('untag (s 2))
       ('make_foreign (s 1) (s 2)) ;; (alloc uses t0)
       ('mv (n target) (s 1))))

    (define (emit-c-aref type src index target free)
      (let ((ctype (irken-type->ctype (un-cref type)))
            (size (ctype->size ctype)))
        ;; note: this allocates, so we must save/restore freep.
        (RV
         ('mv (s 1) (n index))
         ('untag (s 1))
         ('li (s 2) (i size))
         ('mul (s 1) (s 1) (s 2))
         ('str_addr freep (l "freep"))
         @ (rv-helper "offset_foreign" (list (rvop:n src) (rvop:s 1)) free)
         ('mv (n target) (c 0))
         ('ldr_addr freep (l "freep"))
         )))

    (define (emit-halloc-malloc type count target fun-name free)
      (let ((ctype (parse-ctype type))
            (size (ctype->size ctype)))
        (if (< target 0)
            (error "dead %halloc?")
            (RV
             ('str_addr freep (l "freep"))
             ('mv (s 1) (n count))
             ('untag (s 1))
             @ (rv-helper fun-name (list (rvop:i size) (rvop:s 1)) free)
             ('ldr_addr freep (l "freep"))
             ('mv (n target) (c 0))))))

    (define (emit-halloc type count target free)
      (emit-halloc-malloc type count target "make_halloc" free))

    (define (emit-malloc type count target free)
      (emit-halloc-malloc type count target "make_malloc" free))

    ;; the syntax for load/store changes depending on data size.
    (define (store-by-size size)
      (match size with
        1 -> 'sb
        2 -> 'sh
        4 -> 'sw
        8 -> 'sd
        x -> (error (format "str-by-size - bad size: " (int x)))
        ))

    ;; note: this relies on a hack in cps.scm that sets the type of this insn to the type of the rval.
    (define (emit-c-set-int type dst src free)
      (let ((ctype (irken-type->ctype type))
            (size (ctype->size ctype)))
        ;; set target? e.g (let ((_ (set! thing...))))
        (RV
         (com "c-set-int " (type-repr type))
         ;; src is read after the call, so it must survive it too.
         @ (rv-helper "get_foreign" (list (rvop:n dst)) (list:cons src free))
         ('mv (s 1) (n src))
         ('untag (s 1))
         ((store-by-size size) (s 1) (ind 0 (c 0)))
         )))

    ;; the syntax for load/store changes depending on data size.
    ;; signed C types must be sign-extended.  (there is no `ldu`.)
    (define (load-by-size size signed?)
      (match size signed? with
        1 #t -> 'lb
        2 #t -> 'lh
        4 #t -> 'lw
        1 #f -> 'lbu
        2 #f -> 'lhu
        4 #f -> 'lwu
        8 _  -> 'ld
        x _  -> (error (format "load-by-size - bad size: " (int x)))
        ))

    (define (emit-c-get-int type src trg free)
      (let ((ctype (irken-type->ctype type))
            (size (ctype->size ctype)))
        (RV
         (com "c-get-int " (type-repr type))
         @ (rv-helper "get_foreign" (list (rvop:n src)) free)
         ((load-by-size size (ctype-signed? ctype)) (n trg) (ind 0 (c 0)))
         ('tag (n trg))
         )))

    (define (emit-cref->string src len trg free)
      (RV
       (com "%cref->string")
       ('str_addr freep (l "freep"))
       @ (rv-helper "irk_cref_2_string" (list (rvop:n src) (rvop:n len)) free)
       ('mv (n trg) (c 0))
       ('ldr_addr freep (l "freep"))))

    (define (emit-sref refexp src trg free)
      (let ((ref0 (sexp->ref refexp)))
        (RV
         (com "%c-sref")
         ('str_addr freep (l "freep"))
         @ (rv-helper "offset_foreign" (list (rvop:n src) (rvop:i ref0.off)) free)
         ('mv (n trg) (c 0))
         ('ldr_addr freep (l "freep")))))

    ;; %c-get-ptr allocates a new foreign, so we must save/restore freep.
    (define (emit-c-get-ptr src trg free)
      (RV
       (com "%c-get-ptr")
       ('str_addr freep (l "freep"))
       @ (rv-helper "irk_c_get_ptr" (list (rvop:n src)) free)
       ('mv (n trg) (c 0))
       ('ldr_addr freep (l "freep"))))

    (define (emit-c-set-ptr dst src free)
      (RV
       (com "%c-set-ptr")
       @ (rv-helper "irk_c_set_ptr" (list (rvop:n dst) (rvop:n src)) free)))

    (define (emit-free ref free)
      (RV
       (com "%free")
       @ (rv-helper "free_foreign" (list (rvop:n ref)) free)))

    (define (emit-c-sizeof ctexp trg)
      (let ((ctype (parse-ctype ctexp)))
        (RV
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
        '%rv (sexp:list (sig . inline)) args               -> (emit-rv-inline inline args k.target)
	'%array-ref _ (vec index)                          -> (emit-array-ref vec index k.target)
	'%array-set _ (vec index val)                      -> (emit-array-set vec index val)
        '%exit _ (arg)                                     -> (emit-exit arg)
        '%getcc _ ()                                       -> (emit-getcc k.target)
        '%putcc _ (k0 v0)                                  -> (emit-putcc k0 v0 k.target)
        '%ensure-heap _ (size)                             -> (emit-ensure-heap size k.free k.target)
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
             (raise (:RV/UnknownPrimop)))
        ))

    (define (emit-jump-continuation jn k)
      (match (used-jumps::get jn) with
        (maybe:yes free) -> (emit k)
        (maybe:no)       -> (list:nil)
        ))

    (define (emit-test reg jn k0 k1 k)
      (let ((skip-label (new-label "L"))
            (jcont (emit-jump-continuation jn k.insn)))
        (RV
         ('li (s 0) (i 6))
         ('beq (s 0) (n reg) (l skip-label))
         @ (emit k0)
         (label skip-label)
         @ (emit k1)
         @ jcont
         )))

    (define (emit-label jn next)
      (RV
       (label (format ".LJ" (int jn)))
       @ (emit next)
       ))

    (define (emit-jump reg target jn free)
      (RV
       @ (if (not (= target -1))
             (RV ('mv (n target) (n reg)))
             (RV))
       ('j (l (format ".LJ" (int jn))))
       ))

    ;; insn:move has two different meanings/uses,
    ;;  from either varref or varset.
    (define (emit-move var src target)
      ;; MOV <dst-ref> <src-reg>
      (cond ((and (>= src 0) (not (= src var)))
             ;; from varset
             (RV ('mv (n var) (n src))))
            ((and (>= target 0) (not (= target var)))
             ;; from varref
             (RV ('mv (n target) (n var))))
            (else (list:nil))))

    ;; init?: see insn:alloc in cps.scm.
    (define (emit-alloc tag size init? target)
      (if (= size 0)
          (emit-immediate (UITAG tag) target)
          (RV ('alloc (n target) (i (UOTAG tag)) (i size))
              @ (if init? (rv-zero-slots target size) (list:nil)))))

    (define (emit-store off arg tup index)
      (if (< index 254)
          (RV
           (com "store off=" (int off) " arg=" (int arg) " tup=" (int tup) " index=" (int index))
           ('sd (n arg) (ind (* 8 (+ 1 off index)) (n tup))))
          (RV ;; offset > 12 bits
           (com "store off=" (int off) " arg=" (int arg) " tup=" (int tup) " index=" (int index))
           ('li (s 0) (i (+ 1 off index)))
           ('slli (s 0) (s 0) (i 3))
           ('add (s 0) (s 0) (n tup))
           ('sd (n arg) (ind 0 (s 0))))))

    (define (emit-close name nreg body target)
      (let ((l0 (new-label "L"))
            (flabel (safe-function-name name))
            (gc-check (if (vars-get-flag name VFLAG-ALLOCATES)
                          (RV ('heapcheck (s 0)))
                          '())))
        (RV
         ('j (l l0))
         (label flabel)
         @ gc-check
         @ (emit body)
         (label l0)
         ('alloc (n target) (i TC_CLOSURE) (i 2))
         ('lld_addr (s 0) (l flabel))
         ('sd (s 0) (ind 8 (n target)))
         ('sd lenv (ind 16 (n target)))
         )))

    ;; this happens with (let ((_ (set! x y))) ...)
    (define (dead-set target)
      (if (not (= target -1))
          (RV ('li (n target) (i TC_UNDEFINED)))
          (RV)))

    ;; to think about: high-depth varref could be:
    ;; 1) a 'normal' funcall to varref()
    ;; 2) a bl to a scratch-only function to get the right rib?
    ;; 3) same, but with different labels for different depths [only one copy]

    (define (emit-varref-inline depth index target)
      (match depth with
        0 -> (RV ('ld (n target) (ind (* 8 (+ index 2)) lenv)))
        -1 -> (RV ((if (< index 254) 'topref 'bigtopref) (n target) (i index)))
        _  -> (RV
               (com "varref " (int depth) ", " (int index))
               ('mv (s 0) lenv)
               @ (n-of depth (RVINSN 'ld (s 0) (ind 8 (s 0))))
               ('ld (n target) (ind (* 8 (+ index 2)) (s 0))))
        ))

    (define (emit-varref depth index target free)
      (if (and (>= depth 0) (or (> depth 5) (> index 253)))
          (RV
           (com "varref " (int depth) ", " (int index))
           ('str_addr lenv (l "lenv"))
           @ (rv-helper "varref" (list (rvop:i depth) (rvop:i index)) free)
           ('mv (n target) (c 0)))
          (emit-varref-inline depth index target)
          ))

    (define (emit-varset-inline depth index val)
      (match depth with
         0 -> (RV ('sd (n val) (ind (* 8 (+ index 2)) lenv)))
        -1 -> (RV ((if (< index 254) 'topset 'bigtopset) (n val) (i index)))
        _  -> (RV
               (com "varset " (int depth) ", " (int index) " = " (int val))
               ('mv (s 0) lenv)
               @ (n-of depth (RVINSN 'ld (s 0) (ind 8 (s 0))))
               ('sd (n val) (ind (* 8 (+ index 2)) (s 0))))
        ))

    (define (emit-varset depth index val target free)
      (if (and (>= depth 0) (or (> depth 5) (> index 253)))
          (RV
           (com "varset " (int depth) ", " (int index))
           ('str_addr lenv (l "lenv"))
           @ (rv-helper "varset" (list (rvop:i depth) (rvop:i index) (rvop:n val)) free)
           @ (dead-set target))
          (emit-varset-inline depth index val)
          ))

    ;; zero words 1..n of the object in register <reg>.
    (define (rv-zero-slots reg n)
      (let loop ((i 1) (acc (list:nil)))
        (cond ((> i n) (reverse acc))
              ;; sd's offset is 12 bits, signed.
              ((< (* 8 i) 2048)
               (loop (+ i 1) (list:cons (RVINSN 'sd zero (ind (* 8 i) (n reg))) acc)))
              (else
               (loop (+ i 1)
                     (append (reverse (RV ('li (s 0) (i (* 8 i)))
                                          ('add (s 0) (n reg) (s 0))
                                          ('sd zero (ind 0 (s 0)))))
                             acc))))))

    (define (emit-new-env size top? init? types target)
      (RV
       (com "new-env size=" (int size))
       @ (if (< size 254)
             (RV ('alloc (n target) (i TC_ENV) (i (+ size 1))))
             (RV ('bigalloc (n target) (i TC_ENV) (i (+ size 1)))))
       ;; the args are stored as they're computed, and computing one can
       ;;  trigger a collection: so the slots (and the link in slot 1)
       ;;  must hold something harmless until then.  (the heap isn't
       ;;  cleared in advance.)
       @ (if init? (rv-zero-slots target (+ size 1)) (list:nil))
       @ (if top?
             (RV (com "set top")
                 ('str_addr (n target) (l "top")))
             (RV))))

    (define (emit-push reg)
      (RV
       (com "push " (int reg))
       ('sd lenv (ind 8 (n reg))) ;; r[1] = lenv
       ('mv lenv (n reg))))       ;; lenv = r

    (define (emit-pop src target)
      (RV
       (com "pop " (int src) ", " (int target))
       ('ld lenv (ind 8 lenv)) ;; lenv = lenv[1]
       @ (if (and (>= target 0) (not (= target src)))
             (RV ('mv (n target) (n src))) ;; target := src
             (RV))
         ))

    (define (safe-function-name name)
      (format "F" (frob-name (symbol->string name))))

    (define (funcall mname funreg)
      (match mname with
        (maybe:yes name)
        ;; `tail` (auipc+jr, clobbering t1) rather than `j`: j reaches only
        ;;  +/-1MB, and the full compiler's text is several times that.
        -> (RV ('tail (l (safe-function-name name)))) ;; known function
        (maybe:no)
        -> (RV
            ('ld (s 0) (ind 8 (n funreg)))  ;; address via closure
            ('jr (s 0)))
        ))

    (define (emit-tail mname fun args)
      (let ((call (funcall mname fun)))
        (match args with
          -1 -> (RV
                 (com "tail " (maybe mname symbol->string "none") " noargs")
                 ('ld lenv (ind 16 (n fun))) ;; lenv = fun[2]
                 @ call) ;; no args
          _  -> (RV
                 (com "tail " (maybe mname symbol->string "none") " Rargs " (int args))
                 ('ld (s 0) (ind 16 (n fun))) ;; s0 = fun[2]
                 ('sd (s 0) (ind 8 (n args))) ;; args[1] = s0
                 ('mv lenv (n args))
                 @ call)
          )))

    (define (emit-trcall depth name regs)
      (let ((nargs (length regs))
            (npop (if (= nargs 0) depth (- depth 1)))
            (name (safe-function-name name))
            (pops (n-of npop (RVINSN 'ld lenv (ind 8 lenv))))
            (stores (map-range
                        i nargs
                        (RVINSN 'sd (n (nth regs i))
                                 (ind (* 8 (+ 2 i)) lenv)))))
        (RV
         (com "trcall " name " depth " (int depth) " regs " (join int->string "," regs) " npop " (int npop))
         @ pops
         @ stores
         ('j (l name)))))

    ;; we emit insns for k0, which may or may not jump to fail continuation in k1
    (define (emit-fatbar label jn k0 k1 k)
      (let ((lfail (format ".LFail" (int label))))
        (RV
         @ (emit k0)
         (label lfail)
         @ (emit k1)
         @ (emit-jump-continuation jn k.insn)
         )))

    (define (emit-fail label npop free)
      (RV
       @ (n-of npop (RVINSN 'ld lenv (ind 8 lenv)))
       ('j (l (format ".LFail" (int label))))))

    (define (emit-call mname fun args k)
      (let ((free (sort < k.free))
            (nregs (length free))
            (target k.target)
            (lreturn (new-label "R")))
        (RV
         (com "build continuation")
         ('alloc (s 0) (i TC_SAVE) (i (+ nregs 3)))
         ('sd k    (ind 8  (s 0)))
         ('sd lenv (ind 16 (s 0)))
         ('mv k (s 0))
         ('lld_addr (s 0) (l lreturn))
         ('sd (s 0) (ind 24 k))
         (com "save free registers: [" (join int->string ", " free) "]")
         @ (map-range j nregs (RVINSN 'sd (n (nth free j)) (ind (* 8 (+ 4 j)) k)))
         @ (if (>= args 0)
               (RV
                (com "call with args")
                ('ld (s 0) (ind 16 (n fun)))
                ('sd (s 0) (ind 8 (n args)))
                ('mv lenv (n args)))
               (RV
                (com "call without args")
                ('ld lenv (ind 16 (n fun)))))
         @ (funcall mname fun)
         (com "label for return")
         (label lreturn)
         (com "restore free registers: [" (join int->string ", " free) "]")
         @ (map-range j nregs (RVINSN 'ld (n (nth free j)) (ind (* 8 (+ 4 j)) k)))
         (com "pop k")
         ('ld lenv (ind 16 k))
         ('ld k    (ind  8 k))
         @ (if (not (= target -1))
               (RV
                (com "target = Rreturn")
                ('mv (n target) retval))
               (RV))
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
                 (push! code (RV (label labels[i]) @ (emit (nth subs i))))))
             (RV
              (com "nvcase: " (sym dtname) " in " (int src) " nalts " (int nalts))
              ('mv (c 0) (n src))
              ((if (eq? dtname 'list) 'getltag 'getutag) (c 0))
              ('lld_addr (s 0) (l jt-label))  ;; s0 := &table[0]
              ('lld_addr (s 1) (l labels[0])) ;; s1 := &L0
              ('slli (c 0) (c 0) (i 3))       ;; c0 := c0 * 8
              ('add (s 0) (s 0) (c 0))        ;; s0 := s0 + c0
              ('ld  (s 0) (ind 0 (s 0)))      ;; s0 := (s0)
              ('add (s 1) (s 1) (s 0))        ;; s1 := s0 + s1
              ('jr (s 1))                     ;; goto *s1
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
        (RV
         (com "pvcase: names = (" (join symbol->string ", " names) ") #alts = " (int (length alts)))
         ('getutag (n src))
         @ (flatten
            (map-range
                i ntags
                (RV ('li (s 1) (i (nth tags i)))
                    ('beq (n src) (s 1) (l labels[i])))))
         @ (if-maybe ealt mealt (emit ealt) (RV))
         @ (flatten
            (map-range
                i ntags
                (RV (label labels[i])
                    @ (emit (nth alts i)))))
         @ (emit-jump-continuation jump-num k.insn))
        ))

    (define (emit-copy-lit index target free)
      (RV
       ('str_addr freep (l "freep"))
       ('ldr_addr (s 1) (l (format "lit_" (int index))))
       @ (rv-helper "irk_copy_tuple" (list (rvop:s 1)) free)
       ('mv (n target) retval)
       ('ldr_addr freep (l "freep"))
      ))

    (define (emit-litcon index kind target free)
      (if (>= target 0)
          (match kind with
            'string -> (RV ('lld_addr (n target) (l (format "lit_" (int index)))))
            'symbol -> (RV ('lld_addr (n target) (l (format "lit_" (int index)))))
            'record -> (emit-copy-lit index target free)
            'vector -> (emit-copy-lit index target free)
            _       -> (RV ('ldr_addr (n target) (l (format "lit_" (int index))))))
          (RV)))

    (define (emit-ffi csig rtype argtypes name args target free)
      ;; rtype/argtypes are predicates, not s-expressions.
      ;; csig is a parsed ctype, and indicates the signature from the FFI data.
      ;; these should be 'identical' unless varargs.  so we really do need both.
      ;;(printf "emit-ffi " (sym name) " csig: " (csig-repr csig) " argtypes: (" (join type-repr " " argtypes) ")\n")
      (match csig with
        ;; variadic follows same calling convention, no need to specialize.
        (csig:fun name frtype fargtypes _)
        -> (emit-c-call (arrow (ffi-result-type frtype rtype) argtypes) (symbol->string name) args target free #t)
        (csig:obj name obtype)
        -> (begin
             (warning "ffi object reference NYI")
             (RV (com "object reference here")))
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
        (insn:alloc tag size init? k)                -> (emitk (emit-alloc tag size init? k.target) k)
        (insn:store off arg tup i k)                 -> (emitk (emit-store off arg tup i) k)
        (insn:close name nreg body k)                -> (emitk (emit-close name nreg body k.target) k)
        (insn:varref d i k)                          -> (emitk (emit-varref d i k.target k.free) k)
        (insn:varset d i v k)                        -> (emitk (emit-varset d i v k.target k.free) k)
        (insn:new-env size top? init? types k)       -> (emitk (emit-new-env size top? init? types k.target) k)
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
                (raise (:RV/CPSNotSupported)))
        ))

    ;; used by %divmod, must be present.
    (remember-variant-label 'tuple)

    (emit cps)
    ))

;; --- last-minute optimizations ---

;; [none yet]

;; --- perfect hash tables ---

(define (emit-rv-lookup-field-hashtables o)
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
      (o.indent)
      (oformat ".data")
      (oformat ".global irk_ambig_size")
      (o.dedent) (oformat "irk_ambig_size:") (o.indent)
      (oformat ".4byte " (int size))
      (oformat ".global G")
      (o.dedent) (oformat "G:") (o.indent)
      (oformat ".4byte " (join int->string "," (vector->list G)))
      (oformat ".global V")
      (o.dedent) (oformat "V:") (o.indent)
      (oformat ".4byte " (join int->string "," (vector->list V)))
      (o.dedent)
      )))

;; --- file output ---

(define (emit-rv-insn o insn)
  (match insn with
    (rv:label name)
    -> (begin (o.dedent) (o.write (format name ":")) (o.indent))
    (rv:global name)
    -> (begin (o.write (format ".global " name)) (o.dedent) (oformat name ":") (o.indent))
    _ -> (o.write (format-rv insn))
    ))

(define (emit-rv-insns o insns)
  (for-list insn insns
    (emit-rv-insn o insn)))

(define (emit-rv-gen o gen)
  (for insn gen
    (emit-rv-insn o insn)))

(define (emit-rv o cname rv)
  (verbose (printf "emit-rv...\n") (flush))
  ;;(o.copy (get-file-contents "include/rv-preamble.s"))
  (o.write "# -*- Mode: asm -*-")
  (o.write "\n\t.include \"rv-preamble.s\"")
  (verbose (printf "emit constructed literals...\n") (flush))
  (o.write "\t.data")
  (o.write "\t.p2align 3, 0x0")
  (asm-emit-constructed o)
  (verbose (printf "emit lookup-field hashtables...\n"))
  (emit-rv-lookup-field-hashtables o)
  (o.indent)
  (emit-rv-insns
   o
   (RV
    (com "rv64 output")
    = ".text"
    (global "toplevel")
    ('addi sp sp (i -112)) ;; 13 registers, 16-byte aligned
    ('sd (r 'ra) (ind 0  sp))
    ('sd (r 's0) (ind 8  sp))
    ('sd (r 's1) (ind 16 sp))
    ('sd (r 's2) (ind 24 sp))
    ('sd (r 's3) (ind 32 sp))
    ('sd (r 's4) (ind 40 sp))
    ('sd (r 's5) (ind 48 sp))
    ('sd (r 's6) (ind 56 sp))
    ('sd (r 's7) (ind 64 sp))
    ('sd (r 's8) (ind 72 sp))
    ('sd (r 's9) (ind 80 sp))
    ('sd (r 's10) (ind 88 sp))
    ('sd (r 's11) (ind 96 sp))
    ('ldr_addr freep (l "freep"))
    ('ldr_addr lenv  (l "lenv"))
    ('ldr_addr k     (l "k"))
    ('lld_addr (r 't0) (l "Lreturn"))
    ('sd (r 't0) (ind 24 k))
    ;; we initialize any slot a collection could see before it's set:
    ;;  see clear_tospace in gc1.c.
    ('str_addr zero (l "clear_tospace"))
    (com "--- user program starts here ---")
    ))
  (emit-rv-gen o (list-generator rv))
  (emit-rv-insns
   o
   (RV
    (label "Lreturn")
    ('ld (r 'ra) (ind 0  sp))
    ('ld (r 's0) (ind 8  sp))
    ('ld (r 's1) (ind 16 sp))
    ('ld (r 's2) (ind 24 sp))
    ('ld (r 's3) (ind 32 sp))
    ('ld (r 's4) (ind 40 sp))
    ('ld (r 's5) (ind 48 sp))
    ('ld (r 's6) (ind 56 sp))
    ('ld (r 's7) (ind 64 sp))
    ('ld (r 's8) (ind 72 sp))
    ('ld (r 's9) (ind 80 sp))
    ('ld (r 's10) (ind 88 sp))
    ('ld (r 's11) (ind 96 sp))
    ('addi sp sp (i 112))
    ('tail (l "exit_continuation"))
    (label "irk_get_metadata")
    ('ldr_addr (r 'a0) (l (format "lit_" (int (- the-context.literals.count 1)))))
    ('ret)
    ))
  (o.close)
  )

(define (compile-to-rv base cps)
  (let ((rv (cps->rv cps)))
    ;; do stuff with rvcps
    (let ((rvpath (format base ".s"))
          (rv-file (file/open-write rvpath #t #o644))
          (orv (make-writer rv-file)))
      (orv.set-indent "\t")
      (notquiet (printf "\n-- RV output --\n : " rvpath "\n"))
      (emit-rv orv "toplevel" rv)
      (notquiet (printf "wrote " (int (orv.get-total)) " bytes to " rvpath ".\n"))
      (let (((path0 ignore-file) (find-file the-context.options.include-dirs "include/header1.c")))
        (file/close ignore-file)
        (list rvpath path0)))))

