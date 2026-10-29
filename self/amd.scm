;; -*- Mode: Irken -*-

(require "self/backend.scm")
(require "self/asmlits.scm")

;; NOTE, Oct '24: I'm about 2/3 of the way through implementing this,
;;   after doing arm64 first.  Contemplating doing RV next... I'm noticing
;;   that although the cpu architecture is radically different, there's still a
;;   lot of near-boilerplate here that could probably be captured by an extra
;;   intermediate layer.  Something like an RTL with references to k,lenv,freep,
;;   etc and the register file.  This would then be lowered to the actual CPU
;;   using a real register allocation algorithm, etc.
;;
;;  For example, here we cannot have an insn with two memory operands, but the
;;    translation kinda wants that.  Currently, I'm using an extra pass that clumsily
;;    inserts an extra register move (that of course needs a scratch reg).  It'd be
;;    nice if that was all just taken care of (correctly) by a lower layer.
;;
;;  A possible complication here is integrating 'inline' assembly from user source,
;;    there may be an impedance mismatch between that DSL and the new proposed IL.

(define amd-register-names
  ;;           caller callee    C
  ;;           saved  saved   funarg
  #('rax  ;; 0      X
    'rcx  ;; 1      X              3
    'rdx  ;; 2      X              2
    'rbx  ;; 3            X
    'rsi  ;; 4                     1
    'rdi  ;; 5      X              0
    'rsp  ;; 6      X
    'rbp  ;; 7            X
    'r8   ;; 8      X              4
    'r9   ;; 9      X              5
    'r10  ;; 10     X
    'r11  ;; 11     X
    'r12  ;; 12            X
    'r13  ;; 13            X
    'r14  ;; 14            X
    'r15) ;; 15            X
  )

(define amdreg-32-names
  #('eax 'ecx 'edx 'ebx 'esi 'edi 'esp 'ebp 'r8d 'r9d 'r10d 'r11d 'r12d 'r13d 'r14d 'r15d))
(define amdreg-16-names
  #('ax 'cx 'dx 'bx 'si 'di 'sp 'bp 'r8w 'r9w 'r10w 'r11w 'r12w 'r13w 'r14w 'r15w))
(define amdreg-8-names
  #('al 'cl 'dl 'bl 'sil 'dil 'spl 'bpl 'r8b 'r9b 'r10b 'r11b 'r12b 'r13b 'r14b 'r15b))
(define (amdreg->int name)
  (let/cc return
    (for-range i 16
      (if (eq? name amd-register-names[i])
          (return i)))
    (raise (:AMD/NoSuchRegister name))
    ))

(define Rax 0)
(define Rcx 1)
(define Rdx 2)
(define Rbx 3)
(define Rsi 4)
(define Rdi 5)
(define Rsp 6)
(define Rbp 7)

;; (define (amd-carg->reg n)
;;   ;; XXX should we have synthetic entries for stack-pushed args?
;;   #(Rdi
;;     Rsi
;;     Rdx
;;     Rcx
;;     8 ;; r8
;;     9 ;; r9
;;     )[n])

;; ok so can we use our 'register file' to pass extra arguments,
;; or are we looking at the wrong end of it?  If so, maybe we can
;; change our 'file' regs to use the other end?  Or, I guess, just
;; push the extras like a normal compiler?
;;
;; <c functions do stuff here>
;; <top of stack>
;; 00 [...block for ebp-registers...]
;; 01
;; 02
;; ...
;; 19
;; 20

(define amd-carg-locations
  #((amdop:reg 5) ;; rdi
    (amdop:reg 4) ;; rsi
    (amdop:reg 2) ;; rdx
    (amdop:reg 1) ;; rcx
    (amdop:reg 8) ;; r8
    (amdop:reg 9) ;; r9
    ;; from this point we are doing stack arguments [and poking into
    ;; the 'register file' from the other end]
    (amdop:indd  -160(amdop:reg Rbp))
    (amdop:indd  -152(amdop:reg Rbp))
    (amdop:indd  -144(amdop:reg Rbp))
    (amdop:indd  -136(amdop:reg Rbp))
    (amdop:indd  -128(amdop:reg Rbp))
    (amdop:indd  -120(amdop:reg Rbp))
    (amdop:indd  -112(amdop:reg Rbp))
    ))

(define (amd-carg->reg n)
  amd-carg-locations[n]
  )

(define (amdreg->name n)
  amd-register-names[n])

(datatype amd
  (:comment string)           ;; for inline comments
  (:label string)             ;; local label
  (:global string)            ;; global label
  (:insn symbol (list amdop)) ;; instruction
  (:verb string)              ;; verbatim (e.g. ".quad 1234")
  )

;; XXX how to capture underscore/no-underscore best?
(define (format-amd insn)
  (match insn with
    (amd:insn op ())                  -> (format (sym op)) ;; 'return' and maybe other macros.
    (amd:insn 'callq ((amdop:g lab))) -> (format "callq " (clabel lab)) ;; special-case - (%rip) must be left off.
    (amd:insn op args)                -> (format (sym op) " " (join format-amdop ", " args))
    (amd:label name)                  -> (format name ":")
    (amd:global name)                 -> (format ".global " (clabel name) "\n" (clabel name) ":")
    (amd:verb s)                      -> (format s)
    (amd:comment s)                   -> (format "// " s)
    ))

;; inline assembly.

(datatype amdop
  (:trg)                        ;; target
  (:k)                          ;; Rk
  (:lenv)                       ;; Rlenv
  (:freep)                      ;; Rfreep
  (:retval)                     ;; Rreturn
  (:imm int)                    ;; immediate              $3141
  (:s int)                      ;; scratch
  (:a int)                      ;; inline asm arg
  (:c int)                      ;; c fun arg
  (:n int)                      ;; irken register number
  (:reg int)                    ;; register               %rbx
  (:dir string)                 ;; direct                 0xdeadbeef
  (:rel string)                 ;; pc-relative label
  (:g string)                   ;; global label
  (:grel string)                ;; global pc-rel label
  (:B amdop)                    ;; byte/8
  (:H amdop)                    ;; short/16
  (:W amdop)                    ;; word/32
  (:* amdop)                    ;; indirect (notation specifically for calls/jumps)
  ;; note: 'amdop' in these indirect modes must resolve to a register.
  (:ind amdop)                  ;; indirect               (%rcx)
  (:indd int amdop)             ;; indirect displacement: 8(%rsp)
  (:inds int amdop amdop int)   ;; indirect scaled-index: 8(%rsp, %rcx, 4)
  )

;; this is the mapping from cps registers to amd registers.
(define amd-register-file
  #((amdop:reg 15)                  ;; 0
    (amdop:reg 14)                  ;; 1
    (amdop:reg 13)                  ;; 2
    (amdop:reg 12)                  ;; 3
    (amdop:reg 11)                  ;; 4
    (amdop:reg 10)                  ;; 5
    (amdop:reg 9)                   ;; 6
    (amdop:reg 8)                   ;; 7
    (amdop:indd 0 (amdop:reg Rbp))  ;; 8
    (amdop:indd -8 (amdop:reg Rbp))  ;; 9
    (amdop:indd -16 (amdop:reg Rbp)) ;; 10
    (amdop:indd -24 (amdop:reg Rbp)) ;; 11
    (amdop:indd -32 (amdop:reg Rbp)) ;; 12
    (amdop:indd -40 (amdop:reg Rbp)) ;; 13
    (amdop:indd -48 (amdop:reg Rbp)) ;; 14
    (amdop:indd -56 (amdop:reg Rbp)) ;; 15
    (amdop:indd -64 (amdop:reg Rbp)) ;; 16
    (amdop:indd -72 (amdop:reg Rbp)) ;; 17
    (amdop:indd -80 (amdop:reg Rbp)) ;; 18
    (amdop:indd -88 (amdop:reg Rbp)) ;; 19
    ))

(define parse-amdop
  (sexp:symbol 'trg _)                                     -> (amdop:trg)
  (sexp:symbol 'k _)                                       -> (amdop:k)
  (sexp:symbol 'lenv _)                                    -> (amdop:lenv)
  (sexp:symbol 'freep _)                                   -> (amdop:freep)
  (sexp:symbol 'retval _)                                  -> (amdop:retval)
  (sexp:string label)                                      -> (amdop:dir label)
  (sexp:list ((sexp:symbol 'g _) (sexp:string name)))      -> (amdop:g name)
  (sexp:list ((sexp:symbol 'grel _) (sexp:string name)))   -> (amdop:grel name)
  (sexp:list ((sexp:symbol 's _) (sexp:int num)))          -> (amdop:s num)
  (sexp:list ((sexp:symbol 'a _) (sexp:int num)))          -> (amdop:a num)
  (sexp:list ((sexp:symbol 'i _) (sexp:int num)))          -> (amdop:imm num)
  (sexp:list ((sexp:symbol 'c _) (sexp:int num)))          -> (amdop:c num)
  (sexp:list ((sexp:symbol 'r _) (sexp:symbol reg _)))     -> (amdop:reg (amdreg->int reg))
  (sexp:list ((sexp:symbol 'B _) one))                     -> (amdop:B (parse-amdop one))
  (sexp:list ((sexp:symbol 'H _) one))                     -> (amdop:H (parse-amdop one))
  (sexp:list ((sexp:symbol 'W _) one))                     -> (amdop:W (parse-amdop one))
  (sexp:list ((sexp:symbol 'rel _) (sexp:string label)))   -> (amdop:rel label)
  (sexp:list ((sexp:symbol '* _) one))                     -> (amdop:* (parse-amdop one))
  (sexp:list ((sexp:symbol 'ind _) one))                   -> (amdop:ind (parse-amdop one))
  (sexp:list ((sexp:symbol 'ind _) (sexp:int num) op))     -> (amdop:indd num (parse-amdop op))
  (sexp:list ((sexp:symbol 'ind _) (sexp:int num0) op0 op1 (sexp:int num1)))
  -> (amdop:inds num0 (parse-amdop op0) (parse-amdop op1) num1)
  x -> (raise (:Amd/BadExpression (repr x)))
  )

(define parse-amd-inline
  (sexp:list ((sexp:symbol 'com _) (sexp:string comment))) -> (amd:comment comment)
  (sexp:list ((sexp:symbol 'label _) (sexp:string label))) -> (amd:label label)
  (sexp:list ((sexp:symbol rator _) . rands))              -> (amd:insn rator (map parse-amdop rands))
  x -> (raise (:AmdAsm/BadExpression (repr x)))
  )

(define format-amdop
  (amdop:k)         -> "Rk"
  (amdop:lenv)      -> "Rlenv"
  (amdop:freep)     -> "Rfreep"
  (amdop:retval)    -> "Rretval"
  (amdop:s 0)       -> "Rscratch"
  (amdop:s 1)       -> "Rscratch1"
  (amdop:s 2)       -> "Rscratch2"
  (amdop:c n)       -> (format-amdop (amd-carg->reg n))
  (amdop:n n)       -> (format-amdop amd-register-file[n])
  (amdop:reg r)     -> (format "%" (sym (amdreg->name r)))
  (amdop:imm n)     -> (format "$" (int n))
  (amdop:dir name)  -> (format name)
  (amdop:rel name)  -> (format name "(%rip)")
  (amdop:g name)    -> (format (clabel name))
  (amdop:grel name) -> (format (clabel name) "(%rip)")
  (amdop:* op)      -> (format "*" (format-amdop op))
  (amdop:ind op)    -> (format "(" (format-amdop op) ")")
  (amdop:indd n op) -> (format (int n) "(" (format-amdop op) ")")
  (amdop:inds o op0 op1 s) -> (format (int o) "(" (format-amdop op0) ", " (format-amdop op1) ", " (int s) ")")

  (amdop:B (amdop:reg n)) -> (format "%" (sym amdreg-8-names[n]))
  (amdop:H (amdop:reg n)) -> (format "%" (sym amdreg-16-names[n]))
  (amdop:W (amdop:reg n)) -> (format "%" (sym amdreg-32-names[n]))
  (amdop:B (amdop:n n)) -> (format "%" (sym amdreg-8-names[n]))
  (amdop:H (amdop:n n)) -> (format "%" (sym amdreg-16-names[n]))
  (amdop:W (amdop:n n)) -> (format "%" (sym amdreg-32-names[n]))

  (amdop:B op) -> (format (format-amdop op) ".b") ;; not legal syntax
  (amdop:H op) -> (format (format-amdop op) ".h")
  (amdop:W op) -> (format (format-amdop op) ".w")

  x                -> (raise (:AmdAsm/BadOperand x))
  )

;; DSL for asm in the code below.
;; (intent is to be identical to sexp version)

;; might be nice to add a 'macro' facility here!
;; [that way optimizers/etc have visibility into them, unlike
;;  the .macro facility in the assembler]

;; one amdop
(defmacro AMDOP
  (AMDOP <k>)             -> (amdop:k)
  (AMDOP <lenv>)          -> (amdop:lenv)
  (AMDOP <freep>)         -> (amdop:freep)
  (AMDOP <retval>)        -> (amdop:retval)
  (AMDOP (<r> sym))       -> (amdop:reg (amdreg->name sym))
  (AMDOP (<s> n))         -> (amdop:s n)
  (AMDOP (<i> n))         -> (amdop:imm n)
  (AMDOP (<c> n))         -> (amdop:c n)
  (AMDOP (<n> n))         -> (amdop:n n)
  (AMDOP (<B> o))         -> (amdop:B (AMDOP o))
  (AMDOP (<H> o))         -> (amdop:H (AMDOP o))
  (AMDOP (<W> o))         -> (amdop:W (AMDOP o))
  (AMDOP (<l> l))         -> (amdop:dir l)
  (AMDOP (<rel> l))       -> (amdop:rel l)
  (AMDOP (<g> l))         -> (amdop:g l)
  (AMDOP (<grel> l))      -> (amdop:grel l)
  (AMDOP (<*> a))         -> (amdop:* (AMDOP a))
  (AMDOP (<ind> a))       -> (amdop:ind (AMDOP a))
  (AMDOP (<ind> a b))     -> (amdop:indd a (AMDOP b))
  (AMDOP (<ind> a b c d)) -> (amdop:inds a (AMDOP b) (AMDOP c) d)
  ;; note: other modifiers like 'a' and 'trg' are appropriate for inline asm only.
  (AMDOP <rax>) -> (amdop:reg (amdreg->int 'rax))
  (AMDOP <rcx>) -> (amdop:reg (amdreg->int 'rcx))
  (AMDOP <rdx>) -> (amdop:reg (amdreg->int 'rdx))
  (AMDOP <rbx>) -> (amdop:reg (amdreg->int 'rbx))
  (AMDOP <rsi>) -> (amdop:reg (amdreg->int 'rsi))
  (AMDOP <rdi>) -> (amdop:reg (amdreg->int 'rdi))
  (AMDOP <rsp>) -> (amdop:reg (amdreg->int 'rsp))
  (AMDOP <rbp>) -> (amdop:reg (amdreg->int 'rbp))
  (AMDOP <r8>)  -> (amdop:reg (amdreg->int 'r8))
  (AMDOP <r9>)  -> (amdop:reg (amdreg->int 'r9))
  (AMDOP <r10>) -> (amdop:reg (amdreg->int 'r10))
  (AMDOP <r11>) -> (amdop:reg (amdreg->int 'r11))
  (AMDOP <r12>) -> (amdop:reg (amdreg->int 'r12))
  (AMDOP <r13>) -> (amdop:reg (amdreg->int 'r13))
  (AMDOP <r14>) -> (amdop:reg (amdreg->int 'r14))
  (AMDOP <r15>) -> (amdop:reg (amdreg->int 'r15))
  (AMDOP <rip>) -> (amdop:reg (amdreg->int 'rip))
  )

;; list of amdops
(defmacro AMDOPS
  (AMDOPS)            -> (list:nil)
  (AMDOPS op ops ...) -> (list:cons (AMDOP op) (AMDOPS ops ...))
  )

;; one amdinsn
(defmacro AMDINSN
  ;; could do format here ...
  (AMDINSN <com> s ...) -> (amd:comment (format s ...))
  (AMDINSN <label> l)   -> (amd:label l)
  (AMDINSN <global> l)  -> (amd:global l)
  (AMDINSN op args ...) -> (amd:insn op (AMDOPS args ...))
  )

;; list of amdinsns
(defmacro AMD
  (AMD)                        -> (list:nil)
  ;; verbatim insert
  (AMD <=> x insns ...)        -> (list:cons (amd:verb x) (AMD insns ...))
  ;; '@' = splice in place (similar to UNQUOTE-SPLICING in Scheme)
  (AMD <@> sub)                -> sub
  (AMD <@> sub insns ...)      -> (append2 sub (AMD insns ...))
  (AMD (insn ...) insns ...)  -> (list:cons (AMDINSN insn ...) (AMD insns ...))
  )

(define (underscore-c-labels?)
  (match the-context.options.platform with
    'Linux    -> #f
    'FreeBSD  -> #f
    'Darwin   -> #t
    otherwise -> #t
    ))

(define (cps->amd cps)

  (let ((used-jumps (find-jumps cps))
        (underscores? (underscore-c-labels?)))

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
      (AMD ('movq (n reg) retval)
           ('kreturn)))

    (define (emit-literal lit trg)
      (if (= trg -1)
          (list:nil) ;; dead literal
          (AMD
           (com "lit")
           ('movq (i (encode-immediate lit)) (n trg)))))

    ;; most (all?) llvm ariths are the same as amd64.
    ;; main difference is the 'q' suffix.

    ;; mostly just adding a 'q' suffix.
    (define llop->amdop
      'add  -> 'addq
      'sub  -> 'subq
      'mul  -> 'imulq
      'and  -> 'andq
      'shl  -> 'shlq
      'or   -> 'orq
      'xor  -> 'xorq
      'ashr -> (raise (:AMD/BadInstruction "AMD has special register usage"))
      'sdiv -> (raise (:AMD/BadInstruction "AMD has special register usage"))
      other -> other
      )

    (define (emit-arith op arg0 arg1 target)
      ;; XXX think about ways to avoid untag/tag, depending on <op>
      ;; e.g. (2a+1)+(2b+1) == 2a + 2b + 2 == 2(a+b)+2 so leave tagged and sub1 from result.
      (AMD
       (com "arith " (sym op) " " (int arg0) " " (int arg1))
       ('movq (n arg0) (s 0))
       ('movq (n arg1) (s 1))
       ('untag (s 0))
       ('untag (s 1))
       ((llop->amdop op) (s 1) (s 0)) ;; note order
       ('tag (s 0))
       ('movq (s 0) (n target))
       ))

    ;; amd64 can do 64-bit divmod in one insn.
    ;; the result is put in rdx & rax
    (define (emit-divmod arg0 arg1 target)
      ;; we need the tag for `(:tuple q r)`.
      (let ((tuple-tag (alist/get the-context.variant-labels 'tuple "no :tuple polyvar?")))
        (AMD
         (com "divmod")
         ('untag (n arg0))
         ('untag (n arg1))
         ;; note: we use %rdi as a temporary here, the others are all taken!
         ('alloc (i (UOTAG tuple-tag)) (i 2) (i (* 8 3)) (c 0))
         ('movq (n arg0) rax)
         ('zero rdx)
         ('divq (n arg1))
         ('tag rax)
         ('movq rax (ind 8  (c 0)))
         ('tag rdx)
         ('movq rdx (ind 16 (c 0)))
         ('movq (c 0) (n target))
         )))

    (define llcc->amdcc
      ;; translate between llvm CC and amd CC
      ;; there are more condition codes, e.g. PL/MI for pos/neg,
      ;; might be useful.
      'eq  -> 'e
      'slt -> 'l
      'sle -> 'le
      'sgt -> 'g
      'sge -> 'ge
      'ugt -> 'a
      'ult -> 'b
      'ule -> 'be
      'uge -> 'ae
      cc -> (raise (:AMD/UnknownConditionCode cc))
      )

    (define (llcc-suffix op cc)
      (string->symbol (format (sym op) (sym (llcc->amdcc cc)))))

    (define (emit-icmp cc arg0 arg1 target)
      (AMD
       (com "icmp")
       ('cmp (n arg1) (n arg0))               ;; note order.
       ((llcc-suffix 'set cc) (B (n target))) ;; sets only the lowest byte
       ('movzx (B (n target)) (n target))     ;; widen 8 -> 64 bits
       ('shlq (i 16) (n target))              ;; convert to boolean object
       ('orq (i 6) (n target))))

    (define (emit-test-cmp cc arg0 arg1 reg jn k0 k1 k)
      (let ((skip-label (new-label "L"))
            (jcont (emit-jump-continuation jn k.insn)))
        (AMD
         (com "test-cmp")
         ('cmp (n arg1) (n arg0))
         ((llcc-suffix 'j cc) (l skip-label))
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
                 (AMD
                  (com "dtcon " (sym dtname) "." (sym altname))
                  ('movq (i (get-uitag dtname altname alt.index)) (n target)))
                 (AMD
                  (com "dtcon " (sym dtname) "." (sym altname))
                  ('alloc (i (get-uotag dtname altname alt.index)) (i nargs) (i (* 8 (+ 1 nargs))) (s 0))
                  @ (map-range
                        j nargs
                        (AMDINSN 'movq
                                 (n (nth args j))
                                 (ind (* 8 (+ j 1)) (s 0))))
                  ('movq (s 0) (n target)))))))

    (define (emit-nvget target reg index)
      (AMD
       (com "nvget " (int target) " = r" (int reg) "[" (int index) "]")
       ('movq (n reg) (s 0))
       ('movq (ind (* 8 (+ index 1)) (s 0)) (n target))
       ))

    (define (ambig code)
      (tree/insert! the-context.ambig-rec int-cmp code #u))

    (define (emit-record-get label sig rec trg free)
      (let ((label-code (lookup-label-code label)))
        (match (guess-record-type sig) with
          (maybe:yes sig0)
          -> (AMD
              (com "%record-get " (int rec) "." (sym label))
              ;; rec may live in the %rbp register file, and x86 can't
              ;;   use a memory operand as a base, so go through scratch.
              ('movq (n rec) (s 0))
              ('movq (ind (* 8 (+ 1 (index-eq label sig0))) (s 0)) (n trg)))
          (maybe:no)
          -> (begin
               (ambig label-code)
               (AMD
                (com "ambig %record-get " (sym label) " code " (int label-code))
                ('movq (n rec) (c 0))
                ('movq (i label-code) (c 1))
                @ (callq-with-save-restore "record_fetch" free)
                ('movq rax (n trg))
                ))
          )))

    (define (emit-record-set label sig rec val free)
      (let ((label-code (lookup-label-code label)))
        (match (guess-record-type sig) with
          (maybe:yes sig0)
          -> (AMD
              (com "%record-set " (int rec) "." (sym label))
              ;; see emit-record-get.
              ('movq (n rec) (s 0))
              ('movq (n val) (ind (* 8 (+ 1 (index-eq label sig0))) (s 0))))
          (maybe:no)
          -> (begin
               (ambig label-code)
               (AMD
                (com "ambig %record-set " (sym label) " code " (int label-code))
                ('mov (n rec) (c 0))
                ('mov (i label-code) (c 1))
                ('mov (n val) (c 2))
                @ (callq-with-save-restore "record_store" free)
                ))
          )))

    (define (frob-arg i type)
      (match type with
        (type:tvar id _) -> (list:nil)
        (type:pred name predargs _)
        -> (match name with
             'int          -> (AMD ('untag (c i)))
             'bool         -> (AMD ('unbool (c i)))
             'string       -> (AMD ('unstring (c i)))
             'cref         -> (AMD ('unforeign (c i)))
             'char         -> (list:nil) ;; seems wrong.
             'arrow        -> (list:nil)
             'vector       -> (list:nil) ;; should be raw pointer?
             'symbol       -> (list:nil)
             'continuation -> (list:nil)
             'raw          -> (list:nil)
             kind          -> (if (member-eq? kind c-int-types)
                                  (AMD ('untag (c i)))
                                  (error1 "arm frob-arg:" (type-repr type)))
             )))

    ;; since the args are already in Carg 0..n, we just need to know
    ;; how many there are.
    (define (frob-inputs sig nargs)
      (match sig with
        (type:pred 'arrow (result-type . arg-types) _)
        -> (let ((code (list:nil)))
             (for-range-rev i nargs
               (push! code (frob-arg i (nth arg-types i))))
             (flatten (reverse code)))
        _ ;; some constant type
        -> (list:nil)
        ))

    (define (frob-retval type)
      (match type with
        (type:pred 'long _ _)    -> (AMD ('tag rax))
        (type:pred 'short _ _)   -> (AMD ('tagh rax)) ;; & sign extend
        (type:pred 'int _ _)     -> (AMD ('tagw rax)) ;; & sign extend
        (type:pred 'cref _ _)    -> (AMD ('make_foreign rax rax))
        (type:pred '* _ _)       -> (AMD ('make_foreign rax rax))
        (type:pred 'void _ _)    -> (AMD ('movq (i TC_UNDEFINED) rax))
        (type:pred kind _ _)     -> (if (member-eq? kind c-int-types)
                                        (AMD ('tag rax))
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

    ;; C calls.  Two hazards:
    ;;  1) r8/r9 are both irken registers (7/6) and the 5th/6th C argument
    ;;     registers, so with 5 or more args, loading one argument can
    ;;     clobber the source of another.  in that case go through the
    ;;     stack (simpler than cycle decomposition for the rare big call).
    ;;  2) live caller-saved irken registers must be pushed *before* the
    ;;     arguments are loaded, or the push saves the argument instead.
    (define (emit-c-call sig name args target free convert?)
      (let ((nargs (length args))
            ((pre post) (save-restore-code free)))
        (AMD
         @ pre
         ;; put the args into the proper registers
         @ (if (> nargs 4)
               (AMD
                @ (map-range i nargs (AMDINSN 'pushq (n (nth args i))))
                @ (map-range-rev i nargs (AMDINSN 'popq (c i))))
               (map-range-rev i nargs (AMDINSN 'movq (n (nth args i)) (c i))))
         ;; possibly convert some of those args (irken->c)
         @ (if convert? (frob-inputs sig nargs) (list:nil))
         ;; call the function
         ('callq (g name))
         @ post
         @ (if (= target -1)
               (list:nil)
               (AMD
                @ (if convert? (frob-output sig) (list:nil))
                ('movq retval (n target))))
           )))

    (define (emit-amd-inline inline args target)

      (define (check-target)
        ;; no good mechanism for dead target in inline asm.
        ;; all we can do is throw an error.
        (if (= target -1)
            (raise (:AMD/DeadTarget))
            target))

      (define frob-amdop
        ;; convert these into irken register number
        (amdop:trg)              -> (amdop:n (check-target))
        (amdop:a n)              -> (amdop:n (nth args n))
        (amdop:ind op)           -> (amdop:ind (frob-amdop op))
        (amdop:indd n op)        -> (amdop:indd n (frob-amdop op))
        (amdop:inds n op0 op1 s) -> (amdop:inds n (frob-amdop op0) (frob-amdop op1) s)
        (amdop:B op)             -> (amdop:B (frob-amdop op))
        (amdop:H op)             -> (amdop:H (frob-amdop op))
        (amdop:W op)             -> (amdop:W (frob-amdop op))
        x -> x
        )

      (define frob-amdinsn
        (amd:insn name args) -> (amd:insn name (map frob-amdop args))
        other                -> other
        )

      (let ((parsed (map parse-amd-inline inline)))
        (list:cons
         (amd:comment "amd inline")
         (map frob-amdinsn parsed))
        ))

    ;; vec and index may live in the %rbp register file; a memory operand
    ;;   can't be a base or index, so load both into scratch registers.
    (define (emit-array-ref vec index target)
      (AMD
       ('movq (n vec) (s 0))
       ('movq (n index) (s 1))
       ('untag (s 1))
       ('rangecheck (s 0) (s 1))
       ('leaq (ind 8 (s 0) (s 1) 8) (s 0))
       ('movq (ind (s 0)) (n target))))

    (define (emit-array-set vec index val)
      (AMD
       ('movq (n vec) (s 0))
       ('movq (n index) (s 1))
       ('untag (s 1))
       ('rangecheck (s 0) (s 1))
       ('leaq (ind 8 (s 0) (s 1) 8) (s 0))
       ('movq (n val) (ind (s 0)))))

    (define (emit-exit arg)
      (AMD
       ('movq (n arg) retval)
       ('jmp  (l "Lreturn"))
       ))

    (define (emit-putcc rk rv target)
      (AMD
       (com "putcc")
       ('movq (n rk) k)
       ('movq (n rv) (n target))))

    (define (emit-getcc target)
      (AMD
       (com "getcc")
       ('movq k (n target))))

    (define (emit-ensure-heap size free trg)
      (let ((L0 (new-label "EH"))
            (nfree (length free)))
        (AMD
         (com "%ensure-heap: size in " (int size) " nfree = " (int nfree))
         ;; is freep + size words still below limit?  i.e. freep <= limit - size*8.
         ;; size may live in the %rbp register file (see emit-array-ref).
         ('movq (n size) (s 1))
         ('untag (s 1))
         ('shlq (i 3) (s 1))
         ('movq (grel "limit") (s 0))
         ('subq (s 1) (s 0))
         ('cmp freep (s 0))   ;; flags from (limit - size*8) - freep
         ('jge (l L0))
         ;; need to gc
         ;; add free vars as roots
         ('movq (grel "heap1") (s 0))
         @ (map-range i nfree (AMDINSN 'movq (n (nth free i)) (ind (* 8 (+ i 3)) (s 0))))
         ('movq (i nfree) (c 0))
         ;; invoke GC
         ('callq (g "gc_flip"))
         ;; restore free vars
         ('movq (grel "heap0") (s 0))
         @ (map-range i nfree (AMDINSN 'movq (ind (* 8 (+ i 3)) (s 0)) (n (nth free i))))
         (label L0)
         @ (dead-set trg) ;; only happens in testing, but...
         )))

    ;; these three convert in a scratch register, not in place: the
    ;;  source may be a variable that's still live, and an unwrapped
    ;;  string pointer (string+12) or untagged int left in a register
    ;;  looks to the garbage collector like a pointer into the heap.
    ;; %string->cref makes TC_FOREIGN <string> <offset> (see make_string_foreign
    ;;  in header1.c), not a raw pointer into the heap: the pointer has to
    ;;  follow the string when the collector moves it.  the characters are
    ;;  at offset 4 from the end of the header (after the 32-bit length).
    (define (emit-string->cref src trg)
      (AMD
       (com "%string->cref")
       ('alloc (i #x24) (i 2) (i 24) (s 0))
       ('movq (n src) (s 1))
       ('movq (s 1) (ind 8 (s 0)))
       ('movq (i 9) (ind 16 (s 0))) ;; (4<<1)|1
       ('movq (s 0) (n trg))
       ))

    (define (emit-cref->int src target)
      (AMD
       ('unforeign (n src))
       ('movq (n src) (n target))
       ('tag (n target))))

    (define (emit-int->cref src target)
      (AMD
       (com "%int->cref")
       ('untag (n src))
       ('make_foreign (n src) (s 0))
       ('movq (s 0) (n target))))

    (define (emit-c-aref type src index target free)
      (let ((ctype (irken-type->ctype (un-cref type)))
            (size (ctype->size ctype)))
        (AMD
         (com "%c-aref src r" (int src) " index r" (int index) " size " (int size))
         ('untag (n index))
         ('movq (n src) (c 0))
         ('movq (n index) (c 1))
         @ (if (> size 1) (AMD ('imulq (i size) (c 1))) (AMD))
         @ (callq-with-save-restore "offset_foreign" free)
         ('mov retval (n target))
         )))

    (define (emit-halloc-malloc type count target fun-name free)
      (let ((ctype (parse-ctype type))
            (size (ctype->size ctype)))
        (if (< target 0)
            (error "dead %halloc?")
            (AMD
             ('untag (n count))
             ('movq (i size) (c 0))
             ('movq (n count) (c 1))
             @ (callq-with-save-restore fun-name free)
             ('movq retval (n target))))))

    (define (emit-halloc type count target free)
      (emit-halloc-malloc type count target "make_halloc" free))

    (define (emit-malloc type count target free)
      (emit-halloc-malloc type count target "make_malloc" free))

    ;; note: this relies on a hack in cps.scm that sets the type of this insn to the type of the rval.
    (define (emit-c-set-int type dst src free)
      (let ((ctype (irken-type->ctype type))
            (size (ctype->size ctype)))
        ;; set target? e.g (let ((_ (set! thing...))))
        (AMD
         (com "c-set-int " (type-repr type))
         ('movq (n dst) (c 0))
         ;; src is read after the call, so it must survive it too.
         @ (callq-with-save-restore "get_foreign" (list:cons src free))
         ('untag (n src))
         @ (match size with
             1 -> (AMD ('movb (B (n src)) (ind (s 0))))
             2 -> (AMD ('movw (H (n src)) (ind (s 0))))
             4 -> (AMD ('movl (W (n src)) (ind (s 0))))
             8 -> (AMD ('movq (n src)     (ind (s 0))))
             x -> (error (format "c-set-int - bad size: " (int x)))
             )
           )))

    (define (emit-c-get-int type src trg free)
      (let ((ctype (irken-type->ctype type))
            (size (ctype->size ctype)))
        (AMD
         (com "c-get-int " (type-repr type))
         ('movq (n src) (c 0))
         @ (callq-with-save-restore "get_foreign" free)
         ('zero (s 1))
         ;; signed C types must be sign-extended: e.g. a `short` kevent
         ;;   filter of -1 would otherwise read as 65535.
         @ (match size (ctype-signed? ctype) with
             1 #t -> (AMD ('movsbq (ind (s 0)) (s 1)))
             2 #t -> (AMD ('movswq (ind (s 0)) (s 1)))
             4 #t -> (AMD ('movslq (ind (s 0)) (s 1)))
             1 #f -> (AMD ('movb (ind (s 0)) (B (s 1))))
             2 #f -> (AMD ('movw (ind (s 0)) (H (s 1))))
             4 #f -> (AMD ('movl (ind (s 0)) (W (s 1))))
             8 _  -> (AMD ('movq (ind (s 0))     (s 1)))
             x _  -> (error (format "c-get-int - bad size: " (int x)))
             )
          ('movq (s 1) (n trg))
          ('tag (n trg))
         )))

    (define (emit-cref->string src len trg free)
      (AMD
       (com "%cref->string")
       ('movq (n src) (c 0))
       ('movq (n len) (c 1))
       @ (callq-with-save-restore "irk_cref_2_string" free)
       ('movq retval (n trg))))

    (define (emit-sref refexp src trg free)
      (let ((ref0 (sexp->ref refexp)))
        (AMD
         (com "%c-sref")
         ('movq (n src) (c 0))
         ('movq (i ref0.off) (c 1))
         @ (callq-with-save-restore "offset_foreign" free)
         ('movq retval (n trg))
         )))

    (define (emit-c-get-ptr src trg free)
      (AMD
       (com "%c-get-ptr")
       ('movq (n src) (c 0))
       @ (callq-with-save-restore "irk_c_get_ptr" free)
       ('movq retval (n trg))
       ))

    (define (emit-c-set-ptr dst src free)
      (AMD
       (com "%c-set-ptr")
       ('movq (n dst) (c 0))
       ('movq (n src) (c 1))
       @ (callq-with-save-restore "irk_c_set_ptr" free)
       ))

    (define (emit-free ref free)
      (AMD
       (com "%free")
       ('movq (n ref) (c 0))
       @ (callq-with-save-restore "free_foreign" free)))

    (define (emit-c-sizeof ctexp trg)
      (let ((ctype (parse-ctype ctexp)))
        (AMD
         (com "%c-sizeof")
         ;; tagged, like every other irken int (arm/rv emit a 'tag).
         ('movq (i (+ 1 (* 2 (ctype->size ctype)))) (n trg))
         )))

    (define (emit-primop name params type args k)
      (match name params args with
        '%llarith (sexp:symbol op _) (arg0 arg1)           -> (emit-arith op arg0 arg1 k.target)
        '%divmod  _                  (arg0 arg1)           -> (emit-divmod arg0 arg1 k.target)
        '%llicmp  (sexp:symbol cc _) (arg0 arg1)           -> (emit-icmp cc arg0 arg1 k.target)
        '%lleq    _                  (arg0 arg1)           -> (emit-icmp 'eq arg0 arg1 k.target)
        '%dtcon   (sexp:cons dtname altname) args          -> (emit-dtcon dtname altname args k.target)
	'%nvget   (sexp:list (_ (sexp:int index) _)) (reg) -> (emit-nvget k.target reg index)
        '%amd (sexp:list (sig . inline)) args              -> (emit-amd-inline inline args k.target)
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
             (raise (:Amd/UnknownPrimop)))
        ))

    (define (emit-jump-continuation jn k)
      (match (used-jumps::get jn) with
        (maybe:yes free) -> (emit k)
        (maybe:no)       -> (list:nil)
        ))

    (define (emit-test reg jn k0 k1 k)
      (let ((skip-label (new-label "L"))
            (jcont (emit-jump-continuation jn k.insn)))
        (AMD
         (com "test " (int reg))
         ('cmpq (i 6) (n reg)) ;; #x06 == #f
         ('je (l skip-label))
         @ (emit k0)
         (label skip-label)
         @ (emit k1)
         @ jcont
         )))

    (define (emit-label jn next)
      (AMD
       (label (format ".LJ" (int jn)))
       @ (emit next)
       ))

    (define (emit-jump reg trg jn free)
      (AMD
       @ (if (not (= trg -1))
             (AMD ('movq (n reg) (n trg)))
             (AMD))
       ('jmp (l (format ".LJ" (int jn))))
       ))

    ;; insn:move has two different meanings/uses,
    ;;  from either varref or varset.
    (define (emit-move var src target)
      ;; MOV <dst-ref> <src-reg>
      (cond ((and (>= src 0) (not (= src var)))
             ;; from varset
             ;; version with scratch move
             ;;(AMD ('movq (n src) (s 0)) ('movq (s 0) (n var)))
             ;; version with 'impossible' mem->mem insn
             (AMD ('movq (n src) (n var))))
            ((and (>= target 0) (not (= target var)))
             ;; from varref
             ;; with scratch move
             ;;(AMD ('movq (n var) (s 0)) ('movq (s 0) (n target)))
             (AMD ('movq (n var) (n target))))
            (else (list:nil))))

    (define (emit-alloc tag size target)
      (if (= size 0)
          (AMD (com "alloc imm " (int tag)) ('movq (i (UITAG tag)) (n target)))
          (AMD ('alloc (i (UOTAG tag)) (i size) (i (* 8 (+ 1 size))) (n target)))))

    (define (emit-store off arg tup index)
      (AMD
       (com "store off=" (int off) " arg=" (int arg) " tup=" (int tup) " index=" (int index))
       ('movq (n tup) (s 0))
       ('movq (n arg) (s 1))
       ('movq (s 1) (ind (* 8 (+ 1 off index)) (s 0)))
       ))

    (define (emit-close name nreg body target)
      (let ((l0 (new-label "L"))
            (flabel (safe-function-name name))
            (gc-check (if (vars-get-flag name VFLAG-ALLOCATES)
                          (AMD ('heapcheck))
                          '())))
        (AMD
         (com "close " (sym name))
         ('jmp (l l0))
         (label flabel)
         @ gc-check
         @ (emit body)
         (label l0)
         ('alloc (i TC_CLOSURE) (i 2) (i (* 8 (+ 1 2))) (n target))
         ('leaq (rel flabel) (s 0))
         ('movq (n target) (s 1))
         ('movq (s 0) (ind 8 (s 1)))
         ('movq lenv (ind 16 (s 1)))
         )))

    ;; this happens with (let ((_ (set! x y))) ...)
    (define (dead-set target)
      (if (not (= target -1))
          (AMD ('movq (i TC_UNDEFINED) (n target)))
          (AMD)))

    (define (topref idx trg)
      (AMD
       (com "topref " (int idx))
       ('movq (grel "top") (s 0))
       ('movq (ind (* 8 (+ idx 2)) (s 0)) (s 0))
       ('movq (s 0) (n trg))))

    (define (emit-varref depth index target)
      (match depth with
        0 -> (AMD
              (com "varref0 " (int index))
              ('movq lenv (s 0))
              ('movq (ind (* 8 (+ index 2)) (s 0)) (n target)))
        -1 -> (topref index target)
        _  -> (AMD
               (com "varref " (int depth) ", " (int index))
               ('movq lenv (s 0))
               @ (n-of depth (AMDINSN 'movq (ind 8 (s 0)) (s 0)))
               ('movq (ind (* 8 (+ index 2)) (s 0)) (n target)))
        ))

    (define (emit-varset* depth index val)
      (match depth with
         0 -> (AMD
               (com "varset0 " (int index))
               ('movq lenv (s 0))
               ('movq (n val) (ind (* 8 (+ index 2)) (s 0))))
        -1 -> (AMD
               (com "topset " (int index))
               ('movq (grel "top") (s 0))
               ('movq (n val) (s 1))
               ('movq (s 1) (ind (* 8 (+ index 2)) (s 0))))
        _  -> (AMD
               (com "varset " (int depth) ", " (int index) " = " (int val))
               ('mov lenv (s 0))
               @ (n-of depth (AMDINSN 'movq (ind 8 (s 0)) (s 0)))
               ('movq (n val) (ind (* 8 (+ index 2)) (s 0))))
        ))

    (define (emit-varset depth index val target)
      (AMD @ (emit-varset* depth index val)
           @ (dead-set target)))

    (define (emit-new-env size top? types target)
      ;; types are just for comment metadata [for now]
      (AMD
       (com "new-env size=" (int size))
       ('alloc (i TC_ENV) (i (+ size 1)) (i (* 8 (+ size 2))) (n target))
       @ (if top?
             (AMD
              (com "set top")
              ('movq (n target) (grel "top")))
             (list:nil))))

    (define (emit-push reg)
      (AMD
       (com "push " (int reg))
       ('movq (n reg) (s 0))        ;; s0 = Rn
       ('movq lenv (ind 8 (s 0)))   ;; Rn[1] = lenv
       ('movq (s 0) lenv)           ;; lenv = Rn
       ))

    (define (emit-pop src target)
      (AMD
       (com "pop " (int src) ", " (int target))
       ;;('movq (ind 8 lenv) lenv)    ;; lenv = lenv[1]
       ('movq lenv (s 0))
       ('movq (ind 8 (s 0)) (s 0))
       ('movq (s 0) lenv)
       @ (if (and (>= target 0) (not (= target src)))
             (AMD ('movq (n src) (n target))) ;; target := src
             (list:nil))
         ))

    (define (safe-function-name name)
      (format "F" (frob-name (symbol->string name))))

    (define (funcall mname funreg)
      (match mname with
        (maybe:yes name)
        -> (AMD ('jmp (l (safe-function-name name)))) ;; known function
        (maybe:no)
        -> (AMD
            ('movq (n funreg) (s 0))
            ('movq (ind 8 (s 0)) (s 0))
            ('jmpq (* (s 0))))
        ))

    (define (emit-tail mname fun args)
      (let ((call (funcall mname fun)))
        (match args with
          -1 -> (AMD
                 (com "tail " (maybe mname symbol->string "none") " noargs")
                 ('movq (n fun) (s 0))         ;; s0 = fun
                 ('movq (ind 16 (s 0)) lenv)   ;; lenv = fun[2]
                 @ call) ;; no args
          _  -> (AMD
                 (com "tail " (maybe mname symbol->string "none") " Rargs " (int args))
                 ('movq (n fun) (s 0))
                 ('movq (ind 16 (s 0)) (s 0))      ;; s0 = fun[2]
                 ('movq (n args) (s 1))            ;; s1 = args
                 ('movq (s 0) (ind 8 (s 1)))       ;; args[1] = s0
                 ('movq (s 1) lenv)                ;; lenv = args
                 @ call)
          )))

    (define (emit-trcall depth name regs)
      (let ((nargs (length regs))
            (npop (if (= nargs 0) depth (- depth 1)))
            (name (safe-function-name name))
            (pops (n-of npop (AMDINSN 'movq (ind 8 (s 0)) (s 0)))) ;; lenv = lenv[1]
            (stores (map-range
                        i nargs
                        (AMDINSN 'movq (n (nth regs i)) ;; lenv[i..] = Rn..
                                 (ind (* 8 (+ 2 i)) (s 0)))))) ;; note: s0 == lenv
        (AMD
         (com "trcall " name " depth " (int depth) " regs " (join int->string "," regs) " npop " (int npop))
         ;; pull lenv into %rax
         ('movq lenv (s 0))
         @ pops
         @ (if (> npop 0) (AMD ('movq (s 0) lenv)) (AMD)) ;; if we popped, store result in lenv.
         @ stores
         ('jmp (l name)))))

    ;; we emit insns for k0, which may or may not jump to fail continuation in k1
    (define (emit-fatbar label jn k0 k1 k)
      (let ((lfail (format ".LFail" (int label))))
        (AMD
         @ (emit k0)
         (label lfail)
         @ (emit k1)
         @ (emit-jump-continuation jn k.insn)
         )))

    (define (emit-fail label npop free)
      (AMD
       ('movq lenv (s 0))
       @ (n-of npop (AMDINSN 'movq (ind 8 (s 0)) (s 0))) ;; lenv = lenv[1]
       ('movq (s 0) lenv)
       ('jmp (l (format ".LFail" (int label))))))

    (define (emit-call mname fun args k)
      (let ((free (sort < k.free))
            (nregs (length free))
            (target k.target)
            (lreturn (new-label "R")))
        (AMD
         (com "build continuation")
         ('alloc (i TC_SAVE) (i (+ nregs 3)) (i (* 8 (+ nregs 4))) (s 0)) ;; s0 := save[...]
         ('movq k (s 1))              ;; s1 := k
         ('movq (s 1) (ind 8 (s 0)))  ;; save[1] := k
         ('movq lenv (s 1))           ;; s1 := lenv
         ('movq (s 1) (ind 16 (s 0))) ;; save[2] := lenv
         ('movq (s 0) k)              ;; k := save
         ('leaq (rel lreturn) (s 1))
         ('movq (s 1) (ind 24 (s 0)))
         (com "save free registers: [" (join int->string ", " free) "]")
         @ (map-range j nregs (AMDINSN 'movq (n (nth free j)) (ind (* 8 (+ 4 j)) (s 0))))
         @ (if (>= args 0)
               (AMD
                (com "call with args")
                ('movq (n fun) (s 0))
                ('movq (ind 16 (s 0)) (s 0))
                ('movq (n args) (s 1))
                ('movq (s 0) (ind 8 (s 1)))
                ('movq (s 1) lenv))
               (AMD
                (com "call without args")
                ('movq (n fun) (s 0))
                ('movq (ind 16 (s 0)) lenv)))
         @ (funcall mname fun)
         (com "label for return")
         (label lreturn)
         (com "restore free registers: [" (join int->string ", " free) "]")
         ('movq k (s 2))
         @ (map-range j nregs (AMDINSN 'movq (ind (* 8 (+ 4 j)) (s 2)) (n (nth free j))))
         (com "pop k")
         ('movq (ind 16 (s 2)) (s 1))
         ('movq (s 1) lenv)
         ('movq (ind 8 (s 2)) (s 2))
         ('movq (s 2) k)
         @ (if (not (= target -1))
               (AMD
                (com "target = Rreturn")
                ('movq retval (n target)))
               (AMD))
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
      ;; we'll use 32-bit offsets rather than 64-bit.
      ;; .4byte LNV2-TB0, LEA3-TB0, ...
      (format
       ".4byte "
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
                 (push! code (AMD (label labels[i]) @ (emit (nth subs i))))))
             (AMD
              (com "nvcase: " (sym dtname) " in " (int src) " nalts " (int nalts))
              ('movq (n src) (c 0))
              ((if (eq? dtname 'list) 'getltag 'getutag)) ;; assumes arg is (c 0)
              ('leaq (rel jt-label) (s 0))
              ('leaq (rel labels[0]) (s 1))
              ('movslq (ind 0 (s 0) (c 0) 4) (s 0))
              ('addq (s 0) (s 1))
              ('jmp (* (s 1)))
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
        (AMD
         (com "pvcase: names = (" (join symbol->string ", " names) ") #alts = " (int (length alts)))
         ('movq (n src) (c 0))
         ('getutag)
         @ (flatten
            (map-range
                i ntags
                (AMD ('cmpq (i (nth tags i)) (c 0))
                     ('je (l labels[i])))))
         @ (if-maybe ealt mealt (emit ealt) (list:nil))
         @ (flatten
            (map-range
                i ntags
                (AMD (label labels[i])
                     @ (emit (nth alts i)))))
         @ (emit-jump-continuation jump-num k.insn))
        ))

    ;; kludge: maps to r11, r10, r9, r8
    (define caller-saves-set '(4 5 6 7))
    ;; call a C function, preserving any irken registers in `free` that
    ;;   live in caller-saved machine registers.  `free` should include
    ;;   any register the insn itself still reads after the call.
    ;; %rsp is 16-byte aligned in irken code (see the prologue in
    ;;   compile-to-amd), and the ABI requires that at the call, so an odd
    ;;   number of pushes gets an extra 8 bytes of padding.
    (define (save-restore-code free)
      (let ((pushes '())
            (pops '())
            (saved '()))
        (for-list reg free
          (when (and (member-eq? reg caller-saves-set)
                     (not (member-eq? reg saved)))
            (push! saved reg)
            (push! pushes (AMDINSN 'pushq (n reg)))
            (push! pops (AMDINSN 'popq (n reg)))))
        (set! pops (reverse pops))
        (if (odd? (length saved))
            (:tuple (AMD ('subq (i 8) rsp) @ pushes)
                    (AMD @ pops ('addq (i 8) rsp)))
            (:tuple pushes pops))))

    (define (callq-with-save-restore name free)
      (let (((pre post) (save-restore-code free)))
        (AMD
         @ pre
         ('callq (g name))
         @ post)))

    (define (emit-copy-lit index target free)
      (AMD
       ('movq (rel (format "lit_" (int index))) (c 0))
       @ (callq-with-save-restore "irk_copy_tuple" free)
       ('movq retval (n target))))

    (define (emit-litcon index kind target free)
      (if (>= target 0)
          (match kind with
            'string -> (AMD ('leaq (rel (format "lit_" (int index))) (n target)))
            'symbol -> (AMD ('leaq (rel (format "lit_" (int index))) (n target)))
            'record -> (emit-copy-lit index target free)
            'vector -> (emit-copy-lit index target free)
            _       -> (AMD ('movq (rel (format "lit_" (int index))) (n target))))
          (list:nil)))

    (define (emit-ffi csig rtype argtypes name args target free)
      ;; rtype/argtypes are predicates, not s-expressions.
      ;; csig is a parsed ctype, and indicates the signature from the FFI data.
      ;; these should be 'identical' unless varargs.  so we really do need both.
      ;;(printf "emit-ffi " (sym name) " csig: " (csig-repr csig) " argtypes: (" (join type-repr " " argtypes) ")\n")
      (match csig with
        (csig:fun name frtype fargtypes _) ;; varargs no matter here.
        -> (let ((sig (arrow rtype argtypes)))
             (emit-c-call sig (symbol->string name) args target free #t))
        (csig:obj name obtype)
        -> (begin
             (warning "ffi object reference NYI")
             (AMD (com "object reference here")))
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
               (printf (bold "invalid CPS instruction:") "\n")
               (print-insn insn) (newline)
               (raise (:AMD/InvalidCPSInsn)))
        ))

    ;; used by %divmod, must be present.
    (remember-variant-label 'tuple)

    (emit cps)
    ))

;; --- register canon, naming, optimize ---

(define (amd-canonize insns)

  ;; this defines our mapping of irken registers to
  ;;  1) any _actual_ registers we can spare...
  ;;  2) ... with the remainder mapped to the %rbp file.
  ;; in order to pretend that our file versions are like
  ;; real registers we need to convert mem-to-mem operations
  ;; into mem->reg->mem operations.
  ;;
  ;; ** can this work with insns other than 'movq' as well?
  ;; not really? not easily?  might need a model of the insn set?
  ;; e.g. subq 8(%rbp), 16(%rbp)  [ subq n0, n1 == n1 -= n0 ]
  ;; => movq 8(%rbp), %rax
  ;;    subq %rax, 16(%rbp)

  (define (irken-reg->amdop n) amd-register-file[n])

  ;; we want all registers to be put in `amdreg` form.
  (define N
    (amdop:k)                -> (amdop:grel "k")
    (amdop:lenv)             -> (amdop:grel "lenv")
    (amdop:freep)            -> (amdop:grel "freep")
    (amdop:retval)           -> (amdop:reg Rax)
    (amdop:s 0)              -> (amdop:reg Rax)
    (amdop:s 1)              -> (amdop:reg Rcx)
    (amdop:s 2)              -> (amdop:reg Rdx)
    (amdop:c n)              -> (amd-carg->reg n)
    (amdop:n n)              -> (irken-reg->amdop n)
    (amdop:* op)             -> (amdop:* (N op))
    (amdop:ind op)           -> (amdop:ind (N op))
    (amdop:indd n op)        -> (amdop:indd n (N op))
    (amdop:inds o op0 op1 s) -> (amdop:inds o (N op0) (N op1) s)
    (amdop:B op)             -> (amdop:B (N op))
    (amdop:H op)             -> (amdop:H (N op))
    (amdop:W op)             -> (amdop:W (N op))
    otherwise                -> otherwise
    )

  (define M
    (amd:insn op args)       -> (amd:insn op (map N args))
    otherwise                -> otherwise
    )

  (makegen emit
    (for-list insn insns
      (emit (M insn)))))

;; we'd like to have 'irken registers' be a mix of real registers and
;; rbp-indexed fakes.  we can have that if we pretend we can have
;;     op mem, mem
;; and clean it up in a later pass.

;; this is valid only after canonicalization
(define (amd-no-mem2mem-gen gen)

  ;; addressing modes: reg, imm, dir, rel, ind, indd, inds
  ;; we have to catch anything that's not reg/imm

  (define mem-op?
    (amdop:reg _) -> #f
    (amdop:imm _) -> #f
    (amdop:B op)  -> (mem-op? op)
    (amdop:H op)  -> (mem-op? op)
    (amdop:W op)  -> (mem-op? op)
    _             -> #t
    )

  (makegen emit

    (define (frob-insn op src dst)
      ;; [a smarter system would have use/def info and know of a spare reg on hand]
      ;; OPERAND MEM0 MEM1
      ;; => movq MEM0 %rax
      ;;    OPERAND %rax MEM1
      (emit (amd:comment "no-mem2mem"))
      ;; %rax breaks any use of scratch0, trying %rbx now.
      (emit (amd:insn    op (list src (amdop:reg Rbx))))
      (emit (amd:insn 'movq (list (amdop:reg Rbx) dst))))

    (for insn gen
      (match insn with
        (amd:insn op (src dst))
        -> (match (mem-op? src) (mem-op? dst) with
             #t #t -> (frob-insn op src dst)
             _  _  -> (emit insn)
             )
        _
        -> (emit insn)
        ))))

(define (amd-redundant-gen gen)

  (define bogus (amd:insn 'nope (list)))

  (makegen emit

    (let ((last bogus))

      (define (memit insn)
        (if (not (eq? insn bogus))
            (emit insn)))

      (define self-move?
        (amd:insn 'movq (src dst)) -> (eq? src dst)
        _                          -> #f
        )

      (for insn gen
        (cond ((self-move? insn)
               (emit (amd:comment (format "redund: " (format-amd insn)))))
              (else
               (match last insn with
                 ;; looking for:
                 ;;   movq %rax, %rdx
                 ;;   movq %rdx, %rax
                 ;; if we find it, pretend the second insn never happened.
                 (amd:insn 'movq (s0 d0)) (amd:insn 'movq (s1 d1))
                 -> (cond ((and (eq? d0 s1) (eq? s0 d1))
                           (emit (amd:comment (format "redund: " (format-amd insn)))))
                          (else
                           (memit last)
                           (set! last insn)))
                 ;; any other insn we emit last & continue on...
                 _ (amd:insn op rands)
                 -> (begin
                      (memit last)
                      (set! last insn))
                 ;; non-insn (comments, labels, etc) push through
                 _ _
                 -> (begin
                      (memit last)
                      (memit insn)
                      (set! last bogus))
                 ))))
      (memit last)
      )))

;; --- file output ---

(define (emit-amd-insn o insn)
  (match insn with
    (amd:label name)
    -> (begin (o.dedent) (o.write (format name ":")) (o.indent))
    (amd:global name)
    -> (begin (o.write (format ".global " (clabel name))) (o.dedent) (oformat (clabel name) ":") (o.indent))
    _ -> (o.write (format-amd insn))
    ))

(define (emit-amd-insns o insns)
  (for-list insn insns
    (emit-amd-insn o insn)))

(define (emit-amd-gen o gen)
  (for insn gen
    (emit-amd-insn o insn)))

(define (emit-amd o cname insns)
  (verbose (printf "emit-amd...\n") (flush))
  (o.write "// -*- Mode: asm -*-")
  (oformat "\n\t.include "
           (string
            (match the-context.options.platform with
              'Darwin -> "amd-mach-preamble.s"
              _       -> "amd-elf-preamble.s")))
  ;;(o.write "\n\t.include \"amd-preamble.s\"")
  (verbose (printf "emit constructed literals...\n") (flush))
  (o.write "\t.data")
  (o.write "\t.p2align 3, 0x0")
  ;; XXX rename to asm-emit-constructed
  (asm-emit-constructed o)
  (verbose (printf "emit lookup-field hashtables...\n"))
  ;; XXX rename to emit-asm...
  (emit-asm-lookup-field-hashtables o)
  (o.indent)
  (emit-amd-insns
   o
   (AMD
    (com " amd64 output for " (sym the-context.options.platform))
    = (if (eq? the-context.options.platform 'Linux)
          ".section .note.GNU-stack,\"\",@progbits"
          "")
    = ".text"
    (global "toplevel")
    ('pushq r15)
    ('pushq r14)
    ('pushq r13)
    ('pushq r12)
    ('pushq rbx)
    ('movq rsp rbp)
    ('subq (i (* 20 8)) rsp)
    ('leaq (rel "Lreturn") rax)
    ('movq (grel "k") rcx)
    ('movq rax (ind 24 rcx))
    ))
  (emit-amd-gen
   o
   (amd-redundant-gen
    (amd-no-mem2mem-gen
     (amd-canonize insns))))
  (emit-amd-insns
   o
   (AMD
    (label "Lreturn")
    (com "restore registers")
    ('addq (i (* 20 8)) rsp) ;; restore %rsp
    ('popq rbx)
    ('popq r12)
    ('popq r13)
    ('popq r14)
    ('popq r15)
    ('movq rax rdi)                ;; retval => (c 0)
    ('jmp (g "exit_continuation")) ;; tail call, maybe.
    (com "get_metadata function")
    (global "irk_get_metadata")
    = (format "movq lit_" (int (- the-context.literals.count 1)) "(%rip) , %rax")
    ('ret)
    ))
  (o.dedent)
  (o.close)
  )

(define (compile-to-amd base cps)
  (let ((amd (cps->amd cps)))
    ;; do stuff with amdcps
    (let ((amdpath (format base ".s"))
          ;;(amd-file (file/open-write amdpath #t #o644))
          (amd-file (file/open-write amdpath #t #o644))
          (oamd (make-writer amd-file)))
      (oamd.set-indent "\t")
      (notquiet (printf "\n-- AMD output --\n : " amdpath "\n"))
      (emit-amd oamd "toplevel" amd)
      (notquiet (printf "wrote " (int (oamd.get-total)) " bytes to " amdpath ".\n"))
      (let (((path0 ignore-file) (find-file the-context.options.include-dirs "include/header1.c")))
        (file/close ignore-file)
        (list amdpath path0)))))
