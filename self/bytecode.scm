;; -*- Mode: Irken -*-

(define op_lit     (ascii->char 0))
(define op_ret     (ascii->char 1))
(define op_add     (ascii->char 2))
(define op_sub     (ascii->char 3))
(define op_eq      (ascii->char 4))
(define op_tst     (ascii->char 5))
(define op_jmp     (ascii->char 6))
(define op_fun     (ascii->char 7))
(define op_tail    (ascii->char 8))
(define op_tail0   (ascii->char 9))
(define op_env     (ascii->char 10))
(define op_arg     (ascii->char 11))
(define op_ref     (ascii->char 12))
(define op_mov     (ascii->char 13))
(define op_push    (ascii->char 14))
(define op_trcall  (ascii->char 15))
(define op_ref0    (ascii->char 16))
(define op_call    (ascii->char 17))
(define op_pop     (ascii->char 18))
(define op_ge      (ascii->char 19))
(define op_print   (ascii->char 20))
    
;; insn stream consists of bytecodes, label defs, and label refs.
(datatype stream
  (:label-def int)
  (:label-ref int)
  (:code string)
  )

(define stream-repr
  (stream:label-def n) -> (format "L" (int n) ":")
  (stream:label-ref n) -> (format " L" (int n))
  (stream:code s)      -> (format " " (string s))
  )

;; Note: this bytecode design assumes < 256 registers.

(define (compile-to-bytecode base cps)

  (let ((opath (string-append base ".byc"))
        (ofile (file/open-write opath #t #o644))
	(o (make-writer ofile))
        (lits '())
        (nlit 0)
	(label-counter (make-counter 1))
        (jump-label-map (map-maker <))
        )

    (define (new-label)
      (label-counter.inc)
      )

    (define emitk
      acc (cont:k _ _ k) -> (append acc (emit k))
      acc (cont:nil)     -> acc
      )

    (define (emit insn)
      (match insn with
        (insn:literal lit k)			      -> (emitk (emit-literal lit (k/target k)) k)
        (insn:return target)                          -> (emitk (emit-return target) (cont:nil))
        (insn:cexp sig type template args k)          -> (emitk (emit-cexp sig type template args (k/target k)) k)
        (insn:move dst var k)                         -> (emitk (emit-move dst var (k/target k)) k)
        (insn:test reg jn k0 k1 k)                    -> (emitk (emit-test reg jn k0 k1 k) (cont:nil))
        ;;(insn:jump reg target jn free)                -> (emitk (emit-jump reg target jn free) (cont:nil))
        ;; (insn:litcon i kind k)                       -> (begin (emit-litcon i kind (k/target k)) k)
        ;; (insn:testcexp regs sig tmpl jn k0 k1 k)     -> (begin (emit-testcexp regs sig tmpl jn k0 k1 k) (cont:nil))
        ;; (insn:ffi sig type name args k)              -> (error1 "no FFI in C backend" insn)
        ;; (insn:close name nreg body k)                -> (begin (emit-close name nreg body (k/target k)) k)
        ;; (insn:varref d i k)                          -> (begin (emit-varref d i (k/target k)) k)
        ;; (insn:varset d i v k)                        -> (begin (emit-varset d i v (k/target k)) k)
        ;; (insn:new-env size top? types k)             -> (begin (emit-new-env size top? types (k/target k)) k)
        ;; (insn:alloc tag size k)                      -> (begin (emit-alloc tag size (k/target k)) k)
        ;; (insn:store off arg tup i k)                 -> (begin (emit-store off arg tup i) k)
        ;; (insn:invoke name fun args k)                -> (begin (emit-call name fun args k) (cont:nil))
        ;; (insn:tail name fun args)                    -> (begin (emit-tail name fun args) (cont:nil))
        ;; (insn:trcall d n args)                       -> (begin (emit-trcall d n args) (cont:nil))
        ;; (insn:push r k)                              -> (begin (emit-push r) k)
        ;; (insn:pop r k)                               -> (begin (emit-pop r (k/target k)) k)
        ;; (insn:primop name parm t args k)             -> (begin (emit-primop name parm t args k) k)
        ;; (insn:fatbar lab jn k0 k1 k)                 -> (begin (emit-fatbar lab jn k0 k1 k) (cont:nil))
        ;; (insn:fail label npop free)                  -> (begin (emit-fail label npop free) (cont:nil))
        ;; (insn:nvcase tr dt tags jn alts ealt k)      -> (begin (emit-nvcase tr dt tags jn alts ealt k) (cont:nil))
        ;; (insn:pvcase tr tags arities jn alts ealt k) -> (begin (emit-pvcase tr tags arities jn alts ealt k) (cont:nil))
        _ -> (error1 "NYI" insn)
        ))

    ;; the toy vm worked like this: all literals (including immediates) were collected into a vector
    ;;   that was then always referenced by index.  this was done by modifying the CPS stage slightly.
    ;;   I'd like to avoid that now, so can we emulate it to get started?  Or do we make two different
    ;;   'lit' insns in the vm?  Like, 'lit' and 'imm'?  How will 'imm' encode its type? [we could just
    ;;   use the existing irken runtime encoding, read it as an integer and decode at runtime?]
    ;;
    ;; ok, after a second look at vm.scm, I see now that it avoids any decoding at runtime. so the
    ;;   code stream is just integers, and we need to keep it the way it was.  Probably means to avoid
    ;;   modifying cps.scm we need to track all immediates here.  Let's start simply, and de-duplication
    ;;   later.

;    def insn_lit (self, insn):
;        lit_index = insn.params
;        return [opcodes.lit, insn.target, self.encode_int (lit_index)]


    (define (encode-int n)

      (define E
        0 acc -> (list->string acc)
        n acc -> (E (>> n 8) (cons (ascii->char (logand n #xff)) acc))
        )

      (cond ((< n 0) (raise (:NegativeIntInBytecode n)))
            ((< n 255) (char->string (ascii->char n)))
            (else
             (let ((r (E n '())))
               (if (> (string-length r) 254)
                   (raise (:IntegerTooBig n))
                   r)))))

    (define (emit-literal lit target)
      (let ((index nlit))
        (PUSH lits lit)
        (set! nlit (+ 1 nlit))
        (LIST (stream:code 
               (format (char op_lit)
                       (char (ascii->char target))
                       (encode-int index))))
        ))
    
    (define (emit-return reg)
      (LIST (stream:code
             (format (char op_ret)
                     (encode-int reg)))))


    (define get-primop
      "add"   -> op_add
      "sub"   -> op_sub
      "eq"    -> op_eq
      "ge"    -> op_ge
      "print" -> op_print
      x       -> (error1 "no such primop" x)
      )

    ;; %%cexp is really more like 'builtin primapp' here. [consider renaming]
    ;; current prims: add, sub, eq, ge, print
    (define (emit-cexp sig type template args target)
      (LIST (stream:code
             (format (char (get-primop template))
                     (encode-int target)
                     (join encode-int "" args)))))

    (define (emit-move var src target)
      ;; MOV <dst-ref> <src-reg>
      (LIST (stream:code
             (cond ((and (>= src 0) (not (= src var)))
                    ;; from varset
                    (format (char op_mov)
                            (encode-int var)
                            (encode-int src))
                    ;; XXX target := #u
                    )
                   ((and (>= target 0) (not (= target var)))
                    ;; from varref
                    (format (char op_mov)
                            (encode-int target)
                            (encode-int var)))
                   (else "")))))

    ;; # TST <reg> L0
    ;; # <then_code>
    ;; # JMP L1
    ;; # L0:
    ;; # <else_code>
    ;; # L1:
    ;; l0 = label()
    ;; l1 = label()
    ;; then_code = self.emit (then_code)
    ;; else_code = self.emit (else_code)
    ;; then_code.extend ([opcodes.jmp, label_ref (l1), l0])
    ;; return [opcodes.tst, insn.regs[0], label_ref (l0)] + then_code + else_code + [l1]

    ;; XXX push-jump-continuation in the C/LLVM backends checks to see if the cont is actually used.

    (define (emit-test val jn k0 k1 cont)
      (let ((l0 (new-label))
	    (l1 (new-label)))
        (jump-label-map::add jn l1)
        (append
         (LIST (stream:code (format (char op_tst) (encode-int val))) (stream:label-ref l0))
         (emit k0)
         (LIST (stream:code (format (char op_jmp))) (stream:label-ref l0))
         (LIST (stream:label-def l0))
         (emit k1)
         (LIST (stream:label-def l1))
         (emit (k/insn cont))
         )))

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

    (define (emit-stream s)
      (for-list item s
        (match item with
          (stream:code s) -> (o.copy s)
          _ -> (error1 "labels NYI" item)
          )))

    (let ((s (emit cps)))
      (printf "lits:\n")
      (for-list lit lits
        (printn lit))
      (for-list item s
        (printf (stream-repr item) "\n")
        )
      (emit-literals)
      (emit-stream s)
      (o.close)
      )
    #u
    ))
