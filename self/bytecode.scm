;; -*- Mode: Irken -*-

(define (OI name nargs) {code=0 name=name nargs=nargs})

(define opcode-info
  ;;    name   nargs    args
  (list->vector
   (LIST
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
    (OI 'push   1)   ;; args
    (OI 'trcall 3)   ;; offset depth nregs
    (OI 'ref0   2)   ;; target index
    (OI 'call   3)   ;; closure args nregs
    (OI 'pop    1)   ;; target
    (OI 'print  1)   ;; arg
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

;; Note: this bytecode design assumes < 256 registers.

(define (compile-to-bytecode base cps)

  (let ((opath (string-append base ".byc"))
        (ofile (file/open-write opath #t #o644))
	(o (make-writer ofile))
        (lits '())
        (nlit 0)
	(label-counter (make-counter 1))
        (used-jumps (find-jumps cps))
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
        (insn:return target)                          -> (emit-return target)
        (insn:cexp sig type template args k)          -> (emitk (emit-cexp sig type template args (k/target k)) k)
        (insn:move dst var k)                         -> (emitk (emit-move dst var (k/target k)) k)
        (insn:test reg jn k0 k1 k)                    -> (emit-test reg jn k0 k1 k)
        (insn:jump reg target jn free)                -> (emit-jump reg target jn free)
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
        (LINSN 'lit target index)))

    (define (emit-return reg)
      (LINSN 'ret reg))

    ;; %%cexp is really more like 'builtin primapp' here. [consider renaming]
    ;; current prims: add, sub, eq, ge, print
    (define (emit-cexp sig type template args target)
      ;; Note: is it bad that you can use this to call any insn?
      (LIST (stream:insn (string->symbol template) (cons target args))))

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
            (stream:insn 'trcall (index depth nregs))
            -> (PUSH r (INSN 'trcall (resolve index) depth nregs))
            _ -> (PUSH r insn)
            ))

        ;; modified insns
        (reverse r)

        ))

    (define (encode-insn name args)
      (let ((info (name->info name)))
        (if (= (length args) info.nargs)
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
