;; -*- Mode: Scheme -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")

(datatype object
  (:int int)
  (:char char)
  (:bool bool)
  (:string string)
  (:undefined undefined)
  (:symbol symbol)
  (:continuation continuation)
  (:closure (vector (object)) (vector int) int (lenv))
  (:tuple (vector (object)))
  )

(datatype lenv
  (:nil)
  (:rib (object) (lenv))
  )

;; probably need a place to store LITS and CODE... perhaps
;; another vmcont type that contains them, to be used only
;; when calling an unknown function.
(datatype vmcont
  (:nil)
  (:k (vmcont) (lenv) int (vector (object)))
  )

;; XXX need a way to declare record types!
;;(datatype code
;;  ;; <lits> <insns>
;;  (:t (vector object) (vector int))
;;  )

(define (CONS a b) (list:cons a b))
(define (NIL) (list:nil))

(define (load path)

  (let ((f (file:open-read path))
	(code (file:read-buffer f))
	(pc 0)
	(r (NIL))
	)

    (define (next)
      (let ((r (string-ref code pc)))
	(set! pc (+ pc 1))
	r))

    (define (nexti)
      (char->ascii (next)))

    (define (read-int)
      (let ((n (char->ascii (next))))
	(if (= n 255)
	    (let loop ((n 0) (len (char->ascii (next))))
	      (if (zero? len)
		  n
		  (loop (+ (<< n 8) (char->ascii (next))) (sub1 len))))
	    n)))

    (let ((lits
	   ;; read vector of literals
	   (let loop ((lits (NIL)))
	     (match (next) with
	       #\+ -> (loop (CONS (object:int (read-int)) lits))
	       #\- -> (loop (CONS (object:int (- 0 (read-int))) lits))
	       #\T -> (loop (CONS (object:bool #t) lits))
	       #\F -> (loop (CONS (object:bool #f) lits))
	       #\. -> (reverse lits)
	       _ -> (error "reading lits")
	       ))))

      (let loop ((stream (NIL)))
	(if (= pc (string-length code))
	    { code = (reverse stream) lits = lits }
	    (loop (CONS (read-int) stream))))
      )))

;; performance hacks - these could definitely go away with a little
;;   more smarts in analyze.py.
(define (+1 a)
  (%%cexp (int -> int) "%s+1" a))
(define (+2 a)
  (%%cexp (int -> int) "%s+2" a))
(define (+3 a)
  (%%cexp (int -> int) "%s+3" a))
(define (+4 a)
  (%%cexp (int -> int) "%s+4" a))
(define (sub1 a)
  (%%cexp (int -> int) "%s-1" a))

(define print-object
  (object:int n) -> (print n)
  (object:char ch) -> (print ch)
  (object:bool b) -> (print b)
  (object:string s) -> (print s)
  (object:undefined u) -> (print u)
  (object:symbol s) -> (print s)
  (object:continuation c) -> (print c)
  (object:closure a b c d) -> (begin (print-string "<closure>") #u)
  (object:tuple t) -> (print t)
  x -> #u
  )

(define (print-rib v)
  (print-string "(")
  (let loop ((i 0))
    (if (= i (vector-length v))
	(print-string ")")
	(begin
	  (print-object v[i])
	  (print-string " ")
	  (loop (+ i 1))))))

(define print-lenv
  (lenv:nil)                          -> (print-string "\n")
  (lenv:rib (object:tuple args) next) -> (begin (printn args) (print-lenv next))
  (lenv:rib _ _) -> (error "malformed rib")
  )

(define bt
  (vmcont:nil) -> (print-string "<top>\n")
  (vmcont:k next lenv pc saved) -> (begin (print-string " ") (print pc) (print-string "\n") (bt next))
  )

;; VM registers
(define ZED (object:int 0))
(define REGS #(ZED ZED ZED ZED ZED ZED ZED ZED ZED ZED))

(define pc 0)

(define (next-insn)
  (OPS[CODE[pc]])
  )

(define (insn-lit)
;  (printn "lit")
  (set! REGS[CODE[(+1 pc)]] LITS[CODE[(+2 pc)]])
  (set! pc (+3 pc))
  (next-insn)
  )

(define (insn-ret)
;  (printn "ret")
  (match STACK with
     (vmcont:nil)
     -> (set! RETVAL REGS[CODE[(+1 pc)]])
     (vmcont:k _ _ pc0 _)
     -> (begin
	  (set! RETVAL REGS[CODE[(+1 pc)]])
	  (set! pc pc0)
	  (next-insn))
     ))

(define (insn-add)
;  (printn "add")
  (match REGS[CODE[(+2 pc)]] REGS[CODE[(+3 pc)]] with
    (object:int x) (object:int y)
    -> (begin
	 (set! REGS[CODE[(+1 pc)]] (object:int (+ x y)))
	 (set! pc (+4 pc))
	 (next-insn))
    _ _ -> (error "insn-add")
    ))

(define (insn-sub)
;  (printn "sub")
  (match REGS[CODE[(+2 pc)]] REGS[CODE[(+3 pc)]] with
    (object:int x) (object:int y)
    -> (begin
	 (set! REGS[CODE[(+1 pc)]] (object:int (- x y)))
	 (set! pc (+4 pc))
	 (next-insn))
    _ _ -> (error "insn-sub")
    ))

(define (insn-eq)
;  (printn "eq")
  (match REGS[CODE[(+2 pc)]] REGS[CODE[(+3 pc)]] with
    (object:int x) (object:int y)
    -> (begin
	 (set! REGS[CODE[(+1 pc)]] (object:bool (= x y)))
	 (set! pc (+4 pc))
	 (next-insn))
    _ _ -> (error "insn-eq")
    ))

(define (insn-ge)
;  (printn "ge")
  (match REGS[CODE[(+2 pc)]] REGS[CODE[(+3 pc)]] with
    (object:int x) (object:int y)
    -> (begin
	 (set! REGS[CODE[(+1 pc)]] (object:bool (>= x y)))
	 (set! pc (+4 pc))
	 (next-insn))
    _ _ -> (error "insn-ge")
    ))

(define (insn-tst)
;  (printn "tst")
  ;; tst <reg> <&L0> <then-code> <jmp &L1> L0: <else-code> L1:
  ;;  0    1     2
;  (printn REGS)
;  (printn REGS[CODE[(+1 pc)]])
  ;; XXX find a way to use <if> here, matching against object:bool seems overkill
  (match REGS[CODE[(+1 pc)]] with
    (object:bool #t) -> (begin (set! pc (+3 pc)) (next-insn))
    (object:bool #f) -> (begin (set! pc CODE[(+2 pc)]) (next-insn))
    _ -> (error "insn-tst") ;; impossible
    ))

(define (insn-jmp)
;  (printn "jmp")
  ;; jmp &L0
  (set! pc CODE[(+1 pc)])
  (next-insn)
  )

(define (insn-fun)
;  (printn "fun")
  ;; FUN <target> <&L0> <body...> L0:
  (set! REGS[CODE[(+1 pc)]] (object:closure LITS CODE (+3 pc) LENV))
  (set! pc CODE[(+2 pc)])
  (next-insn)
  )

(define (insn-tail)
;  (printn "tail")
  ;; <tail> <closure-reg> <args-reg>
  ;; extend closure's environment with args, jump
  (match REGS[CODE[(+1 pc)]] with
    (object:closure lits0 code0 pc0 lenv0)
    -> (begin 
	 (set! LENV (lenv:rib REGS[CODE[(+2 pc)]] lenv0))
	 (set! LITS lits0)
	 (set! CODE code0)
	 (set! pc pc0)
	 (next-insn)
	 )
    _ -> (error "insn-tail")
    ))

(define (insn-tail0)
;  (printn "tail0")
  ;; <tail0> <closure-reg>
  (match REGS[CODE[(+1 pc)]] with
    (object:closure lits0 code0 pc0 lenv0)
    -> (begin 
	 (set! LENV lenv0)
	 (set! LITS lits0)
	 (set! CODE code0)
	 (set! pc pc0)
	 (next-insn)
	 )
    _ -> (error "insn-tail0")
    ))

(define (insn-env)
;  (printn "env")
  ;; ENV <target> <size>
  (set! REGS[CODE[(+1 pc)]] (object:tuple (%make-vector CODE[(+2 pc)] (object:int 0))))
  (set! pc (+3 pc))
  (next-insn)
  )

(define (insn-arg)
;  (printn "arg")
;  (printn REGS)
  ;; ARG <tuple-reg> <arg-reg> <index>
  ;;  0       1           2       3
  (match REGS[CODE[(+1 pc)]] with
    (object:tuple args)
    -> (begin
	 (set! args[CODE[(+3 pc)]] REGS[CODE[(+2 pc)]])
	 (set! pc (+4 pc))
;	 (printn args)
	 (next-insn))
    _ -> (error "insn-arg")
    ))

(define (insn-ref)
;  (printn "ref")
  ;; REF <target> <depth> <index>
  (let loop ((env LENV)
	     (depth CODE[(+2 pc)]))
    (match env depth with
      (lenv:rib (object:tuple vals) next) 0
      -> (begin
	   (set! REGS[CODE[(+1 pc)]] vals[CODE[(+3 pc)]])
	   (set! pc (+4 pc))
	   (next-insn))
      (lenv:rib _ next) n -> (loop next (sub1 depth))
      _ _ -> (error "insn-ref")
      )))

(define (insn-ref0)
;  (printn "ref0")
  ;; REF0 <target> <index>
  (match LENV with
    (lenv:rib (object:tuple vals) _)
    -> (begin
;	 (printn vals[CODE[(+2 pc)]])
	 (set! REGS[CODE[(+1 pc)]] vals[CODE[(+2 pc)]])
	 (set! pc (+3 pc))
	 (next-insn))
    _ -> (error "insn-ref0")
    ))

(define (insn-mov)
;  (printn "mov")
  ;; MOV <dst-ref> <src-reg>
  (set! REGS[CODE[(+1 pc)]] REGS[CODE[(+2 pc)]])
  (set! pc (+3 pc))
  (next-insn))

(define (insn-push)
;  (printn "push")
  ;; PUSH <args-reg>
  (set! LENV (lenv:rib REGS[CODE[(+ pc 1)]] LENV))
  (set! pc (+2 pc))
  (next-insn)
  )

(define (insn-trcall)
;  (printn "trcall")
  ;; TRCALL <&L0> <depth> <nregs> <reg0> <reg1> ... L0:
  ;;   0      1      2       3      4      5
  (let loop0 ((depth CODE[(+2 pc)])
	      (env LENV))
    (match env with
      (lenv:rib (object:tuple args) next)
      -> (if (> depth 0)
	     (loop0 (sub1 depth) next)
	     (let loop1 ((n CODE[(+3 pc)]))
	       (cond ((zero? n)
		      (set! LENV env)
		      (set! pc CODE[(+1 pc)])
		      (next-insn))
		     (else
		      ;;                            3    + 1 == 4
		      (set! args[(sub1 n)] REGS[CODE[(+3 (+ n pc))]])
;		      (printn args)
		      (loop1 (sub1 n))))))
      _ -> (error "insn-trcall")
      )))

(define (get-regs n)
  (let loop ((v (make-vector n (object:int 0)))
	     (i 0))
    (if (= i n)
	v
	(begin
	  (set! v[i] REGS[i])
	  (loop v (+1 i))))))

(define (insn-call)
;  (printn "call")
;  (printn REGS[CODE[(+2 pc)]])
  ;; CALL closure_reg args_reg nregs POP target
  (match REGS[CODE[(+1 pc)]] with
    (object:closure lits0 code0 pc0 lenv0)
    -> (begin
	 (set! STACK (vmcont:k STACK LENV (+4 pc) (get-regs CODE[(+3 pc)])))
	 (set! LENV (lenv:rib REGS[CODE[(+2 pc)]] lenv0))
	 (set! LITS lits0)
	 (set! CODE code0)
	 (set! pc pc0)
	 (next-insn))
    _ -> (error "call")
    ))

(define (set-regs v)
;  (print-string "restoring: ")
;  (printn v)
  (let loop ((n (sub1 (vector-length v))))
    (if (>= n 0)
	(begin
	  (set! REGS[n] v[n])
	  (loop (sub1 n)))))
  )

(define (insn-pop)
;  (printn "pop")
  (match STACK with
    (vmcont:k stack0 lenv0 pc0 regs0)
    -> (begin
	 (set-regs regs0)
	 (set! LENV lenv0)
	 ;; BIG BUG HERE, NO WAY TO RESTORE LITS or CODE
	 (set! STACK stack0)
	 (set! REGS[CODE[(+1 pc)]] RETVAL)
	 (set! pc (+2 pc))
	 (next-insn))
    _ -> (error "pop")
    ))

;; consider for CODE: a 16-bit or 32-bit untagged integer array?
;; insn data
(define CODE #(0))
;; literals
(define LITS #((object:int 0)))
;; opcodes
(define OPS
  #(insn-lit
    insn-ret
    insn-add
    insn-sub
    insn-eq
    insn-tst
    insn-jmp
    insn-fun
    insn-tail
    insn-tail0
    insn-env
    insn-arg
    insn-ref
    insn-mov
    insn-push
    insn-trcall
    insn-ref0
    insn-call
    insn-pop
    insn-ge
    ))

;; lexical env
(define LENV (lenv:nil))
;; stack
(define STACK (vmcont:nil))
;; return value from functions
(define RETVAL (object:int 0))

(let ((code (load sys:argv[1])))
  (set! CODE (list->vector code.code))
  (set! LITS (list->vector code.lits))
  (set! STACK (vmcont:nil))
  (printn OPS)
  (printn CODE)
  (printn LITS)
  (set! pc 0)
  (next-insn)
  (printn RETVAL)
  )
