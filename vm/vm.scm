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
	      (if (= len 0)
		  n
		  (loop (+ (<< n 8) (char->ascii (next))) (- len 1))))
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

;; performance hacks
(define (+1 a)
  (%%cexp (int -> int) "%s+1" a))
(define (+2 a)
  (%%cexp (int -> int) "%s+2" a))
(define (+3 a)
  (%%cexp (int -> int) "%s+3" a))
(define (+4 a)
  (%%cexp (int -> int) "%s+4" a))
;;(define (-1 a)
;;  (%%cexp (int -> int) "%s-1" a))

;; VM registers
(define ZED (object:int 0))
(define regs #(ZED ZED ZED ZED ZED ZED ZED ZED ZED ZED))

(define pc 0)

(define (next-insn)
  (OPS[CODE[pc]])
  )

(define (insn-lit)
  (printn "lit")
  (set! regs[CODE[(+1 pc)]] LITS[CODE[(+2 pc)]])
  (set! pc (+3 pc))
  (next-insn)
  )

(define (insn-ret)
  (printn "ret")
  (printn regs[CODE[(+1 pc)]])
  regs[CODE[(+1 pc)]]
  )

(define (insn-add)
  (printn "add")
  (match regs[CODE[(+2 pc)]] regs[CODE[(+3 pc)]] with
    (object:int x) (object:int y) -> (begin (set! regs[CODE[(+1 pc)]] (object:int (+ x y)))
					    (set! pc (+4 pc))
					    (next-insn))
    _ _ -> (error "insn-plus")
    ))

(define (insn-sub)
  (printn "sub")
  (match regs[CODE[(+2 pc)]] regs[CODE[(+3 pc)]] with
    (object:int x) (object:int y) -> (begin (set! regs[CODE[(+1 pc)]] (object:int (- x y)))
					    (set! pc (+4 pc))
					    (next-insn))
    _ _ -> (error "insn-minus")
    ))

(define (insn-eq)
  (printn "eq")
  (match regs[CODE[(+2 pc)]] regs[CODE[(+3 pc)]] with
    (object:int x) (object:int y) -> (begin (set! regs[CODE[(+1 pc)]] (object:bool (= x y)))
					    (set! pc (+4 pc))
					    (next-insn))
    _ _ -> (error "insn-equals")
    ))

(define (insn-tst)
  ;; tst <reg> <size> <then-code> <jmp> <else-code>
  (printn "tst")
  (printn regs)
  (printn regs[CODE[(+1 pc)]])
  ;; XXX find a way to use <if> here, matching against object:bool seems overkill
  (match regs[CODE[(+1 pc)]] with
    (object:bool #t) -> (begin (set! pc (+3 pc)) (next-insn))
    (object:bool #f) -> (begin (set! pc (+3 (+ pc CODE[(+2 pc)]))) (next-insn))
    _ -> (error "insn-tst") ;; impossible
    ))

(define (insn-jmp)
  (printn "jmp")
  (set! pc (+2 (+ pc CODE[(+1 pc)])))
  (next-insn)
  )

(define (insn-fun)
  (printn "fun")
  ;; <FUN> <target> <size> <body...>
  (set! regs[CODE[(+1 pc)]] (object:closure LITS CODE (+3 pc) LENV))
  (set! pc (+3 (+ pc CODE[(+2 pc)])))
  (next-insn)
  )

(define (insn-tail)
  (printn "tail")
  ;; <tail> <closure-reg> <args-reg>
  ;; extend closure's environment with args, jump
  (match regs[CODE[(+1 pc)]] with
    (object:closure lits0 code0 pc0 lenv0)
    -> (begin 
	 (set! LENV (lenv:rib regs[CODE[(+2 pc)]] lenv0))
	 (set! LITS lits0)
	 (set! CODE code0)
	 (set! pc pc0)
	 (next-insn)
	 )
    _ -> (error "insn-tail")
    ))

(define (insn-tail0)
  (printn "tail0")
  ;; <tail> <closure-reg> <args-reg>
  (match regs[CODE[(+1 pc)]] with
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
  (printn "env")
  (set! regs[CODE[(+1 pc)]] (object:tuple (%make-vector CODE[(+2 pc)] (object:int 0))))
  (set! pc (+3 pc))
  (next-insn)
  )

(define (insn-arg)
  (printn "arg")
  (printn regs)
  (match regs[CODE[(+1 pc)]] with
    (object:tuple args)
    -> (begin (set! args[CODE[(+3 pc)]] regs[CODE[(+2 pc)]])
	      (set! pc (+4 pc))
	      (printn args)
	      (next-insn))
    _ -> (error "insn-arg")
    ))

(define (insn-ref)
  (printn "ref")
  (let loop ((env LENV)
	     (depth CODE[(+2 pc)]))
    (match env depth with
      (lenv:rib (object:tuple vals) next) 0
      -> (begin
	   (set! regs[CODE[(+1 pc)]] vals[CODE[(+3 pc)]])
	   (set! pc (+4 pc))
	   (next-insn))
      (lenv:rib _ next) n -> (loop next (- depth 1))
      _ _ -> (error "insn-ref")
      )))

(define (insn-mov)
  (set! regs[CODE[(+1 pc)]] regs[CODE[(+2 pc)]])
  (set! pc (+3 pc))
  (next-insn))

(define (insn-push)
  (set! LENV (lenv:rib regs[CODE[(+ pc 1)]] LENV))
  (set! pc (+2 pc))
  (next-insn)
  )

;; (define (insn-trcall)
;;   (let loop0 ((depth CODE[(+1 pc)])
;; 	      (env LENV))
;;     (match env with
;;       (lenv:rib args next)
;;       -> (if (> depth 0)
;; 	     (loop0 (- depth 1) next)
;; 	     (let loop1 ((n CODE[(+2 pc)]))
;; 	       (cond ((< n 0)
;; 		      ;; XXX figure out how to set <pc> here!
;; 		      (set! LENV env)
;; 		      (next-insn))
;; 		     (else
;; 		      (set! args[(-1 n)] regs[CODE[(+ 2 (+ n pc))]])
;; 		      (loop1 (-1 n))))))
;;       _ -> (error "insn-trcall")
;;       )))

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
    ))

;; lexical env
(define LENV (lenv:nil))

(let ((code (load sys:argv[1])))
  (set! CODE (list->vector code.code))
  (set! LITS (list->vector code.lits))
  (printn OPS)
  (printn CODE)
  (printn LITS)
  (set! pc 0)
  (next-insn)
  (printn regs)
  )
