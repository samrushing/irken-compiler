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
  ;; (:closure ...)
  ;; (:lenv ...)
  )

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
  (printn regs)
  (next-insn))

(define (insn-ret)
  (printn "ret")
  (printn pc)
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
  (match regs[CODE[(+1 pc)]] with
    (object:bool #t) -> (begin (set! pc (+3 pc)) (next-insn))
    (object:bool #f) -> (begin (set! pc (+3 (+ pc CODE[(+2 pc)]))) (next-insn))
    _ -> (error "insn-tst") ;; impossible
    ))

(define (insn-jmp)
  (printn "jmp")
  (match regs[CODE[(+1 pc)]] with
    (object:int offset) -> (begin (set! pc (+ (+2 pc) offset)) (next-insn))
    _ -> (error "insn-jmp") ;; impossible
    ))

;; consider for CODE: a 16-bit or 32-bit untagged integer array?
;; insn data
(define CODE #(0))
;; literals
(define LITS #((object:int 0)))
;; opcodes
(define OPS #(insn-lit insn-ret insn-add insn-sub insn-eq insn-tst insn-jmp))

(let ((code (load sys:argv[1])))
  (set! CODE (list->vector code.code))
  (set! LITS (list->vector code.lits))
  (printn OPS)
  (printn CODE)
  (printn LITS)
  (set! pc 0)
  (next-insn)
  )
