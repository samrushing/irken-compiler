;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/os.scm")

(datatype object
  (:int int)
  (:char char)
  (:bool bool)
  (:string string)
  (:undefined)
  (:symbol symbol)
  (:closure (vector object) (vector int) int lenv)
  (:tuple (vector object))
  )

(define object-repr
  (object:int n)     -> (format (int n))
  (object:char n)    -> (format "#\\" (char n)) ;; note: printable/non-printable, newline, etc...
  (object:bool b)    -> (format (bool b))
  (object:string s)  -> (format (string s))
  (object:undefined) -> "#u"
  (object:symbol s)  -> (format (sym s))
  (object:closure lits code pc lenv)
  -> (format "<closure>") ;; tbd
  (object:tuple vals)
  -> (format "{" (join object-repr " " (vector->list vals)) "}")
  )

(datatype lenv
  (:nil)
  (:rib object lenv)
  )

;; probably need a place to store LITS and CODE... perhaps
;; another vmcont type that contains them, to be used only
;; when calling an unknown function.
(datatype vmcont
  (:nil)
  (:k vmcont lenv int (vector object))
  )

;; XXX need a way to declare record types!
;;(datatype code
;;  ;; <lits> <insns>
;;  (:t (vector object) (vector int))
;;  )

(define (load-machine path)

  (let ((f (file/open-read path))
	(code (file/read-buffer f))
	(pc 0)
	(r '())
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
	    (let loop ((n 0)
                       (len (char->ascii (next))))
	      (if (zero? len)
		  n
		  (loop (+ (<< n 8) (char->ascii (next)))
                        (sub1 len))))
	    n)))

    (let ((lits
	   ;; read vector of literals
	   (let loop ((lits '()))
	     (match (next) with
	       #\+ -> (loop (cons (object:int (read-int)) lits))
	       #\- -> (loop (cons (object:int (- 0 (read-int))) lits))
	       #\T -> (loop (cons (object:bool #t) lits))
	       #\F -> (loop (cons (object:bool #f) lits))
               #\u -> (loop (cons (object:undefined) lits))
               #\c -> (loop (cons (object:char (ascii->char (read-int))) lits))
	       #\. -> (reverse lits)
	       _ -> (error "reading lits")
	       ))))

      (let loop ((stream '()))
	(if (= pc (string-length code))
	    { code = (reverse stream) lits = lits }
	    (loop (cons (read-int) stream))))
      )))

(define (+1 a) (+ 1 a))
(define (+2 a) (+ 2 a))
(define (+3 a) (+ 3 a))
(define (+4 a) (+ 4 a))
(define (sub1 a) (- a 1))

(define print-object
  (object:int n)           -> (printf (int n))
  (object:char ch)         -> (printf (char ch))
  (object:bool b)          -> (printf (bool b))
  (object:string s)        -> (printf (string s))
  (object:undefined)       -> (printf "#u")
  (object:symbol s)        -> (printf (sym s))
  (object:closure a b c d) -> (begin (print-string "<closure>") #u)
  (object:tuple t)         -> (print t)
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
(define REGS (make-vector 10 (object:int 0)))

(define pc 0)

(datatype opcode
  (:t string int))

(define (print-args n)
  (print-string "\t")
  (let loop ((i 0))
    (if (< i n)
	(begin
	  (print CODE[(+1 (+ pc i))])
	  (print-string " ")
	  (loop (+1 i))
	  )))
  (print-string "\n")
  )

;; the tracing version
(define (next-insn)
  (let ((opcode CODE[pc])
	(info opcode-info[opcode]))
    (match info with
       (opcode:t name nargs)
       -> (begin
	    (print pc)
	    (print-string "\t" )
	    (print-string name)
	    (print-args nargs)
	    (OPS[opcode])))))

(define (vm-error)
  (let ((opcode CODE[pc])
	(info opcode-info[opcode]))
    (match info with
      (opcode:t name nargs)
      -> (begin
           (printf "------------***\n"
                   "Error in VM:\n"
                   (int pc)
                   "\t"
                   name)
           (print-args nargs)
           #u
           ))))

;; the normal version
(define (next-insn0)
  (OPS[CODE[pc]])
  )

(define (insn-lit)
  ;; <LIT> <target> <litnum>
  (set! REGS[CODE[(+1 pc)]] LITS[CODE[(+2 pc)]])
  (set! pc (+3 pc))
  (next-insn)
  )

(define (insn-ret)
  (match STACK with
     (vmcont:nil)
     -> (set! RETVAL REGS[CODE[(+1 pc)]])
     (vmcont:k _ _ pc0 _)
     -> (begin
	  (set! RETVAL REGS[CODE[(+1 pc)]])
	  (set! pc pc0)
	  (next-insn))
     ))

(defmacro binop
  (binop op rtype)
  -> (lambda ()
       (match REGS[CODE[(+2 pc)]] REGS[CODE[(+3 pc)]] with
         (object:int x) (object:int y)
         -> (begin
              (set! REGS[CODE[(+1 pc)]] (rtype (op x y)))
              (set! pc (+4 pc))
              (next-insn))
         _ _ -> (vm-error)
         ))
  )

(define insn-add (binop + object:int))
(define insn-sub (binop - object:int))
(define insn-eq  (binop = object:bool))
(define insn-lt  (binop < object:bool))
(define insn-gt  (binop > object:bool))
(define insn-le  (binop <= object:bool))
(define insn-ge  (binop >= object:bool))

(define (insn-print)
  (printf (object-repr REGS[CODE[(+1 pc)]]))
  (set! pc (+2 pc))
  (next-insn))

(define (insn-tst)
  ;; tst <reg> <&L0> <then-code> <jmp &L1> L0: <else-code> L1:
  ;;  0    1     2
  (match REGS[CODE[(+1 pc)]] with
    (object:bool #t) -> (begin (set! pc (+3 pc)) (next-insn))
    (object:bool #f) -> (begin (set! pc CODE[(+2 pc)]) (next-insn))
    _ -> (vm-error)
    ))

(define (insn-jmp)
  ;; jmp &L0
  (set! pc CODE[(+1 pc)])
  (next-insn)
  )

(define (insn-fun)
  ;; FUN <target> <&L0> <body...> L0:
  (set! REGS[CODE[(+1 pc)]] (object:closure LITS CODE (+3 pc) LENV))
  (set! pc CODE[(+2 pc)])
  (next-insn)
  )

(define (insn-tail)
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
    _ -> (vm-error)
    ))

(define (insn-tail0)
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
    _ -> (vm-error)
    ))

(define (insn-env)
  ;; ENV <target> <size>
  (set! REGS[CODE[(+1 pc)]] (object:tuple (make-vector CODE[(+2 pc)] (object:int 0))))
  (set! pc (+3 pc))
  (next-insn)
  )

(define (insn-arg)
  ;; ARG <tuple-reg> <arg-reg> <index>
  ;;  0       1           2       3
  (match REGS[CODE[(+1 pc)]] with
    (object:tuple args)
    -> (begin
	 (set! args[CODE[(+3 pc)]] REGS[CODE[(+2 pc)]])
	 (set! pc (+4 pc))
;	 (printn args)
	 (next-insn))
    _ -> (vm-error)
    ))

(define (insn-ref)
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
      _ _ -> (vm-error)
      )))

(define (insn-ref0)
  ;; REF0 <target> <index>
  (match LENV with
    (lenv:rib (object:tuple vals) _)
    -> (begin
	 (set! REGS[CODE[(+1 pc)]] vals[CODE[(+2 pc)]])
	 (set! pc (+3 pc))
	 (next-insn))
    _ -> (vm-error)
    ))

(define (insn-mov)
  ;; MOV <dst-ref> <src-reg>
  (set! REGS[CODE[(+1 pc)]] REGS[CODE[(+2 pc)]])
  (set! pc (+3 pc))
  (next-insn))

(define (insn-push)
  ;; PUSH <args-reg>
  (set! LENV (lenv:rib REGS[CODE[(+ pc 1)]] LENV))
  (set! pc (+2 pc))
  (next-insn)
  )

(define (insn-trcall)
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
		      (loop1 (sub1 n))))))
      _ -> (vm-error)
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
    _ -> (vm-error)
    ))

(define (set-regs v)
  (let loop ((n (sub1 (vector-length v))))
    (if (>= n 0)
	(begin
	  (set! REGS[n] v[n])
	  (loop (sub1 n)))))
  )

(define (insn-pop)
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
    _ -> (vm-error)
    ))

;; insn data
(define CODE (list->vector '(0)))
;; literals
(define LITS #((object:int 0)))
;; opcodes
(define OPS
  (list->vector
   (LIST
    insn-lit
    insn-ret
    insn-add
    insn-sub
    insn-eq
    insn-lt
    insn-gt
    insn-ge
    insn-le
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
    insn-print ;; prim
    )))

(defmacro OI
  (OI s n) -> (opcode:t s n)
  )

(define opcode-info
  #((OI "lit" 2)
    (OI "ret" 1)
    (OI "add" 3)
    (OI "sub" 3)
    (OI "eq" 3)
    (OI "lt" 3)
    (OI "gt" 3)
    (OI "le" 3)
    (OI "ge" 3)
    (OI "tst" 2)
    (OI "jmp" 1)
    (OI "fun" 2)
    (OI "tail" 2)
    (OI "tail0" 1)
    (OI "env" 2)
    (OI "arg" 3)
    (OI "ref" 3)
    (OI "mov" 2)
    (OI "push" 1)
    (OI "trcall" 3)
    (OI "ref0" 2)
    (OI "call" 3)
    (OI "pop" 1)
    (OI "print" 1)
    ))

;; lexical env
(define LENV (lenv:nil))
;; stack
(define STACK (vmcont:nil))
;; return value from functions
(define RETVAL (object:int 0))

(define (test)
  (let ((code (load-machine sys.argv[1])))
    (set! CODE (list->vector code.code))
    (set! LITS (list->vector code.lits))
    (set! STACK (vmcont:nil))
    ;;(printn OPS)
    ;;(printn CODE)
    ;;(printn LITS)
    (set! pc 0)
    (next-insn)
    (printf (object-repr RETVAL) "\n")
    ))

(test)
