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
  (:closure (vector object) (vector int) int lenv) ;; add <name>
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

;; (define (print-rib v)
;;   (print-string "(")
;;   (let loop ((i 0))
;;     (if (= i (vector-length v))
;; 	(print-string ")")
;; 	(begin
;; 	  (print-object v[i])
;; 	  (print-string " ")
;; 	  (loop (+ i 1))))))

;; (define print-lenv
;;   (lenv:nil)                          -> (print-string "\n")
;;   (lenv:rib (object:tuple args) next) -> (begin (printn args) (print-lenv next))
;;   (lenv:rib _ _) -> (error "malformed rib")
;;   )

(define bt
  (vmcont:nil) -> (print-string "<top>\n")
  (vmcont:k next lenv pc saved) -> (begin (print-string " ") (print pc) (print-string "\n") (bt next))
  )

;; XXX: if we accidentally leave a large object in a register,
;;  gc will continue to copy it. Think about how/when to clear
;;  the register set.

;; VM registers

(define REGS (make-vector 20 (object:int 0)))

(define pc 0)

(datatype opcode
  (:t string int (-> undefined))
  )
  
(define (get-args n)
  (let ((r '()))
    (for-range i n
      (push! r CODE[(+1 (+ pc i))]))
    (reverse r)))

(define (print-lenv)
  (printf "[")
  (let loop ((lenv LENV))
    (match lenv with
      (lenv:rib (object:tuple vals) next)
      -> (begin 
           (printf "{")
           (for-range i (vector-length vals)
             (printf (object-repr vals[i]) " "))
           (printf "} ")
           (loop next))
      (lenv:nil)
      -> (printf "]\n")
      _ -> (vm-error)
      )
    ))

(define (print-regs)
  (printf "[")
  (for-range i (vector-length REGS)
    (printf (object-repr REGS[i]) " "))
  (printf "]\n"))

(define (vm-error)
  (let ((opcode CODE[pc])
	(info opcode-info[opcode]))
    (match info with
      (opcode:t name nargs _)
      -> (begin
           (printf "------------***\n"
                   "Error in VM:\n"
                   (int pc)
                   " " name "\n")
           (print-regs)
           ;; (print-args nargs)
           #u
           ))))

;; the tracing version
(define (next-insn0)
  (let ((opcode CODE[pc])
	(info opcode-info[opcode]))
    (match info with
       (opcode:t name nargs _)
       -> (begin
            (print-lenv)
            (print-regs)
            (printf (lpad 4 (int pc)) 
                    "  " (rpad 8 name)
                    " " (join int->string " " (get-args nargs))
                    "\n")
            (OPS[opcode]))
      )))

;; the normal version
(define (next-insn)
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
  ;; PRINT <target> <arg>
  ;; (target is ignored)
  (printf (object-repr REGS[CODE[(+2 pc)]]) "\n")
  (set! pc (+3 pc))
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

(define (insn-set)
  ;; SET <depth> <index> <value>
  (let loop ((env LENV)
	     (depth CODE[(+1 pc)]))
    (match env depth with
      (lenv:rib (object:tuple vals) next) 0
      -> (begin
	   (set! vals[CODE[(+2 pc)]] REGS[CODE[(+3 pc)]])
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
  (next-insn)
  )

(define (insn-epush)
  ;; EPUSH <args-reg>
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
  ;; CALL closure_reg args_reg nregs
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

;; just like insn-pop, just no target.
(define (insn-pop0)
  (match STACK with
    (vmcont:k stack0 lenv0 pc0 regs0)
    -> (begin
	 (set-regs regs0)
	 (set! LENV lenv0)
	 (set! STACK stack0)
	 (set! pc (+1 pc))
	 (next-insn))
    _ -> (vm-error)
    ))

(define (insn-topis)
  ;; TOPIS <env>
  (set! TOP REGS[CODE[(+1 pc)]])
  (set! pc (+2 pc))
  (next-insn))

(define (insn-topref)
  ;; TOPREF target index
  (match TOP with
    (object:tuple vals)
    -> (begin
         (set! REGS[CODE[(+1 pc)]] vals[CODE[(+2 pc)]])
         (set! pc (+3 pc))
         (next-insn))
    _ -> (vm-error)
    ))

(define (insn-topset)
  ;; TOPSET index val
  (match TOP with
    (object:tuple vals)
    -> (begin
         (set! vals[CODE[(+1 pc)]] REGS[CODE[(+2 pc)]])
         (set! pc (+3 pc))
         (next-insn))
    _ -> (vm-error)
    ))

(define (insn-epop)
  ;; EPOP
  (match LENV with
    (lenv:rib _ next)
    -> (begin 
         (set! LENV next)
         (set! pc (+1 pc))
         (next-insn))
    _ -> (vm-error)
    ))

;; XXX not sure this is a good idea.  it will make calls
;;   to next-insn an 'unknown' function, which probably 
;;   slows things down too much.
(define (insn-tron) 
  (let ((tmp next-insn))
    (set! next-insn next-insn0)
    (set! next-insn0 tmp)
    (next-insn)
    ))

(define (insn-gc)
  ;; no-op
  (next-insn)
  )

;; insn data
(define CODE (list->vector '(0)))
;; literals
(define LITS #((object:int 0)))
;; opcodes

(define opcode-info
  (list->vector
   (LIST
    (opcode:t "lit"     2 insn-lit)
    (opcode:t "ret"     1 insn-ret)
    (opcode:t "add"     3 insn-add)
    (opcode:t "sub"     3 insn-sub)
    (opcode:t "eq"      3 insn-eq)
    (opcode:t "lt"      3 insn-lt)
    (opcode:t "gt"      3 insn-gt)
    (opcode:t "le"      3 insn-le)
    (opcode:t "ge"      3 insn-ge)
    (opcode:t "tst"     2 insn-tst)
    (opcode:t "jmp"     1 insn-jmp)
    (opcode:t "fun"     2 insn-fun)
    (opcode:t "tail"    2 insn-tail)
    (opcode:t "tail0"   1 insn-tail0)
    (opcode:t "env"     2 insn-env)
    (opcode:t "arg"     3 insn-arg)
    (opcode:t "ref"     3 insn-ref)
    (opcode:t "mov"     2 insn-mov)
    (opcode:t "epush"   1 insn-epush)
    (opcode:t "trcall"  3 insn-trcall)
    (opcode:t "ref0"    2 insn-ref0)
    (opcode:t "call"    3 insn-call)
    (opcode:t "pop"     1 insn-pop)
    (opcode:t "print"   2 insn-print)
    (opcode:t "topis"   1 insn-topis)
    (opcode:t "topref"  2 insn-topref)
    (opcode:t "topset"  2 insn-topset)
    (opcode:t "set"     3 insn-set)
    (opcode:t "pop0"    0 insn-pop0)
    (opcode:t "epop"    0 insn-epop)
    (opcode:t "tron"    0 insn-tron)
    (opcode:t "troff"   0 insn-tron)
    (opcode:t "gc"      0 insn-gc)
    ;; we stop here, not going to bother doing datatypes...
    )))

(define OPS
  (let ((nops (vector-length opcode-info))
        (v (make-vector nops insn-lit)))
    (for-range i nops
      (match opcode-info[i] with
        (opcode:t name nargs fun) -> (set! v[i] fun)
        ))
    v))

;; lexical env
(define LENV (lenv:nil))
(define TOP (object:tuple #()))

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
    (printf "result: " (object-repr RETVAL) "\n")
    ))

(test)
