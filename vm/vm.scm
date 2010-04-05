;; -*- Mode: Scheme -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")

(define (unbox x)
  ;; type-unsafe - allows us to 'cast' a literal so its hidden
  ;;   run-time type is revealed.
  (%%cexp (int -> 'a) "/*cast*/(object*)%s" x))

(datatype insn-stream
  (:t (list (-> int)) (list int)))

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

    (define (read-lit)
      (let loop ((n 0) (len (char->ascii (next))))
	(if (= len 0)
	    n
	    (loop (+ (<< n 8) (char->ascii (next))) (- len 1)))))

    (let loop ((insns (NIL))
	       (data (NIL)))
      (match (next) with
	#\L ;; literal
	-> (let ((target (nexti))
		 (value (unbox (read-lit))))
	     (loop (CONS insn-literal insns)
		   (CONS value (CONS target data))))

	#\R ;; return
	-> (insn-stream:t (reverse (CONS insn-return insns)) (reverse (CONS (nexti) data)))

	#\+ ;; primop plus
	-> (let ((target (nexti))
		 (r0 (nexti))
		 (r1 (nexti)))
	     (loop (CONS insn-plus insns)
		   (CONS r1 (CONS r0 (CONS target data)))))

	#\- ;; primop minus
	-> (let ((target (nexti))
		 (r0 (nexti))
		 (r1 (nexti)))
	     (loop (CONS insn-minus insns)
		   (CONS r1 (CONS r0 (CONS target data)))))

	#\= ;; primop equals
	-> (let ((target (nexti))
		 (r0 (nexti))
		 (r1 (nexti)))
	     (loop (CONS insn-equals insns)
		   (CONS r1 (CONS r0 (CONS target data)))))

	_ -> (error "bytecode")

	))
    ))

;; insn thunks
(define vm-insns #(insn-return))
;; insn data
(define vm-data #(0))

;; VM registers
(define regs #(0 0 0 0 0 0 0 0 0 0))

(define pci 0)
(define pcd 0)

(define (next-insn)
  (set! pci (+ pci 1))
  vm-insns[pci]
  )

(define (next-data)
  (let ((r vm-data[pcd]))
    (set! pcd (+ pcd 1))
    r))

(define (insn-literal)
  (set! regs[vm-data[pcd]] vm-data[(+ pcd 1)])
  (set! pcd (+ pcd 2))
  ((next-insn)))

(define (insn-return)
  regs[vm-data[pcd]]
  )

(define (insn-plus)
  (set! regs[vm-data[pcd]] (+ regs[vm-data[(+ pcd 1)]] regs[vm-data[(+ pcd 2)]]))
  (set! pcd (+ pcd 3))
  ((next-insn)))

(define (insn-minus)
  (set! regs[vm-data[pcd]] (- regs[vm-data[(+ pcd 1)]] regs[vm-data[(+ pcd 2)]]))
  (set! pcd (+ pcd 3))
  ((next-insn)))

(define (insn-equals)
  (set! regs[vm-data[pcd]] (= regs[vm-data[(+ pcd 1)]] regs[vm-data[(+ pcd 2)]]))
  (set! pcd (+ pcd 3))
  ((next-insn)))

(match (load sys:argv[1]) with
  (insn-stream:t insns data)
  -> (begin
       (set! vm-insns (list->vector insns))
       (set! vm-data (list->vector data))
       (printn vm-insns)
       (printn vm-data)
       (set! pci 0)
       (set! pcd 0)
       (vm-insns[0])
       )
  )




	     
	     
	       
  
