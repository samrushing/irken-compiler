
(define (call/cc p)
  (let ((k (%getcc)))
    (p (lambda (r) (%putcc k r)))))

(define (error arg)
  (%printn arg))

(define (open path flags)
  (let ((fd (cexp "box (open (GET_STRING_POINTER(%s), unbox(%s)))" path flags)))
    (if (%ge? fd 0)
	fd
	(error "open() failed"))))

(define (make-string n)
  (let ((s (%make-tuple #x10 (cexp "box(string_tuple_length(unbox(%s)))" n))))
    (%set-string-len s n)
    s))

(define (copy-string s1 n)
  (let ((s2 (make-string n)))
    (cexp "box (memcpy (GET_STRING_POINTER(%s), GET_STRING_POINTER(%s), unbox(%s)))" s2 s1 n)
    s2))

(define (ascii->char n)
  (%make-immediate n #x02))

(define (char->ascii c)
  (cexp "box (GET_PAYLOAD(%s))" c))

(define (string-ref s n)
  (ascii->char (cexp "box(((pxll_string *)%s)->data[unbox(%s)])" s n)))

(define (string-set! s n c)
  (cexp "((pxll_string *)%s)->data[unbox(%s)] = GET_PAYLOAD (%s)" s n c))

(define (read fd bytes)
  (let ((buffer (make-string bytes)))
    (let ((r (cexp "box (read (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd buffer bytes)))
      (if (%== r bytes)
	  buffer
	  (if (%lt? r bytes)
	      (copy-string buffer r)
	      (error "read() failed"))))))

(define (write fd s)
  (cexp "box (write (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd s (%string-length s)))

;; vector

(define (make-vector n)
  (%make-tuple #x14 n))

(define (vector-ref v n)
  (cexp "((pxll_vector*)(%s))->val[unbox(%s)]" v n))

(define (vector-set! v n x)
  (cexp "((pxll_vector*)(%s))->val[unbox(%s)] = %s" v n x))

(define (vector-length v)
  (cexp "get_tuple_size(%s)" v))

(define (make-empty-bytecode-vector default-val)
  (let ((v (make-vector 256)))
    (let loop ((i 255))
      (vector-set! v i default-val)
      (if (%zero? i)
	  v
	  (loop (%- i 1))))))

(call/cc
 (lambda (exit)
   (set! error exit)

   (define (vm)

     (let ((lenv #f)
	   (pc 0)
	   (regs (make-vector 10)) ;; registers
	   (code #f)
	   (opcodes (make-vector 2))
	   )
       ;; opcode functions
       (define (op-literal)
	 ;; ... <LIT> <value> <target> ...
	 (%printn "<LITERAL>")
	 (vector-set! regs (vector-ref code (%+ pc 2)) (vector-ref code (%+ pc 1)))
	 (set! pc (%+ pc 3))
	 #f
	 )
	 
       (define (op-return)
	 ;; ... <RETURN> <target>
	 (%printn "<RETURN>")
	 (%printn regs)
	 (exit (vector-ref regs (vector-ref code (%+ pc 1)))))
	 ;;(let ((result (vector-ref code (%+ pc 1))))
	 ;;(%print "result= ")
	 ;;(%printn result)
	 ;;(exit result))

       (define (populate-opcode-vector)
	 (vector-set! opcodes 0 op-literal)
	 (vector-set! opcodes 1 op-return)
	 )

       ;; VM main loop
       (define (loop)
	 (%printn pc)
	 ;; fetch and execute next insn
	 ((vector-ref code pc))
	 (loop))

       (define (make-test-code)
	 (let ((tcode (make-vector 5)))
	   (vector-set! tcode 0 op-literal)
	   (vector-set! tcode 1 3141)
	   (vector-set! tcode 2 0)
	   (vector-set! tcode 3 op-return)
	   (vector-set! tcode 4 0)
	   tcode
	   ))

       (define (test)
	 (set! code (make-test-code))
	 (set! pc 0)
	 (%printn code)
	 (loop))
       
       (test)
       )
     )
   (vm)
   ))


