;; -*- Mode: Irken -*-

(define (print x)
  (%backend c (%%cexp ('a -> undefined) "dump_object (%0, 0)" x))
  (%backend llvm (%llvm-call ("@irk_dump_object" ('a -> undefined) ccc) x))
  (%backend (arm amd rv) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
  (%backend bytecode (%%vminsn ('a -> undefined) 'printo x))
  )

(define (printn x)
  (print x)
  (newline))

;; note: discards return value.
(define (print-string s)
  (%backend c (%%cexp (string int -> int) "fwrite (%0, 1, %1, stdout)" s (string-length s)) #u)
  (%backend llvm (%llvm-call ("@irk_write_stdout" (string int -> int) ccc) s (string-length s)) #u)
  (%backend (arm amd rv) (%asm-c-call ("irk_write_stdout" (string int -> int) #f) s (string-length s)) #u)
  (%backend bytecode (%%vminsn (string -> int) 'prints s) #u)
  )

(define (flush)
  (%backend c (%%cexp (-> int) "fflush (stdout)"))
  (%backend llvm (%llvm-call ("@irk_flush" (-> int) ccc)))
  (%backend (arm amd rv) (%asm-c-call ("irk_flush" (-> int))))
  ;; not using stdio
  (%backend bytecode #u))

(define (newline)
  (%backend c (%%cexp (char -> int) "fputc (GET_CHAR(%0), stdout)" #\newline))
  ;; note: these are both 'wrong', in the sense that irk_putc() assumes an irken
  ;;   character as input, which relies on argument frobbing *not* being applied to characters.
  (%backend llvm (%llvm-call ("@irk_putc" (char -> undefined) ccc) #\newline))
  (%backend (arm amd rv) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
  (%backend bytecode (print-string "\n")))

(define (= a b)
  (%backend c (%%cexp (int int -> bool) "%0==%1" a b))
  (%backend (llvm arm amd rv) (%llicmp eq a b))
  (%backend bytecode (%%vminsn (int int -> bool) eq a b)))

(define (zero? a)
  (%backend c (%%cexp (int -> bool) "%0==0" a))
  (%backend (bytecode llvm arm amd rv) (= a 0)))

(define (< a b)
  (%backend c (%%cexp (int int -> bool) "%0<%1" a b))
  (%backend (llvm arm amd rv) (%llicmp slt a b))
  (%backend bytecode (%%vminsn (int int -> bool) 'lt a b))
  )

(define (<= a b)
  (%backend c (%%cexp (int int -> bool) "%0<=%1" a b))
  (%backend (llvm arm amd rv) (%llicmp sle a b))
  (%backend bytecode (%%vminsn (int int -> bool) 'le a b))
  )

(define (> a b)
  (%backend c (%%cexp (int int -> bool) "%0>%1" a b))
  (%backend (llvm arm amd rv) (%llicmp sgt a b))
  (%backend bytecode (%%vminsn (int int -> bool) 'gt a b))
  )

(define (>= a b)
  (%backend c (%%cexp (int int -> bool) "%0>=%1" a b))
  (%backend (llvm arm amd rv) (%llicmp sge a b))
  (%backend bytecode (%%vminsn (int int -> bool) 'ge a b))
  )

(define (>0 a)
  (%backend c (%%cexp (int -> bool) "%0>0" a))
  (%backend (llvm arm amd rv) (%llicmp sgt a 0))
  (%backend bytecode (%%vminsn (int int -> bool) 'gt a 0))
  )

(define (<0 a)
  (%backend c (%%cexp (int -> bool) "%0<0" a))
  (%backend (llvm arm amd rv) (%llicmp slt a 0))
  (%backend bytecode (%%vminsn (int int -> bool) 'lt a 0))
  )

(define (binary+ a b)
  (%backend c (%%cexp (int int -> int) "%0+%1" a b))
  (%backend (llvm arm amd rv) (%llarith add a b))
  (%backend bytecode (%%vminsn (int int -> int) 'add a b))
  )

(define (binary- a b)
  (%backend c (%%cexp (int int -> int) "%0-%1" a b))
  (%backend (llvm arm amd rv) (%llarith sub a b))
  (%backend bytecode (%%vminsn (int int -> int) 'sub a b))
  )

(define (binary* a b)
  (%backend c (%%cexp (int int -> int) "%0*%1" a b))
  (%backend (llvm arm amd rv) (%llarith mul a b))
  (%backend bytecode (%%vminsn (int int -> int) 'mul a b))
  )

(defmacro +
  (+ x)       -> x
  (+ a b ...) -> (binary+ a (+ b ...))
  )

(defmacro -
  (- a)         -> (binary- 0 a)
  (- a b)	-> (binary- a b)
  (- a b c ...) -> (binary- a (+ b c ...))
  )

(defmacro *
  (* x) -> x
  (* a b ...) -> (binary* a (* b ...))
  )

;; these implement the C99/C11+ truncated division/remainder operation.
(define (unsafe-div a b)
  (%backend c (%%cexp (int int -> int) "%0 / %1" a b))
  (%backend (llvm arm rv) (%llarith sdiv a b))
  (%backend bytecode (%%vminsn (int int -> int) 'div a b))
  (%backend amd
    (match (%divmod #f a b) with
      (:tuple q r) -> q
      ))
  )

(define (unsafe-rem a b)
  (%backend c (%%cexp (int int -> int) "%0 %% %1" a b))
  (%backend (llvm arm rv) (%llarith srem a b)) ;; arm64 actually udiv/msub
  (%backend bytecode (%%vminsn (int int -> int) 'srem a b))
  (%backend amd
    (match (%divmod #f a b) with
      (:tuple q r) -> r
      ))
  )

;; See https://en.wikipedia.org/wiki/Modulo for details
;; this code is from Daan Leijen, "Division and Modulus for Computer Scientists"
;; https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf

(define (divmod-truncated n d)
  (%backend (arm amd rv) (%divmod #f n d)) ;; llvm probably also has a divmod prim
  (%backend (c llvm bytecode)
    (if (zero? d)
        (raise (:DivideByZero n))
        (:tuple (unsafe-div n d) (unsafe-rem n d)))))

;; this matches R6RS div-and-mod
(define (divmod-euclidean n d)
  (if (zero? d)
      (raise (:DivideByZero n))
      (let ((q (unsafe-div n d))
            (r (unsafe-rem n d)))
        (match (int-cmp r 0) (int-cmp d 0) with
          (cmp:<) (cmp:>) -> (:tuple (- q 1) (+ r d))
          (cmp:<) _       -> (:tuple (+ q 1) (- r d))
          _       _       -> (:tuple q r)
          ))))

(define (divmod-floored n d)
  (if (zero? d)
      (raise (:DivideByZero n))
      (let ((q (unsafe-div n d))
            (r (unsafe-rem n d)))
        (match (int-cmp r 0) (int-cmp d 0) with
          (cmp:>) (cmp:<) -> (:tuple (- q 1) (+ r d))
          (cmp:<) (cmp:>) -> (:tuple (- q 1) (+ r d))
          _       _       -> (:tuple q r)
          ))))

(define divmod divmod-euclidean)

(define (div-euclidean n d)
  (let (((q r) (divmod-euclidean n d)))
    q))

(define (mod-euclidean n d)
  (let (((q r) (divmod-euclidean n d)))
    r))

(define (div-floored n d)
  (let (((q r) (divmod-floored n d)))
    q))

(define (mod-floored n d)
  (let (((q r) (divmod-floored n d)))
    r))

;; scheme < R6RS, truncated
(define (remainder n d)
  (if (zero? d)
      (raise (:DivideByZero n))
      (unsafe-rem n d)))

;; scheme < R6R6, floored
(define modulo mod-floored)

;; let's call this Irken's decision.
(define /   div-euclidean)
(define mod mod-euclidean)

(define (<< a b)
  (%backend c (%%cexp (int int -> int) "%0<<%1" a b))
  (%backend (llvm arm rv) (%llarith shl a b))
  (%backend bytecode (%%vminsn (int int -> int) 'shl a b))
  (%backend amd ;; shift has to be in %CL
    (%amd
     ((int int -> int)
      (untag (a 1))
      (untag (a 0))
      (movq (a 1) (r rcx))
      (salq (B (r rcx)) (a 0))
      (movq (a 0) trg)
      (tag trg))
     a b))
  )

(define (>> a b)
  (%backend c (%%cexp (int int -> int) "%0>>%1" a b))
  (%backend (llvm arm rv) (%llarith ashr a b))
  (%backend bytecode (%%vminsn (int int -> int) 'ashr a b))
  (%backend amd ;; shift has to be in %CL
    (%amd
     ((int int -> int)
      (untag (a 1))
      (untag (a 0))
      (movq (a 1) (r rcx))
      (sarq (B (r rcx)) (a 0))
      (movq (a 0) trg)
      (tag trg))
     a b))
  )

(define (bit-get n i)
  (%backend c (%%cexp (int int -> bool) "(%0&(1<<%1))>0" n i))
  ;; XXX probably a better way on some platforms
  (%backend (llvm arm amd rv bytecode) (>0 (logand n (<< 1 i))))
  )

(define (bit-set n i)
  (%backend c (%%cexp (int int -> int) "%0|(1<<%1)" n i))
  ;; XXX probably a better way on some platforms
  (%backend (llvm arm amd rv bytecode) (logior n (<< 1 i)))
  )

;; any reason I can't use the same characters that C does?
;; yeah - '|' is a comment start character in scheme.
(define (logior a b)
  (%backend c (%%cexp (int int -> int) "%0|%1" a b))
  (%backend (llvm arm amd rv) (%llarith or a b))
  (%backend bytecode (%%vminsn (int int -> int) 'or a b))
  )

(defmacro logior*
  (logior* x)       -> x
  (logior* a b ...) -> (logior a (logior* b ...))
  )

(define (logxor a b)
  (%backend c (%%cexp (int int -> int) "%0^%1" a b))
  (%backend (llvm arm amd rv) (%llarith xor a b))
  (%backend bytecode (%%vminsn (int int -> int) 'xor a b))
  )

(defmacro logxor*
  (logxor* x)       -> x
  (logxor* a b ...) -> (logxor a (logxor* b ...))
  )

(define (logand a b)
  (%backend c (%%cexp (int int -> int) "%0&%1" a b))
  (%backend (llvm arm amd rv) (%llarith and a b))
  (%backend bytecode (%%vminsn (int int -> int) 'and a b))
  )

(define (lognot a)
  (%backend c (%%cexp (int -> int) "~%0" a))
  (%backend (llvm bytecode) (- -1 a)) ;; why not x^-1 here?
  (%backend (amd arm rv) (logxor a -1))
  )

(define (popcount n)
  (%backend c
    (%%cexp (int -> int) "__builtin_popcount(%0)" n))
  (%backend llvm
    (%llvm-call ("@irk_popcount" (int -> int)) n))
  (%backend arm
    (%arm
     ((int -> int)
      (untag (a 0))
      (fmov (r d0) (a 0))
      (cnt.8b (r v0) (r v0))
      (uaddlv.8b (r h0) (r v0))
      (fmov (r w0) (r s0))
      (mov trg (r x0))
      (tag trg))
     n))
  (%backend amd
    (%amd
     ((int -> int)
      (untag (a 0))
      (popcnt (a 0) trg)
      (tag trg))
     n))
  (%backend rv
    (%rv ;; requires Zbb extension
     ((int -> int)
      (untag (a 0))
      (cpop trg (a 0))
      (tag trg))
     n))
  (%backend bytecode
    (let loop ((count 0) (n n))
      (if (zero? n)
          count
          (loop (+ 1 count) (logand n (- n 1)))))))

(define (pow x n)
  (cond ((< n 0) (raise (:NotImplemented "negative exp" n)))
        ((= n 0) 1)
        ((= n 1) x)
        ((= 0 (logand n 1)) (pow (* x x) (>> n 1)))
        (else (* x (pow (* x x) (>> (- n 1) 1))))))

(define (odd? n)
  (= 1 (logand 1 n)))

(define (even? n)
  (= 0 (logand 1 n)))

;; note: use llvm.minnum
(define (min x y)
  (if (< x y) x y))

;; note: use llvm.maxnum
(define (max x y)
  (if (> x y) x y))

(define (abs x) (if (< x 0) (- 0 x) x))

(define (how-many x n)
  (/ (- (+ x n) 1) n))

(datatype cmp
  (:<)
  (:=)
  (:>)
  )

(define cmp-repr
  (cmp:<) -> "<"
  (cmp:=) -> "="
  (cmp:>) -> ">"
  )

;; int-cmp is important enough to be a primitive.
(%backend c
  (define (int-cmp a b) : (int int -> cmp)
    (%%cexp (int int -> cmp) "(object*)(irk_int)UITAG((%0 < %1) ? 0 : ((%1 < %0) ? 2 : 1))" a b)))
(%backend llvm
  (define (int-cmp a b)
    (%llvm-call ("@irk_int_cmp" (int int -> cmp)) a b)))

(%backend bytecode
  ;; XXX needs an opcode, to be fair.
  ;; XXX any reason to not use 'cmp'???
  (define (int-cmp a b)
    (cond ((< a b) (cmp:<))
          ((> a b) (cmp:>))
          (else (cmp:=)))))
(%backend arm
  (define (int-cmp a b)
    (%arm
     ((int int -> cmp)
      ;; note: we do not bother with untagging the ints. [XXX verify that this is ok]
      ;; note: constants for TC_USERIMM included here, reflecting
      ;;  the three values for `cmp` datatype.
      (mov (s 0) (i 22))                ;; s0 <- #0x16 (TC_USERIMM)
      (movk (s 0) (i 1) (lsl 16))       ;; s0 <- #x10016 (cmp:=)
      (add (s 1) (s 0) (i 16) (lsl 12)) ;; s1 <- s0 + (16 << 12) (cmp:>)
      (cmp (a 0) (a 1))                 ;; cmp a, b
      (csel (s 0) (s 1) (s 0) (cc hi))  ;; s0 <= hi ? s1 : s0
      (mov (s 1) (i 22))                ;; s1 <- (cmp:<)
      (csel trg (s 1) (s 0) (cc lo)))   ;; trg <- lo ? s1 : s0
     a b
     )))
(%backend amd
  (define (int-cmp a b)
    (%amd
     ((int int -> cmp)
      (zero (s 0))
      (cmpq (a 0) (a 1))
      (setb (B (s 0)))
      (shlq (i 16) (s 0))
      (addq (i #x10016) (s 0))
      (cmpq (a 1) (a 0))
      (movq (i 22) (s 1))
      (cmovaeq (s 0) (s 1))
      (movq (s 1) trg))
     a b
     )))

(%backend rv
  ;; XXX should do some speed tests on these two...
  ;; (define (int-cmp a b)
  ;;   (%asm-c-call ("irk_int_cmp" (int int -> cmp) #f) a b))

  (define (int-cmp a b)
    (%rv
     ((int int -> cmp)
      (bltu  (a 1) (a 0) "@0")
      (lui   (s 0) (i 16))
      (addiw (s 1) (s 0) (i 22))
      (li    (s 0) (i 22))
      (bgeu  (a 0) (a 1) "@1")
      (j "@2")
      (label "@0")
      (lui   (s 0) (i 32))
      (addiw (s 1) (s 0) (i 22))
      (li    (s 0) (i 22))
      (bltu  (a 0) (a 1) "@2")
      (label "@1")
      (mv    (s 0) (s 1))
      (label "@2")
      (mv    trg (s 0)))
     a b))
  )

(define (magic-cmp a b)
  (%backend c
    ;; note: magic_cmp returns -1|0|+1, we adjust that to UITAG 0|1|2
    ;;  to match the 'cmp' datatype.
    (%%cexp ('a 'a -> cmp) "(object*)UITAG(1+magic_cmp(%0, %1))" a b))
  (%backend llvm
    (%llvm-call ("@irk_magic_cmp" ('a 'a -> cmp)) a b))
  (%backend bytecode
    (%%vminsn ('a 'a -> cmp) 'cmp a b))
  (%backend arm
    (%arm
     (('a 'a -> cmp)
      ;; might need to save/restore 29/30
      (mov (c 0) (a 0))
      (mov (c 1) (a 1))
      (bl "magic_cmp")
      (mov (w (s 0)) (i 22))
      (movk (w (s 0)) (i 1) (lsl 16))
      (add (c 0) (s 0) (c 0) (lsl 16))
      (mov trg (c 0)))
     a b))
  (%backend rv
    (%rv
     (('a 'a -> cmp)
      (mv (c 0) (a 0))
      (mv (c 1) (a 1))
      (jal "magic_cmp")
      (slli (c 0) (c 0) (i 16))
      (lui (s 0) (i 16))
      (addiw (s 0) (s 0) (i 22))
      (add trg (c 0) (s 0)))
     a b
     ))
  (%backend amd
    (%amd
     (('a 'a -> cmp)
      (movq (a 0) (c 0))
      (movq (a 1) (c 1))
      (callq (g "magic_cmp"))
      (shlq (i 16) (r rax))
      (addq (i #x10016) (r rax))
      (movq (r rax) trg))
     a b
     ))
  )

(define (eq? a b)
  (%backend c (%%cexp ('a 'a -> bool) "%0==%1" a b))
  (%backend llvm (%lleq #f a b))
  (%backend bytecode (%%vminsn ('a 'a -> bool) 'eq a b))
  (%backend arm
    (%arm
     (('a 'a -> bool)
      (com "eq?")
      (mov (s 0) (i 6)) ;; 0x0006 (#f)
      (mov (s 1) (i 6))
      (movk (s 1) (i 1) (lsl 16)) ;; 0x10006 (#t)
      (cmp (a 0) (a 1))
      (csel trg (s 1) (s 0) (cc eq)))
     a b))
  (%backend amd
    (%amd
     (('a 'a -> bool)
      (com "eq?")
      (zero (s 0))
      (cmpq (a 0) (a 1))
      (sete (B (s 0)))
      (shlq (i 16) (s 0))
      (orq (i 6) (s 0))
      (movq (s 0) trg))
     a b
     ))
  (%backend rv
    (%rv
     (('a 'a -> bool)
      (beq (a 0) (a 1) "@0")
      (li  trg (i 6))
      (j "@1")
      (label "@0")
      (lui trg (i 16))
      (addiw trg trg (i 6))
      (label "@1"))
     a b))
  )

(define (not x)
  (eq? x #f))

(define (magic<? a b)
  (eq? (cmp:<) (magic-cmp a b)))

(define (magic=? a b)
  (eq? (cmp:=) (magic-cmp a b)))

(define (char=? a b)
  (eq? a b))

(define (int->char n)
  (%backend c
    (%%cexp (int -> char) "TO_CHAR(%0)" n))
  (%backend llvm
    (%llvm-call ("@irk_makei" (int int -> char)) #x02 n))
  (%backend bytecode
    (%%vminsn (int int -> char) 'makei #x02 n))
  (%backend arm
    (%arm
     ((int -> char)
      (lsr (s 0) (a 0) (i 1))
      (mov trg (i 2))
      (bfi trg (s 0) (i 16) (i 48)))
     n))
  (%backend amd
    (%amd
     ((int -> char)
      (andq (i -2) (a 0))
      (shlq (i 15) (a 0))
      (leaq (ind 2 (a 0)) trg)
      )
     n))
  (%backend rv
    (%rv
     ((int -> char)
      (srli (a 0) (a 0) (i 1))
      (slli (a 0) (a 0) (i 16))
      (ori  trg (a 0) (i 2))
      )
     n))
  )

(define (char->int c)
  (%backend c
    (%%cexp (char -> int) "GET_CHAR(%0)" c))
  (%backend llvm
    (%llvm-call ("@irk_get_char" (char -> int)) c))
  (%backend bytecode
    (%%vminsn (char -> int) 'unchar c))
  (%backend arm
    (%arm
     ((char -> int)
      (lsr (s 0) (a 0) (i 15))
      (orr trg (s 0) (i 1)))
     c
     ))
  (%backend amd
    (%amd
     ((char -> int)
      (movq (a 0) trg)
      (shrq (i 15) trg)
      (orq (i 1) trg))
     c
     ))
  (%backend rv
    (%rv
     ((char -> int)
     (srli (a 0) (a 0) (i 15))
     (ori  trg (a 0) (i 1)))
     c))
  )

;; close enough for now.
(define char->ascii char->int)
(define ascii->char int->char)

(define (char< a b)
  (< (char->int a) (char->int b)))

(define (char-cmp a b)
  (int-cmp (char->int a) (char->int b)))

(define (string-length s)
  (%backend c (%%cexp ((raw string) -> int) "%0->len" s))
  (%backend llvm (%llvm-call ("@irk_string_len" (string -> int)) s))
  (%backend bytecode (%%vminsn (string -> int) 'slen s))
  (%backend arm
    (%arm
     ((string -> int)
      (ldrsw trg (ind (a 0) (i 8))) ;; trg = irk_string.len
      (lsl trg trg (i 1))           ;; trg <<= 1
      (orr trg trg (i 1)))          ;; trg |= 1
     s))
  (%backend amd
    (%amd
     ((string -> int)
      (movq (a 0) (s 0))
      (movl (ind 8 (s 0)) (W (s 0))) ;; grab 32-bit size
      (movslq (W (s 0)) (s 0))       ;; sign extend
      (movq (s 0) trg)
      (tag trg))
     s))
  (%backend rv
    (%rv
     ((string -> int)
      (lw trg (ind 8 (a 0)))
      (tag trg))
     s))
  )

(define (make-vector n val)
  (%backend (c llvm)
    (%ensure-heap #f n)
    (%make-vector #f n val))
  (%backend bytecode
    (%%vminsn (int 'a -> (vector 'a)) 'vmake n val))
  (%backend arm
    (%ensure-heap #f n)
    (%arm
     ((int 'a -> (vector 'a))
      ;; this is an allocating function, we have to save/restore freep.
      (str_addr (s 0) freep "freep")
      (mov (c 0) (a 0))
      (mov (c 1) (a 1))
      (untag (c 0))
      (bl "make_vector")
      (ldr_addr freep "freep")
      (mov trg (c 0)))
     n val))
  (%backend amd
    (%ensure-heap #f n)
    (%asm-c-call ("make_vector" (int 'a -> (vector 'a))) n val))
  (%backend rv
    (%ensure-heap #f n)
    (%rv
     ((int 'a -> (vector 'a))
      (str_addr freep "freep")
      (mv (c 0) (a 0))
      (mv (c 1) (a 1))
      (untag (c 0))
      (jal "make_vector")
      (ldr_addr freep "freep")
      (mv trg (c 0)))
     n val))
  )

(define (copy-vector v)
  (let ((vlen (vector-length v))
        (r (make-vector vlen v[0])))
    (for-range i vlen
      (set! r[i] v[i]))
    r))

(define (vector-length v)
  (%backend c
    (%%cexp
     ((vector 'a) -> int)
     "(%0 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*%0)" v))
  (%backend llvm (if (eq? v #()) 0 (%llvm-call ("@irk_tuple_len" ((vector 'a) -> int)) v)))
  (%backend bytecode
    (%%vminsn ((vector 'a) -> int) 'vlen v))
  (%backend arm
    (%arm (((vector 'a) -> int) (vlen trg (a 0))) v))
  (%backend amd
    (%amd (((vector 'a) -> int) (vlen (a 0) trg)) v))
  (%backend rv
    (%rv (((vector 'a) -> int) (vlen trg (a 0))) v))
  )

;; (make-array (2 3) 0) => #(#(0 0 0) #(0 0 0))
(defmacro make-array
  (make-array (n) v)
  -> (make-vector n v)
  (make-array (n ns ...) v)
  -> (list->vector
      (map (lambda (x) (make-array (ns ...) v))
           (n-of n 0))))

;; this is a little harsh. 8^)
;; think of it as a placeholder for something better to come.
(define (error x)
  (print-string "\n***\nRuntime Error, halting: ")
  (printn x)
  (%exit #f -1)
  )

(define (error1 msg ob)
  (newline)
  (print-string msg)
  (print-string " ")
  (error ob))

(define (error2 msg ob0 ob1)
  (newline)
  (print-string msg)
  (print-string "\n\t")
  (print ob0)
  (print-string "\n\t")
  (print ob1)
  (print-string "\n")
  (%exit #f -1)
  )

(define (impossible)
  (error "Why, sometimes I've believed as many as six impossible things before breakfast."))

(defmacro assert
  (assert val)     -> (if val #u (raise (:AssertionError)))
  (assert val msg) -> (if val #u (begin (print-string  msg) (newline) (raise (:AssertionError))))
  )

(define (id x) x)

;; ignore an unused value.
(define (discard _) #u)

;; these must be macros (rather than functions) in order to inline them.
;; Note that outside of callcc and throw, these are not necessarily type-safe.
(defmacro getcc
  (getcc) -> (%getcc #f))

(defmacro putcc
  (putcc k r) -> (%putcc #f k r))

;; type-safe callcc & throw from sml/nj. See "Typing First-Class Continuations in ML", Duba, Harper & McQueen.
;; http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.1725
(define (callcc p) : (((continuation 'a) -> 'a) -> 'a)
  (p (getcc)))

(define (throw k v)
  (putcc k v))

(defmacro let/cc
  (let/cc name body ...)
  -> (callcc
      (lambda ($k)
	(let ((name (lambda ($val)
		      (throw $k $val))))
	  body ...))))

;; haskell maybe /ml option
(datatype maybe (:yes 'a) (:no))
(datatype bool (:true) (:false))
(datatype result (:ok 'a) (:err 'b))
;;(datatype symbol (:t string int))

;; useful for polyvariant pairs
(define pair->first
  (:pair a _) -> a)
(define pair->second
  (:pair _ b) -> b)

(define maybe?
  (maybe:yes _) -> #t
  (maybe:no)    -> #f
  )

(define maybe
  (maybe:yes val) _ -> val
  (maybe:no)    val -> val
  )

(define probably
  (maybe:yes x) -> x
  (maybe:no) -> (raise (:ProbablyNot))
  )

;; ocaml's Obj.magic (i.e., cast to any type)
;; aside from evil, this can be used to help narrow
;;  down the source of type errors.
;; XXX no llvm or bytecode
(%backend c
  (define (magic x)
    (%%cexp ('a -> 'b) "%0" x)))

;; world save/load
;; is this is a big restriction - requiring that the thunk return an int?
;; Note: <thunk> isn't really a thunk because there's no way to cast away the
;; argument from call/cc.

;; N.B.: ASLR will cause this to break.
;; Disabling ASLR is platform-specific:
;; OSX:
;;   http://src.chromium.org/viewvc/chrome/trunk/src/build/mac/change_mach_o_flags.py?revision=111385
;;   *or* link with -no_pie (cc arg '-Wl,-no_pie')
;; Linux: setarch `uname -m` -R <binary>

(%backend c
  ;; XXX: make this work with llvm & bytecode.
  (define (dump filename thunk)
    (%%cexp (string (continuation int) -> int) "dump_image (%0, %1)" filename thunk))

  (define (load filename)
    (%%cexp (string -> (continuation int)) "load_image (%0)" filename))
  )

(%backend amd
  ;; XXX: make this work with llvm & bytecode.
  (define (dump filename thunk)
    (%asm-c-call ("dump_image" (string (continuation int) -> int)) filename thunk)
    )
  (define (load filename)
    (%asm-c-call ("load_image" (string -> (continuation int))) filename)
    )
  )

;; *********************************************************************
;; VERY IMPORTANT LESSON: do not *ever* make a generator that doesn't
;;   have an infinite loop at the end.  Very Weird Shit happens, and
;;   you'll waste two days trying to figure out how the compiler is
;;   borken.
;; I suppose I could build such a thing into make-generator? Maybe force
;;   the user to pass in an end-of-stream object?
;; Better idea: use a "maybe" object.  That way we don't have to
;;   invent a value to act as a sentinel, which won't work for some
;;   types - e.g. bools.
;; *********************************************************************

;; based on:
;;   http://list.cs.brown.edu/pipermail/plt-scheme/2006-April/012418.html
;;  urgh, they've broken that link now.  try this instead:
;;   http://hkn.eecs.berkeley.edu/~dyoo/plt/generator/
;;  this might be the original message:
;;  http://list.cs.brown.edu/pipermail/plt-scheme/2006-April/012456.html

;; this simpler version uses getcc and putcc directly.
;; note: the type annotation is necessary.
(define (make-generator producer) : ((((maybe 'a) -> undefined) -> undefined) -> ( -> (maybe 'a)))
  (let ((ready #f)
        ;; holding useless continuations
        (caller (getcc))
        (saved-point (getcc))
        )
    (define (entry-point)
      (set! caller (getcc))
      (if ready
          (putcc saved-point #u)
          (producer yield)))
    (define (yield v)
      (set! saved-point (getcc))
      (set! ready #t)
      (putcc caller v))
    entry-point
    ))

;; implements the 'generator protocol' for you.
;; XXX note: makegen & make-generator should have their names swapped.
(defmacro makegen
  (makegen emit body ...)
  -> (make-generator
      (lambda ($consumer)
        (define (emit $x) ($consumer (maybe:yes $x)))
        body ...
        (forever ($consumer (maybe:no)))
        )))

;; emits `(:tuple index item)`
(define (counting-gen gen)
  (let ((i 0))
    (makegen emit
      (for item gen
        (emit (:tuple i item))
        (inc! i)))))

;; emits `(:tuple last? item)`
(define (notify-last-gen gen)
  ;; delay each item in order to tag with `last?` boolean.
  (let ((last (match (gen) with
                (maybe:yes item) -> item
                (maybe:no)       -> (raise (:EmptyGenerator)))))
    (makegen emit
      (for item gen
        (emit (:tuple #f last))
        (set! last item))
      (emit (:tuple #t last))
      )))

(define (null-gen)
  (make-generator
   (lambda (emit)
     (forever (emit (maybe:no))))))

;; We use polymorphic variants for exceptions.
;; Since we're a whole-program compiler there's no need to declare
;; them - though I might could be convinced it's still a good idea.
;;
;; Exception data must be monomorphic to preserve type safety.
;;
;; <try> and <raise> are implemented as macros, with one extra hitch:
;;  two special compiler primitives are used to check that exception
;;  types are consistent: %exn-raise and %exn-handle

;; consider catching OSError here and printing strerror(errno):
;; (copy-cstring (%%cexp (-> cstring) "strerror (errno)"))))

;; XXX comment here on replacing/updgrading this function.
(define (base-exception-handler exn) : ((rsum 'a) -> 'b)
  (error1 "uncaught exception" exn))

(define *the-exception-handler* base-exception-handler)

(defmacro raise
  (raise exn) -> (*the-exception-handler* (%exn-raise #f exn))
  )

;; TODO:
;; * might be nice to have exceptions automatically capture __LINE__ and __FILE__
;; * have the compiler keep a map of the names of exceptions so that uncaught
;;   ones are reported in a useful way.  [another approach might be to auto-generate
;;   the base exception handler to catch and print the names of all known exceptions]

;; XXX consider how save/restore of *the-exception-handler*  might want to be done
;;  by `callcc` [or at the very least a variant that does].

;; XXX consider implementing the chain of exceptions as a stack/list,
;; rather than hiding it in a chain of lambda/let bindings.  Then we might have
;; a single global exception-handling function that tries each handler in turn.

(defmacro try
  ;; done accumulating body parts, finish up.
  (try (begin body0 ...) <except> exn-match ...)
  -> (callcc
      (lambda ($exn-k0)
        (let (($old-hand *the-exception-handler*))
          (set!
           *the-exception-handler*
           (lambda ($exn-val)
             (set! *the-exception-handler* $old-hand)
             (throw $exn-k0
              (%exn-handle #f $exn-val
               (match $exn-val with
                 exn-match ...
                 _ -> (raise $exn-val))))))
          (let (($result (begin body0 ...)))
            (set! *the-exception-handler* $old-hand)
            $result))))
  ;; accumulating body parts...
  (try (begin body0 ...) body1 body2 ...) -> (try (begin body0 ... body1) body2 ...)
  ;; begin to accumulate...
  (try body0 body1 ...)                   -> (try (begin body0) body1 ...)
  )

(define TAGSIZE 16)
(define TAGMASK #xffff)

(%backend c
  (define (set-verbose-gc b)
    (%%cexp (bool -> bool) "verbose_gc = %0" b))
  (define (get-word-size)
    (%%cexp (-> int) "sizeof(irk_int)"))
  (define (get-int-size)
    (%%cexp (-> int) "sizeof(int)"))
  )
(%backend llvm
  (define (set-verbose-gc b)
    (%llvm-call ("@irk_set_verbose_gc" (bool -> bool) ccc) b))
  ;; these are fixed by the LP64 target
  (define (get-word-size) 8)
  (define (get-int-size)  4)
  )

(%backend (arm amd rv)
  (define (set-verbose-gc b)
    (%asm-c-call ("irk_set_verbose_gc" (bool -> bool) #f) b))
  ;; these are fixed by the LP64 target
  (define (get-word-size) 8)
  (define (get-int-size)  4)
  )

(%backend bytecode

  (define (set-verbose-gc b)
    (%%vminsn (bool -> undefined) 'quiet b))

  ;; this is a kludge, but should work for a few more years...
  (define (get-word-size)
    ;; note: this string conversion is deliberate: it avoids encoding an
    ;;  over-sized literal integer into the bytecode string.
    (if (< (string->int "1073741824") 0) 4 8))

  ;; true for nearly all 32 and 64-bit platforms.
  ;; [only one I've used: old-school 64-bit OSF/1 on DEC Alpha]
  (define (get-int-size) 4)

  )

(define (get-metadata)
  (%backend c (%%cexp (-> sexp) "irk_get_metadata()"))
  (%backend llvm (%llvm-call ("@irk_get_metadata" (-> sexp))))
  (%backend bytecode (%%vminsn (-> sexp) 'meta))
  (%backend (amd arm rv) (%asm-c-call ("irk_get_metadata" (-> sexp))))
  )

(define (~~object->int ob)
  (%backend c (%%cexp ('a -> int) "unbox(irk_object2int(%0))" ob))
  (%backend llvm (%llvm-call ("@irk_object2int" ('a -> int) ccc) ob))
  (%backend bytecode (%%vminsn ('a -> int) 'ob2int ob))
  (%backend amd
    (%amd
     (('a -> int)
      (movq (a 0) trg)
      (tag trg))
     ob))
  (%backend arm
    (%arm
     (('a -> int)
      (mov trg (a 0))
      (tag trg))
     ob))
  (%backend rv
    (%rv
     (('a -> int)
      (mv trg (a 0))
      (tag trg))
     ob))
  )

(define (~~object*->int ob)
  (%backend c (%%cexp ('a -> int) "unbox(irk_objectptr2int(%0))" ob))
  (%backend llvm (%llvm-call ("@irk_objectptr2int" ('a -> int) ccc) ob))
  (%backend bytecode (%%vminsn ('a -> int) 'obptr2int ob))
  (%backend amd
    (%amd
     (('a -> int)
      (com "~~object*->int")
      (movq (ind (a 0)) trg)
      (tag trg))
     ob))
  (%backend arm
    (%arm
     (('a -> int)
      (com "~~object*->int")
      (ldr trg (ind (a 0)))
      (tag trg))
     ob))
  (%backend rv
    (%rv
     (('a -> int)
      (com "~~object*->int")
      (ld trg (ind 0 (a 0)))
      (tag trg))
     ob
     ))
  )

(define (read-cycle-counter)
  (%backend c
    (%%cexp (-> int) "(irk_int)rdtsc()"))
  (%backend llvm
    (%llvm-call ("@irk_readcyclecounter" (-> int))))
  (%backend bytecode
    (raise (:NotImplementedYet "read-cycle-counter")))
  ;; this is the 'virtual count register'
  (%backend arm
    (%arm
     ((-> int)
      (isb)
      (mrs trg "CNTVCT_EL0")
      (tag trg)
      )))
  (%backend amd
    (%amd
     ((-> int)
      (rdtsc) ;; the OG
      (shlq (i 32) (r rdx))
      (orq (r rax) (r rdx))
      (movq (r rdx) trg)
      (tag trg))))
  (%backend rv
    (%rv
     ((-> int)
      (rdcycle trg)
      (tag trg))))
  )

;; VM needs to shift all argv right by one.
(define (shrink-argv v)
  (let ((n (- (vector-length v) 1))
        (r (make-vector n "")))
    (for-range i n
      (set! r[i] v[(+ i 1)]))
    r))

(define (get-argv)
  (%backend c (%%cexp (-> (vector string)) "irk_make_argv()"))
  (%backend llvm (%llvm-call ("@irk_make_argv" (-> (vector string)) ccc)))
  (%backend bytecode (shrink-argv (%%vminsn (-> (vector string)) 'argv)))
  (%backend arm
    (%arm
     ((-> (vector string))
      (str_addr (s 0) freep "freep")
      (bl "irk_make_argv")
      (ldr_addr freep "freep")
      (mov trg (c 0)))))
  (%backend rv
    (%rv
     ((-> (vector string))
      (str_addr freep "freep")
      (jal "irk_make_argv")
      (ldr_addr freep "freep")
      (mv trg (c 0)))))
  (%backend amd
    (%asm-c-call ("irk_make_argv" (-> (vector string)))))
  )

;; note: argc is redundant, but convenient.
(define sys
  (let ((argv (get-argv))
        (argc (vector-length argv)))
    { argc=argc argv=argv }
    ))
