;; -*- Mode: Irken -*-

(define (print x)
  (%backend c (%%cexp ('a -> undefined) "dump_object (%0, 0)" x))
  (%backend llvm (%llvm-call ("@irk_dump_object" ('a -> undefined) ccc) x))
  (%backend bytecode (%%cexp ('a -> undefined) "printo" x))
  )

(define (printn x)
  (print x)
  (newline))

;; note: discards return value.
(define (print-string s)
  (%backend c (%%cexp (string int -> int) "fwrite (%0, 1, %1, stdout)" s (string-length s)) #u)
  (%backend llvm (%llvm-call ("@irk_write_stdout" (string int -> int) ccc) s (string-length s)) #u)
  (%backend bytecode (%%cexp (string -> int) "prints" s) #u)
  )

(define (flush)
  (%backend c (%%cexp (-> int) "fflush (stdout)"))
  (%backend llvm (%llvm-call ("@irk_flush" (-> int) ccc)))
  ;; not using stdio
  (%backend bytecode #u))

(define (print-char ch)
  (%backend c (%%cexp (char -> int) "fputc (GET_CHAR(%0), stdout)" ch))
  (%backend llvm (%llvm-call ("@irk_putc" (char -> undefined) ccc) ch))
  (%backend bytecode (print-string (char->string ch)))
  )

(define (newline)
  (%backend (c llvm) (print-char #\newline))
  (%backend bytecode (print-string "\n")))

(define (= a b)
  (%backend c (%%cexp (int int -> bool) "%0==%1" a b))
  (%backend llvm (%llicmp eq a b))
  (%backend bytecode (%%cexp (int int -> bool) "eq" a b)))

(define (zero? a)
  (%backend c (%%cexp (int -> bool) "%0==0" a))
  (%backend (bytecode llvm) (= a 0)))

(define (< a b)
  (%backend c (%%cexp (int int -> bool) "%0<%1" a b))
  (%backend llvm (%llicmp slt a b))
  (%backend bytecode (%%cexp (int int -> bool) "lt" a b))
  )

(define (<= a b)
  (%backend c (%%cexp (int int -> bool) "%0<=%1" a b))
  (%backend llvm (%llicmp sle a b))
  (%backend bytecode (%%cexp (int int -> bool) "le" a b))
  )

(define (> a b)
  (%backend c (%%cexp (int int -> bool) "%0>%1" a b))
  (%backend llvm (%llicmp sgt a b))
  (%backend bytecode (%%cexp (int int -> bool) "gt" a b))
  )

(define (>= a b)
  (%backend c (%%cexp (int int -> bool) "%0>=%1" a b))
  (%backend llvm (%llicmp sge a b))
  (%backend bytecode (%%cexp (int int -> bool) "ge" a b))
  )

(define (>0 a)
  (%backend c (%%cexp (int -> bool) "%0>0" a))
  (%backend llvm (%llicmp sgt a 0))
  (%backend bytecode (%%cexp (int int -> bool) "gt" a 0))
  )

(define (<0 a)
  (%backend c (%%cexp (int -> bool) "%0<0" a))
  (%backend llvm (%llicmp slt a 0))
  (%backend bytecode (%%cexp (int int -> bool) "lt" a 0))
  )

(define (binary+ a b)
  (%backend c (%%cexp (int int -> int) "%0+%1" a b))
  (%backend llvm (%llarith add a b))
  (%backend bytecode (%%cexp (int int -> int) "add" a b))
  )

(define (binary- a b)
  (%backend c (%%cexp (int int -> int) "%0-%1" a b))
  (%backend llvm (%llarith sub a b))
  (%backend bytecode (%%cexp (int int -> int) "sub" a b))
  )

(define (binary* a b)
  (%backend c (%%cexp (int int -> int) "%0*%1" a b))
  (%backend llvm (%llarith mul a b))
  (%backend bytecode (%%cexp (int int -> int) "mul" a b))
  )

(defmacro +
  (+ x)       -> x
  (+ a b ...) -> (binary+ a (+ b ...)))

(defmacro -
  (- a)         -> (binary- 0 a)
  (- a b)	-> (binary- a b)
  (- a b c ...) -> (- (binary- a b) c ...))

(defmacro *
  (* x) -> x
  (* a b ...) -> (binary* a (* b ...)))

(define (/ a b)
  (%backend c (%%cexp (int int -> int) "%0/%1" a b))
  (%backend llvm (%llarith sdiv a b))
  (%backend bytecode (%%cexp (int int -> int) "div" a b))
  )

;; Note: this is incorrect! mod and remainder are not the same
;;  operation. See http://en.wikipedia.org/wiki/Modulo_operation
;;  [specifically, the sign of the result can differ]
(define (mod a b)
  (%backend c (%%cexp (int int -> int) "%0 %% %1" a b))
  (%backend llvm (%llarith srem a b))
  (%backend bytecode (%%cexp (int int -> int) "srem" a b))
  )

(define remainder mod)

(define (divmod a b)
  (:tuple (/ a b) (remainder a b)))

(define (<< a b)
  (%backend c (%%cexp (int int -> int) "%0<<%1" a b))
  (%backend llvm (%llarith shl a b))
  (%backend bytecode (%%cexp (int int -> int) "shl" a b))
  )

(define (>> a b)
  (%backend c (%%cexp (int int -> int) "%0>>%1" a b))
  (%backend llvm (%llarith ashr a b))
  (%backend bytecode (%%cexp (int int -> int) "ashr" a b))
  )

(define (bit-get n i)
  (%backend c (%%cexp (int int -> bool) "(%0&(1<<%1))>0" n i))
  (%backend llvm (>0 (logand n (<< 1 i))))
  (%backend bytecode (>0 (logand n (<< 1 i))))
  )

(define (bit-set n i)
  (%backend c (%%cexp (int int -> int) "%0|(1<<%1)" n i))
  (%backend llvm (logior n (<< 1 i)))
  (%backend bytecode (logior n (<< 1 i)))
  )

;; any reason I can't use the same characters that C does?
;; yeah - '|' is a comment start character in scheme.
(define (logior a b)
  (%backend c (%%cexp (int int -> int) "%0|%1" a b))
  (%backend llvm (%llarith or a b))
  (%backend bytecode (%%cexp (int int -> int) "or" a b))
  )

(define (logxor a b)
  (%backend c (%%cexp (int int -> int) "%0^%1" a b))
  (%backend llvm (%llarith xor a b))
  (%backend bytecode (%%cexp (int int -> int) "xor" a b))
  )

(define (logand a b)
  (%backend c (%%cexp (int int -> int) "%0&%1" a b))
  (%backend llvm (%llarith and a b))
  (%backend bytecode (%%cexp (int int -> int) "and" a b))
  )

(define (lognot a)
  (%backend c (%%cexp (int -> int) "~%0" a))
  (%backend llvm (- -1 a))
  (%backend bytecode (- -1 a))
  )

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
  (define (int-cmp a b)
    (%%cexp (int int -> cmp) "(object*)(pxll_int)UITAG((%0 < %1) ? 0 : ((%1 < %0) ? 2 : 1))" a b)))
(%backend llvm
  (define (int-cmp a b)
    (%llvm-call ("@irk_int_cmp" (int int -> cmp)) a b)))
(%backend bytecode
  ;; XXX needs an opcode, to be fair.
  (define (int-cmp a b)
    (cond ((< a b) (cmp:<))
          ((> a b) (cmp:>))
          (else (cmp:=)))))

(define (magic-cmp a b)
  (%backend c
    ;; note: magic_cmp returns -1|0|+1, we adjust that to UITAG 0|1|2
    ;;  to match the 'cmp' datatype.
    (%%cexp ('a 'a -> cmp) "(object*)UITAG(1+magic_cmp(%0, %1))" a b))
  (%backend llvm
    (%llvm-call ("@irk_magic_cmp" ('a 'a -> cmp)) a b))
  (%backend bytecode
    (%%cexp ('a 'a -> cmp) "cmp" a b))
  )

(define (magic<? a b)
  (eq? (cmp:<) (magic-cmp a b)))

(define (eq? a b)
  (%backend c (%%cexp ('a 'a -> bool) "%0==%1" a b))
  (%backend llvm (%lleq #f a b))
  (%backend bytecode (%%cexp ('a 'a -> bool) "eq" a b))
  )

(define (not x)
  (eq? x #f))

(define (char=? a b)
  (eq? a b))

(define (int->char n)
  (%backend c
    (%%cexp (int -> char) "TO_CHAR(%0)" n))
  (%backend llvm
    (%llvm-call ("@irk_makei" (int int -> char)) #x02 n))
  (%backend bytecode
    (%%cexp (int int -> char) "makei" #x02 n))
  )

(define (char->int c)
  (%backend c
    (%%cexp (char -> int) "GET_CHAR(%0)" c))
  (%backend llvm
    (%llvm-call ("@irk_get_char" (char -> int)) c))
  (%backend bytecode
    (%%cexp (char -> int) "unchar" c))
  )

;; close enough for now.
(define char->ascii char->int)
(define ascii->char int->char)

(define (char< a b)
  (< (char->int a) (char->int b)))

(define (string-length s)
  (%backend c (%%cexp ((raw string) -> int) "%0->len" s))
  (%backend llvm (%llvm-call ("@irk_string_len" (string -> int)) s))
  (%backend bytecode (%%cexp (string -> int) "slen" s))
  )

(define (make-vector n val)
  (%backend (c llvm)
    (%ensure-heap #f n)
    (%make-vector #f n val))
  (%backend bytecode
    (%%cexp (int 'a -> (vector 'a))
            "vmake"
            n val))
  )

(define (vector-length v)
  (%backend c
    (%%cexp
     ((vector 'a) -> int)
     "(%0 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*%0)" v))
  (%backend llvm (if (eq? v #()) 0 (%llvm-call ("@irk_tuple_len" ((vector 'a) -> int)) v)))
  (%backend bytecode
    (%%cexp ((vector 'a) -> int) "vlen" v))
  )

(define (address-of ob)
  (%%cexp ('a -> int) "(pxll_int)%0" ob))

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

(define assert
  #t -> #u
  #f -> (error "assertion failed.")
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

(defmacro letcc
  (letcc name body ...)
  -> (callcc (lambda (name) body ...)))

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

;; ocaml's Obj.magic (i.e., cast to any type)
(define (magic x)
  (%%cexp ('a -> 'b) "%0" x))

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

(define (dump filename thunk)
  (%%cexp (string (continuation int) -> int) "dump_image (%0, %1)" filename thunk))

(define (load filename)
  (%%cexp (string -> (continuation int)) "load_image (%0)" filename))

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

(%backend c
  (define (set-verbose-gc b)
    (%%cexp (bool -> bool) "verbose_gc = %0" b))
  (define (get-word-size)
    (%%cexp (-> int) "sizeof(pxll_int)"))
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

(%backend bytecode

  (defmacro make-ffi
    (make-ffi name rtype nargs (formal0 ...) (ftype0 ...))
    -> (let (($pfun (%%cexp (string -> int) "dlsym" name)))
         (lambda (formal0 ...)
           (printf "** ffi: " name "\n")
           (%%cexp (int char int ftype0 ...)
                   "ffi"
                   $pfun rtype nargs
                   formal0 ...))))

  (define (set-verbose-gc b)
    (%%cexp (bool -> undefined) "quiet" b))

  ;; this is a kludge, but should work for a few more years...
  (define (get-word-size)
    (match (< (<< 1 30) 0) with
      #t -> 4
      #f -> 8
      ))

  (define (get-int-size)
    ;; true for nearly all 32 and 64-bit platforms.
    ;; [only one I've used: old-school 64-bit OSF/1 on DEC Alpha]
    4)


  )
