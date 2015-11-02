;; -*- Mode: Irken -*-

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (print x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0)" x))

(define (print-string s)
  (%%cexp (string int -> int) "fwrite (%0, 1, %1, stdout)" s (string-length s))
  #u
  )

;; original version returns how many chars were written...
(define (print-string* s)
  (%%cexp (string int -> int) "fwrite (%0, 1, %1, stdout)" s (string-length s)))

(define (print-char ch)
  (%%cexp (char -> int) "fputc (GET_CHAR(%0), stdout)" ch))

(define (terpri)
  (print-char #\newline))

(define newline terpri)

(define (= a b)
  (%%cexp (int int -> bool) "%0==%1" a b))

(define (zero? a)
  (%%cexp (int -> bool) "%0==0" a))

(define (< a b)
  (%%cexp (int int -> bool) "%0<%1" a b))

(define (<= a b)
  (%%cexp (int int -> bool) "%0<=%1" a b))

(define (> a b)
  (%%cexp (int int -> bool) "%0>%1" a b))

(define (>= a b)
  (%%cexp (int int -> bool) "%0>=%1" a b))

(define (>0 a)
  (%%cexp (int -> bool) "%0>0" a))

(define (<0 a)
  (%%cexp (int -> bool) "%0<0" a))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (- a b)
  (%%cexp (int int -> int) "%0-%1" a b))

(define (* a b)
  (%%cexp (int int -> int) "%0*%1" a b))

(define (/ a b)
  (%%cexp (int int -> int) "%0/%1" a b))

(define (remainder a b)
  (%%cexp (int int -> int) "%0 %% %1" a b))

(define (<< a b)
  (%%cexp (int int -> int) "%0<<%1" a b))

(define (>> a b)
  (%%cexp (int int -> int) "%0>>%1" a b))

(define (bit-get n i)
  (%%cexp (int int -> bool) "(%0&(1<<%1))>0" n i))

(define (bit-set n i)
  (%%cexp (int int -> int) "%0|(1<<%1)" n i))

;; any reason I can't use the same characters that C does?
;; yeah - '|' is a comment start character in scheme.
(define (logior a b)
  (%%cexp (int int -> int) "%0|%1" a b))

(define (logxor a b)
  (%%cexp (int int -> int) "%0^%1" a b))

(define (logand a b)
  (%%cexp (int int -> int) "%0&%1" a b))

(define (lognot a b)
  (%%cexp (int int -> int) "%0~%1" a b))

(define (min x y)
  (if (< x y) x y))

(define (max x y)
  (if (> x y) x y))

(define (abs x) (if (< x 0) (- 0 x) x))

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define (not x)
  (eq? x #f))

(define (char=? a b)
  (%%cexp (char char -> bool) "%0==%1" a b))

(define (string-length s)
  (%%cexp ((raw string) -> int) "%0->len" s))

(define (make-vector n val)
  (%ensure-heap #f n)
  (%make-vector #f n val))

;; (define (make-vec16 n)
;;   ;; XXX ensure-heap here
;;   (%make-vec16 #f n))

(define (vector-length v)
  (%%cexp
   ((vector 'a) -> int)
   "(%0 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*%0)" v))

(define (address-of ob)
  (%%cexp ('a -> int) "(pxll_int)%0" ob))

;; this is a little harsh. 8^)
;; think of it as a placeholder for something better to come.
(define (error x)
  (print-string "\n***\nRuntime Error, halting: ")
  (printn x)
  (%exit #f x)
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
  (%exit #f #u)
  )

(define (impossible)
  (error "Why, sometimes I've believed as many as six impossible things before breakfast."))

(define (id x) x)

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

;; ocaml's Obj.magic
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
(define (make-generator producer)
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

(cinclude "sys/errno.h")

(define (syscall retval)
  (if (< retval 0)
      (raise (:OSError (%%cexp (-> int) "errno")))
      retval))
