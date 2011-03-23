;; -*- Mode: Irken -*-

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (print x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0)" x))

(define (print-string s)
  (%%cexp (string int -> undefined) "(fwrite (%0, 1, %1, stdout), PXLL_UNDEFINED)" s (string-length s)))

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

;; these must be macros (rather than functions) so that call/cc etc
;;   will still work if inlining is turned off.
(defmacro getcc
  (getcc) -> (%%cexp (-> (continuation 'a)) "k"))
(defmacro putcc
  (putcc k r) -> (%%cexp ((continuation 'a) 'a -> 'b) "(k=%0, %1)" k r))

;; the '^' prefix tells the compiler to never inline this
;;  function - which would not work correctly otherwise
;;  (i.e., it can capture the wrong continuation...)
;;  [this will be Done Better Later]

(define (^call/cc p)
  (let ((k (getcc)))
    (p (lambda (r) (putcc k r)))
    ))

;; sml-nj version
(define (^callcc p)
  (p (getcc)))

(define callcc ^callcc)

(define (throw k v)
  (putcc k v))

;; this won't work because it captures the wrong continuation
;; (defmacro let/cc
;;   (let/cc name body ...)
;;   -> (let ((k (getcc))
;; 	   (name (lambda (r) (putcc k r))))
;;        body ...))

(defmacro let/cc
  (let/cc name body ...)
  -> (^call/cc (lambda (name) body ...)))

;; using smlnj callcc
(defmacro letcc
  (let/cc name body ...)
  -> (callcc (lambda (name) body ...)))

;; avoid forcing the user to use the funky name
(define call/cc ^call/cc)
(define call-with-current-continuation ^call/cc)

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

(define (dump filename thunk)
  (%%cexp (string ('a -> int) -> int) "dump_image (%0, %1)" filename thunk))

(define (load filename)
  (%%cexp (string -> ('a -> int)) "load_image (%0)" filename))

;; *********************************************************************
;; VERY IMPORTANT LESSON: do not *ever* make a generator that doesn't
;;   have an infinite loop at the end.  Very Weird Shit happens, and
;;   you'll waste two days trying to figure out how the compiler is
;;   borken.
;; I suppose I could build such a thing into make-generator? Maybe force
;;   the user to pass in an end-of-stream object?
;; *********************************************************************

;; based on:
;;   http://list.cs.brown.edu/pipermail/plt-scheme/2006-April/012418.html
;;  urgh, they've broken that link now.  try this instead:
;;   http://hkn.eecs.berkeley.edu/~dyoo/plt/generator/
;;  this might be the original message:
;;  http://list.cs.brown.edu/pipermail/plt-scheme/2006-April/012456.html

;; (define (make-generator producer)
;;   (let ((ready #f)
;;         ;; just holding useless continuations
;;         (caller (call/cc id))
;;         (saved-point (call/cc id)))

;;     (define (entry-point)
;;       (call/cc
;;        (lambda (k)
;;          (set! caller k)
;;          (if ready
;;              (saved-point #f)
;;              (producer yield)))))

;;     (define (yield v)
;;       (call/cc
;;        (lambda (k)
;;          (set! ready #t)
;;          (set! saved-point k)
;;          (caller v))))
;;     entry-point
;;     ))

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
