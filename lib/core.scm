
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (print x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0)" x))

(define (print-string s)
  (%%cexp (string -> int) "fputs (%s, stdout)" s))

(define (print-char ch)
  (%%cexp (char -> int) "fputc (GET_CHAR(%s), stdout)" ch))

(define (terpri)
  (print-char #\newline))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (zero? a)
  (%%cexp (int -> bool) "%s==0" a))

(define (< a b)
  (%%cexp (int int -> bool) "%s<%s" a b))

(define (<= a b)
  (%%cexp (int int -> bool) "%s<=%s" a b))

(define (> a b)
  (%%cexp (int int -> bool) "%s>%s" a b))

(define (>= a b)
  (%%cexp (int int -> bool) "%s>=%s" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (* a b)
  (%%cexp (int int -> int) "%s*%s" a b))

(define (/ a b)
  (%%cexp (int int -> int) "%s/%s" a b))

(define (min x y)
  (if (< x y) x y))

(define (eq? a b)
  (%%cexp ('a 'b -> bool) "%s==%s" a b))

(define (not x)
  (eq? x #f))

(define (char=? a b)
  (%%cexp (char char -> bool) "%s==%s" a b))

;; this is a little harsh. 8^)
;; think of it as a placeholder for something better to come.
(define (error x)
  (printn x)
  (%%cexp (-> 'a) "goto Lreturn")
  ;; NOTREACHED
  ;; note: keep the 'a there... it allows a call to <error> to take any type...
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(define (id x) x)

;; the '^' prefix tells the compiler to never inline this
;;  function - which would not work correctly otherwise
;;  (i.e., it can capture the wrong continuation...)
;;  [this will be Done Better Later]

(define (getcc)
  (%%cexp (-> continuation) "k"))

(define (putcc k r)
  (%%cexp (continuation 'a -> 'a) "(k=%s, %s)" k r))

(define (^call/cc p)
  (let ((k (getcc)))
    (p (lambda (r) (putcc k r)))))

;; avoid forcing the user to use the funky name
(define call/cc ^call/cc)
(define call-with-current-continuation ^call/cc)

;; world save/load
;; is this is a big restriction - requiring that the thunk return an int?

(define (dump filename thunk)
  (%%cexp (string (-> int) -> int) "dump_image (%s, %s)" filename thunk))

(define (load filename)
  (%%cexp (string -> (-> int)) "load_image (%s)" filename))

;; *********************************************************************
;; VERY IMPORTANT LESSON: do not *ever* make a generator that doesn't
;;   have an infinite loop at the end.  Very Weird Shit happens, and
;;   you'll waste two days trying to figure out how the compiler is
;;   borken.
;; I suppose I could build such a thing into make-generator? Maybe force
;;   the user to pass in an end-of-stream object?
;; *********************************************************************

;; based on:
;;   http://www.cs.brown.edu/pipermail/plt-scheme/2006-April/012418.html

;; would this be any cleaner with let/cc instead of call/cc??

(define (make-generator producer)
  (let ((ready #f)
        ;; just holding useless continuations
        (caller (call/cc id))
        (saved-point (call/cc id)))

    (define (entry-point)
      (call/cc
       (lambda (k)
         (set! caller k)
         (if ready
             (saved-point #f)
             (producer yield)))))

    (define (yield v)
      (call/cc
       (lambda (k)
         (set! ready #t)
         (set! saved-point k)
         (caller v))))
    entry-point
    ))
