
(define (not x)
  (%eq? x #f))

(define (error x)
  (%printn x)
  ;; return undefined because abort() is <void>
  (%%cexp "(abort(), PXLL_UNDEFINED)"))

(define (immediate-int? n)
  (%%cexp "PXLL_TEST(((pxll_int)(%s))&1)" n))

(define (integer? n)
  (immediate-int? n))

(define (get-typecode x)
  (%%cexp "box(get_typecode(%s))" x))

(define (eq? x y)
  (%eq? x y))

(define (> x:int y:int)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%gt? x y))

(define (< x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%lt? x y))

(define (= x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%eq? x y))

(define (+ x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%+ x y))

(define (- x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%- x y))

(define (* x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%* x y))

(define (/ x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%/ x y))

(define (tuple-length t)
  ;; XXX type check it to make sure it's not immediate
  ;; *** doesn't count the <next> pointer ***
  (%%cexp "box(((*(pxll_int *)(%s))>>8)-1)" t))

;; XXX range check!
(define (tuple-ref t n)
  ;; *** doesn't count the <next> pointer ***
  (%%verify "TC_TUPLE" 1 t)
  (%%cexp "((pxll_tuple*)(%s))->val[unbox(%s)]" t n))

(define (tuple-next t)
  (%%verify "TC_TUPLE" 1 t)
  (%%cexp "((pxll_tuple*)(%s))->next" t))

;; the '^' prefix tells the compiler to never inline this
;;  function - which would not work correctly otherwise
;;  (i.e., it can capture the wrong continuation...)
;;  [this will be Done Better Later]

(define (^call/cc p)
  (let ((k (%getcc)))
    (p (lambda (r) (%putcc k r)))))

;; avoid forcing the user to use the funky name
(define call/cc ^call/cc)
(define call-with-current-continuation ^call/cc)

; world save/load

(define (dump filename thunk)
  (%%verify "TC_STRING" 1 filename)
  (%%verify "TC_CLOSURE" 1 thunk)
  (%%cexp "dump_image (GET_STRING_POINTER(%s), %s)" filename thunk)
  #f)

(define (load filename)
  (%%verify "TC_STRING" 1 filename)
  (%%cexp "load_image (GET_STRING_POINTER(%s))" filename))

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

(define (make-generator producer)
  (let ((caller #f)
	(saved-point #f))

    (define (entry-point)
      (^call/cc
       (lambda (k)
	 (set! caller k)
	 (if saved-point
	     (saved-point #f)
	     (producer yield)))))

    (define (yield v)
      (^call/cc
       (lambda (k)
	 (set! saved-point k)
	 (caller v))))

    entry-point
    ))
