;; -*- Mode: Irken -*-

;; immediate floating-point.
;; assumes IEEE754 binary64 'double'.

;; SEEEEEEEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
;; S := sign
;; E := exponent (11 bits)
;; F := significand (52 bits)
;;
;; by masquerading as irken runtime integers, we lose one bit of the significand.
;; [bit zero is always set when stored, always cleared when accessed]

;; Note:
;; although the runtime (i.e. gc) sees an immediate float as an integer,
;;   it is not stored shifted like other integers.  This can be confusing when
;;   looking at the value as an 'integer'.
;; for example, the encoded value of '1' is:
;;   0x3ff0000000000000
;; but appears as
;;   0x1ff8000000000000
;; when viewed with %f2i.

;; two new no-op typing prims:
;; (%f2i #f n) -> changes the type of an int to float.
;; (%i2f #f n) -> changes the type of a float to int.

(require "lib/basis.scm")
(require "demo/bignum.scm")

;; XXX write our own printer.
(define (float->string f)
  (let ((buffer (make-char-buffer 40))
        (r (%%cexp (float int (cref char) -> int)
                   "snprintf(%2, %1, \"%%.17e\", %0)"
                   f 40 buffer)))
    (if (> r 40)
        (raise (:Snprintf/ResultTooBig r))
        (%cref->string #f buffer r))))

;; this isn't meant for normal computation, instead it is a tool for encoding/decoding floats.
(datatype ieee754
  (:double bool int int) ;; sign exponent significand (implied high bit of one)
  (:zero bool)           ;; sign
  (:infinity bool)       ;; sign
  (:nan bool int)        ;; NaN (not a number)
  (:subnormal bool int)  ;; sign significand (implied high bit of zero)
  )

;; this really should go in lib/core.scm
(define (bitmask w n)
  (logand n (- (<< 1 w) 1)))

;; decode an immediate float into an ieee754.
(define (float/decode f)
  (let ((fi (%f2i #f f))
        (neg? (<0 fi))
        (exp (bitmask 11 (>> fi 51)))
        (sig (logior* (<< 1 52)
                      (<< (bitmask 51 fi) 1)
                      1))) ;; tag.
    (match exp sig with
      #x000 0 -> (ieee754:zero neg?)
      #x000 _ -> (ieee754:subnormal neg? sig)
      #x7ff 0 -> (ieee754:infinity neg?)
      #x7ff _ -> (ieee754:nan neg? sig)
      _ _     -> (ieee754:double neg? (- exp 1023) sig)
      )))

;; encode an ieee754 into an immediate float.
(define float/encode
  (ieee754:double neg? exp sig)
  -> (cond ((or (> exp 2046) (< exp -1022))
            (raise (:BadFloatExponent exp)))
           ((or (< sig 0) (> sig (<< 1 52)))
            (raise (:BadFloatSignificand sig)))
           (else
            (let ((sign (if neg? 1 0))
                  (exp0 (+ exp 1023))
                  (r (logior
                      (<< (logior (<< sign 11) exp0) 51)
                      (>> (bitmask 52 sig) 1)))) ;; tricky - here is where we lose one bit.
              (%i2f #f r)
              )))
  (ieee754:zero neg?)
  -> (%i2f #f (<< (if neg? #x800 #x000) 51))
  (ieee754:infinity neg?)
  -> (%i2f #f (<< (if neg? #xfff #x7ff) 51))
  (ieee754:nan neg? sig)
  -> (%i2f #f (logior (<< (if neg? #xfff #x7ff) 51) sig))
  (ieee754:subnormal neg? sig)
  -> (%i2f #f (logior (<< (if neg? #x800 #x000) 51) sig))
  )

;; some exact values
(define f/0  (float/encode (ieee754:zero #f)))
(define f/1  (float/encode (ieee754:double #f 0 0)))
(define f/10 (float/encode (ieee754:double #f 3  #x4000000000000)))
(define f/∞  (float/encode (ieee754:infinity #f)))

(cinclude "math.h")

(define (f+ a b)
  (%%cexp (float float -> float) "%0 + %1" a b))
(define (f- a b)
  (%%cexp (float float -> float) "%0 - %1" a b))
(define (f* a b)
  (%%cexp (float float -> float) "%0 * %1" a b))
(define (f/ a b)
  (%%cexp (float float -> float) "%0 / %1" a b))
(define (fsin a)
  (%%cexp (float -> float) "sin(%0)" a))
(define (fcos a)
  (%%cexp (float -> float) "cos(%0)" a))
(define (fpow a b)
  (%%cexp (float -> float) "pow(%0,%1)" a b))
;; XXX not portable.
;; (define (fexp10 a)
;;   (%%cexp (float -> float) "__exp10(%0)" a))
(define (fexp10 a)
  (%%cexp (float -> float) "pow(10,%0)" a))

;; ---- float parsing ----

(define (parse-float s)

  (define (char->digit ch)
    (- (char->int ch) 48))

  ;; (list char) -> (list int)
  (define read-digits
    acc (digit . tl)
    -> (if (digit? digit)
           (read-digits (list:cons (char->digit digit) acc) tl)
           (:tuple (reverse acc) (list:cons digit tl)))
    acc ()
    -> (:tuple (reverse acc) (list:nil))
    )

  (define read-signed-digits
    (#\- . tl) -> (:tuple #t (read-digits (list:nil) tl))
    (#\+ . tl) -> (:tuple #f (read-digits (list:nil) tl))
    chars      -> (:tuple #f (read-digits (list:nil) chars))
    )

  (define read-fraction
    (#\. . tl) -> (read-digits (list:nil) tl)
    chars      -> (:tuple (list:nil) chars)
    )

  (define read-exponent
    (#\E #\+ . tl) -> (:tuple #f (read-digits (list:nil) tl))
    (#\E #\- . tl) -> (:tuple #t (read-digits (list:nil) tl))
    (#\E     . tl) -> (:tuple #f (read-digits (list:nil) tl))
    (#\e #\+ . tl) -> (:tuple #f (read-digits (list:nil) tl))
    (#\e #\- . tl) -> (:tuple #t (read-digits (list:nil) tl))
    (#\e     . tl) -> (:tuple #f (read-digits (list:nil) tl))
    chars          -> (:tuple #f (:tuple (list:nil) chars))
    )

  (define digits->int
    acc ()         -> acc
    acc (dig . tl) -> (digits->int (+ (* 10 acc) dig) tl)
    )

  (define digits->big
    acc ()         -> acc
    acc (dig . tl) -> (digits->big (big (+ (* big/10 acc) (I dig))) tl)
    )

  (define (clamp n dexp)
    (let ((nbits (big->bits n)))
      (if (> nbits 52)
          (clamp (big-quo n big/10) (+ dexp 1))
          (:tuple (big->int (big-lshift n (- 52 nbits))) (- nbits 1) dexp))))

  (define (normalize n)
    (logxor n (<< 1 51)))

  (let ((chars (string->list s))
        ((neg? (whole chars)) (read-signed-digits chars))
        ((frac chars) (read-fraction chars))
        ((eneg? (exp chars)) (read-exponent chars))
        (dexp0 (digits->int 0 exp))
        (dexp1 (if eneg? (- dexp0) dexp0))
        (dexp (- dexp1 (length frac)))
        (joined (digits->big big/0 (append whole frac)))
        ((n52 bexp dexp) (clamp joined dexp))
        (normal (normalize n52)))
    (printf "     s " s "\n"
            "joined " (big-repr joined) "\n"
            "   n52 " (int n52) "\n"
            "   n52 " (hex n52) "\n"
            "  bexp " (int bexp) "\n"
            " dexp0 " (int dexp0) "\n"
            " dexp1 " (int dexp1) "\n"
            "  dexp " (int dexp) "\n"
            "normal " (zpad 13 (hex normal)) "\n")
    (let ((result (float/encode (ieee754:double neg? bexp (<< normal 1)))))
      (when (not (zero? dexp))
        ;; we have a decimal exponent to deal with
        (set! result (f* result (fexp10 (int->float dexp)))))
      result)
    ))

(define (big->float n)

  (define (clamp n)
    (let ((nbits (big->bits n)))
      (if (> nbits 52)
          (clamp (big-quo n big/10))
          (:tuple (big->int (big-lshift n (- 52 nbits))) (- nbits 1)))))

  (define (normalize n)
    (logxor n (<< 1 51)))

  (define (encode n neg?)
    (let (((n52 bexp) (clamp n))
          (normal (normalize n52)))
    (float/encode (ieee754:double neg? bexp (<< normal 1)))))

  (match n with
    (big:zero)  -> f/0
    (big:pos _) -> (encode n #f)
    (big:neg _) -> (encode (big-negate n) #t))
  )

(define (int->float n)
  (big->float (int->big n)))

;; ---- testing ----

(define (t0)
  (printf "+zero " (float (float/encode (ieee754:zero #f))) "\n")
  (printf "-zero " (float (float/encode (ieee754:zero #t))) "\n")
  (printf "+inf  " (float (float/encode (ieee754:infinity #f))) "\n")
  (printf "-inf  " (float (float/encode (ieee754:infinity #t))) "\n")
  (printf "+NaN  " (float (float/encode (ieee754:nan #f 42))) "\n")
  (printf "-NaN  " (float (float/encode (ieee754:nan #t 42))) "\n")
  (printf "subn  " (float (float/encode (ieee754:subnormal #f 42))) "\n")
  )

(define (pfloat f)
  (printf (float f) "\n"))

(define (t1)
  (pfloat (parse-float "3"))
  (pfloat (parse-float "1234"))
  (pfloat (parse-float "1234.5"))
  (pfloat (parse-float "3.14159265358979323846"))
  (pfloat (parse-float "1e10"))
  (pfloat (parse-float "1e-5"))
  (pfloat (parse-float "3.14159e10"))
  (pfloat (parse-float "-3.14159"))
  (pfloat (parse-float "3.14159e1"))
  (pfloat (parse-float "3.14159e-1"))
  (pfloat (parse-float "3.14159e+1"))
  (pfloat (parse-float "7"))
  (pfloat (parse-float "7e20"))
  (pfloat (parse-float "7e-1"))
  (pfloat (parse-float "2718281828e-4"))
  (pfloat (parse-float "2718281828e+4"))
  (pfloat (parse-float "2718.281828e-4"))
  (pfloat (parse-float "2718.281828e+4"))
  )

(define (t2)
  (pfloat (f* f/∞ f/∞))
  (pfloat (float/encode (ieee754:double #f 1 #x8000000000000)))
  (pfloat (int->float 3))
  (pfloat (big->float (big-lshift (int->big 1) 400)))
  (pfloat (fexp10 (int->float 3)))
  (pfloat (parse-float "1234.567891234"))
  (pfloat (int->float -3))
  )

;; (t0)
;; (t1)
;; (t2)

(pfloat (int->float 1))
(printf (zpad 16 (hex (%f2i #f (int->float 1)))) "\n")
(pfloat (fsin (f/ (parse-float "3.14159265358979323846") (int->float 4))))
