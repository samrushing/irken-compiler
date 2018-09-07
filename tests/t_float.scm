;; -*- Mode: Irken -*-

;; can we haz floats?
;;
;; boxing is the obvious and less efficient approach.
;;
;; can we have immediate floats though?
;;
;; [can we safely assume ieee754?]
;;
;; my first thought was to use ...10 to represent floats.
;; this would require shifting all other immediate types
;; by one, and adjusting a _lot_ of other code.  It would
;; also have a performance impact, because all immediate-testing
;; code would need a second branch to test for int, then float, then...
;;
;; but what if we could let ints masquerade as floats, knowing that we
;; lose one bit of the fraction part? (52->51 bits of fraction in a
;; binary64 float).
;;
;; the runtime code (dump_object, etc...) would have no way of telling
;;  them apart, but that facility is becoming less important.
;;
;; the typing system would need a way to cast back and forth.
;; so %i2f and %f2i.
;;
;; we would need a way to create float objects from literals.
;;

;; assumes IEEE754 binary64 'double'.

;; SEEEEEEEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
;; S := sign
;; E := exponent (11 bits)
;; F := significand (52 bits)
;;
;; by masquerading as irken integers, we lose one bit of the significand.
;; [bit zero is always set when stored, always cleared when accessed]

(require "lib/basis.scm")
(require "demo/bignum.scm")

(define (float->string f)
  (let ((buffer (make-char-buffer 40))
        (r (%%cexp (float int (cref char) -> int)
                   "snprintf(%2, %1, \"%%e\", %0)"
                   f 40 buffer)))
    (if (> r 40)
        (raise (:Snprintf/ResultTooBig r))
        (%cref->string #f buffer r))))

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
                      (bitmask 52 sig))))
              (printf (zpad 16 (hex r)) "\n")
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

(define f0 (float/encode (ieee754:zero #f)))
(define f1 (float/encode (ieee754:double #f 0 (<< 1 52))))

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

;; (printf (float (f+ f1 f1)) "\n")
;; (printf (float (f+ f1 f0)) "\n")
;; (printf (float (f- f1 f1)) "\n")

(define (t0)
  (printf "+zero " (float (float/encode (ieee754:zero #f))) "\n")
  (printf "-zero " (float (float/encode (ieee754:zero #t))) "\n")
  (printf "+inf  " (float (float/encode (ieee754:infinity #f))) "\n")
  (printf "-inf  " (float (float/encode (ieee754:infinity #t))) "\n")
  (printf "+NaN  " (float (float/encode (ieee754:nan #f 42))) "\n")
  (printf "-NaN  " (float (float/encode (ieee754:nan #t 42))) "\n")
  (printf "subn  " (float (float/encode (ieee754:subnormal #f 42))) "\n")
  )

;;(t0)

;;(printf "sin(pi/4) " (float

(define (highest-bit-set n)
  (%backend c (%%cexp (int -> int) "(64 - __builtin_clzl (%0))" n))
  (%backend llvm (- 64 (%llvm-call ("@irk_ctlz" (int -> int)) n)))
  (%backend bytecode
    (let loop ((n n) (r 0))
      (if (zero? n)
          r
          (loop (>> n 1) (+ r 1))))))

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

  (define move-dot
    whole frac 0 -> (:tuple whole frac 0)
    whole frac n
    -> (let ((lw (length whole))
             (lf (length frac))
             (m (if (>0 n) (min lf n) (min lw (- n)))))
         ;; adjust the position of the dot by moving digits from whole <-> frac.
         ;; wwww.ffff E+1 => wwwwf.fff
         ;; wwww.ffff E+2 => wwwwff.ff
         ;; wwww.ffff E-1 => www.wffff
         ;; wwww.ffff E-2 => ww.wwffff
         (cond ((>0 n) (:tuple (append whole (take frac m)) (drop frac m) (- n m)))
               ((<0 n) (:tuple (take whole (- lw m)) (append (drop whole (- lw m)) frac) (+ n m)))
               (else   (:tuple whole frac 0)))))

  (let ((chars (string->list s))
        ((neg? (whole chars)) (read-signed-digits chars))
        ((frac chars) (read-fraction chars))
        ((eneg? (exp chars)) (read-exponent chars))
        (exp0 (if eneg? (- (digits->int 0 exp)) (digits->int 0 exp)))
        ((whole frac exp) (move-dot whole frac exp0)))
    (printf "  "
            (if neg? "-" "+")
            (zpad 1 (join int->string "" whole))
            "."
            (zpad 1 (join int->string "" frac))
            "e"
            (int exp)
            "\n")
    ))

;; returns (:tuple neg? numerator denominator-power)
;; e.g. denominator-power of 5 => denominator = 10^5.

(define (string->dec-ratio s)
  (let ((chars (string->list s))
        (neg? #f))
    (let/cc return
      (match chars with
        ()         -> (return (:tuple #f (big:zero) 0)) ;; what strtod does
        (#\- . tl) -> (begin (set! chars tl) (set! neg? #t))
        _ -> #u)

      (let loop ((chars chars)
                 (r (big:zero))
                 (dot 0))
        (match chars dot with
          ()         _ -> (:tuple neg? r dot)
          (#\. . tl) 0 -> (loop tl r 1)
          (#\e . tl) _ -> (raise (:String2Float/NotImplemented s))
          (#\E . tl) _ -> (raise (:String2Float/NotImplemented s))
          (digit . tl) _
          -> (if (not (digit? digit))
                 (raise (:String2Float/ParseFailure s))
                 (loop tl
                       (big (+ (* r big/10)
                               (I (- (char->ascii digit) 48))))
                       (if (> dot 0) (+ 1 dot) dot)))
          )))))

;; ok, converting to binary.
;; 1) split into whole and fraction parts.
;;  note: there's no way to avoid this, since we are in base 10 but
;;    need the mantissa in binary.  for example, 3.14 and 31.4 have
;;    different mantissas.

;; generate the ratio 16 bits at a time.
(define (ratio-bit-gen n d)
  (set! d (big-quo d (int->big (<< 1 16))))
  (makegen emit
    (let loop ((n n))
      (let (((q r) (big-div n d)))
        (emit q)
        (loop (big-lshift r 16))))))

;; ;; too many digits, needs bignum:
;; (printn (string->dec-ratio "3.14159265358979323846"))
;; ;; just fits:
;; (printn (string->dec-ratio "3.14159265358979323"))

;; 400921fb54442d18
;; 0 100 0000 0000
;; exp = 0x400
;; actual exp
;;     = 0x400 - 0x3ff = 1
;; sig = 921fb54442d18
;;     = 1001 0010 0001 1111 1011 0101 0100 0100 0100 0010 1101 0001 1000
;; actual sig
;;     = 0001 1001 0010 0001 1111 1011 0101 0100 0100 0100 0010 1101 0001 1000
;;     = 1921fb54442d18
;;     = 0b11001001000011111101101010100010001000010110100011000
;; with exp
;;     = 0b11 0b001001000011111101101010100010001000010110100011000

;; "314159265358979323846"
;; "1000000000000000000000"
;; 300100100001111110110101010001000100001011010001100001000110


;;    3 0010 0100 0011 1111 0110 1010 1000 1000 1000 0101 1010 0011
;; 0011 0010 0100 0011 1111 0110 1010 1000 1000 1000 0101 1010 0011
;;  3    2    4    3    f    6    a    8    8    8    5    a    3

;;3243f6a8885a3

(parse-float "3.14159e10")
(parse-float "-3.14159")
(parse-float "3.14159e1")
(parse-float "3.14159e-1")
(parse-float "3.14159e+1")
(parse-float "7")
(parse-float "7e20")
(parse-float "7e-1")
(parse-float "2718281828e-4")
(parse-float "2718281828e+4")
(parse-float "2718.281828e-4")
(parse-float "2718.281828e+4")
