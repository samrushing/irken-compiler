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
  (:double bool int int) ;; sign exponent significand
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

(printf "+zero " (float (float/encode (ieee754:zero #f))) "\n")
(printf "-zero " (float (float/encode (ieee754:zero #t))) "\n")
(printf "+inf  " (float (float/encode (ieee754:infinity #f))) "\n")
(printf "-inf  " (float (float/encode (ieee754:infinity #t))) "\n")
(printf "+NaN  " (float (float/encode (ieee754:nan #f 314159))) "\n")
(printf "-NaN  " (float (float/encode (ieee754:nan #t 314159))) "\n")
(printf "subn  " (float (float/encode (ieee754:subnormal #f 314159))) "\n")

;;(printf "sin(pi/4) " (float

(define (highest-bit-set n)
  (%backend c (%%cexp (int -> int) "(64 - __builtin_clzl (%0))" n))
  (%backend llvm (- 64 (%llvm-call ("@irk_ctlz" (int -> int)) n)))
  (%backend bytecode
    (let loop ((n n) (r 0))
      (if (zero? n)
          r
          (loop (>> n 1) (+ r 1))))))

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

;; generate the ratio 4 bits at a time.
(define (ratio-bit-gen n d)
  (set! d (big-quo d (int->big 16)))
  (makegen emit
    (let loop ((n n))
      (let (((q r) (big-div n d)))
        (emit q)
        (loop (big-lshift r 4))))))

;; too many digits, needs bignum:
(printn (string->dec-ratio "3.14159265358979323846"))
;; just fits:
(printn (string->dec-ratio "3.14159265358979323"))

(let (((neg? num den-pow) (string->dec-ratio "3.14159265358979323846"))
      (gen (ratio-bit-gen num (big-pow big/10 (- den-pow 1)))))
  (printn (big->dec num))
  (printn (big->dec (big-pow big/10 den-pow)))
  (for-range i 60
    (match (gen) with
      (maybe:yes digit)
      -> (printf (bin (big->int digit)) " ")
      (maybe:no)
      -> (impossible))))
(printf "\n")

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


;; 3  0010 0100 0011 1111 0110 1010 1000 1000 1000 0101 1010 0011
;; 11 0010 0100 0011 1111 0110 1010 1000 1000 1000 0101 1010 0011
;;  3  2    4    3    f    6    a    8    8    8    5    a    3

;;3243f6a8885a3

