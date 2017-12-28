;; -*- Mode: Irken -*-

(datatype big
  (:zero)
  (:pos (list int))
  (:neg (list int))
  )

;; note: we use limbs that are nearly machine-sized, assuming that we
;; have access to two efficient primitives:
;; 1) multiply two 1-digit numbers giving a 2-digit result.
;; 2) divide a 2-digit dividend by a 1-digit divisor.

;; defaults are for 64-bit machines
(define big/bits           56)
(define big/base           #x100000000000000)
(define big/halfbits       28)
(define big/mask           #xffffffffffffff)
(define big/halfmask       #xfffffff)
(define big/repr-width     14)
(define big/decimal-base   10000000000000000)
(define big/decimal-pad    16)

;; (define (big-init bits)
;;   (define biggest-power-of-ten
;;     0 a -> a
;;     n a -> (biggest-power-of-ten (/ n 10) (+ 1 a))
;;     )
;;   (assert (= 0 (remainder bits 4)))
;;   (set! big/bits bits)
;;   (set! big/base (<< 1 bits))
;;   (set! big/halfbits (/ bits 2))
;;   (set! big/mask (- (<< 1 big/bits) 1))
;;   (set! big/halfmask (- (<< 1 big/halfbits) 1))
;;   (set! big/repr-width (/ bits 4))
;;   (set! big/decimal-pad (biggest-power-of-ten big/base -1))
;;   (set! big/decimal-base (pow 10 big/decimal-pad))
;;   (printf "bignum: using " (int bits) "-bit limbs.\n")
;;   )

;; (define (check-platform)
;;   (match (get-word-size) with
;;     8 -> (big-init 56)
;;     4 -> (big-init 28)
;;     _ -> (raise (:StrangePlatform))
;;     ))

;; note: for various reasons, this combines poorly with
;;  other code at the top-level that might do bignum computation.
;;  theoretically this will always be called first, but I think
;;  sometimes it is not.  Of course all this code would run faster
;;  if the values were constants and inlined.  Maybe we can use
;;  an included file to hide some magic?
;(check-platform)

;; for testing, use hexadecimal digits

(define (digits-repr digs)
  (let loop ((digs digs) (acc '()))
    (match digs with
      () -> acc
      (hd . tl) -> (loop tl (list:cons (format (zpad big/repr-width (hex hd))) acc))
      )))

(define big-repr
  (big:zero) -> "B0"
  (big:pos digits) -> (format "B+" (join id "." (digits-repr digits)))
  (big:neg digits) -> (format "B-" (join id "." (digits-repr digits)))
  )


(define big->digits
  (big:zero) -> '()
  (big:neg ds) -> ds
  (big:pos ds) -> ds
  )

;; canonicalize the results of a subtraction
;; Note: the list of digits is in MSB form here.
(define remove-zeros
  (0 . tl) -> (remove-zeros tl)
  x	   -> x
  )

;; in standard LSB form

(define canon-count
  ()        nz z -> (:tuple nz z)
  (0)       nz z -> (:tuple nz (+ z 1))
  (_ . tl)  nz z -> (canon-count tl (+ nz 1) z)
  )

;; remove trailing zeros, if present.
(define (canon digs)
  (let (((nz z) (canon-count digs 0 0)))
    (if (> z 0)
        (take digs nz)
        digs)))

;; ---------- comparison -----------

;; this will fail if either list is non-canonical
;;  (i.e. contains zero padding).

;;       aaaaaaaaaaaaaaaaa    <-- we are starting from this end
;;           bbbbbbbbbbbbb

(define digits-cmp
  () _    -> (cmp:<)
  _  ()   -> (cmp:>)
  (a) (b) -> (int-cmp a b)
  (a . as) (b . bs)
  -> (let ((rest (digits-cmp as bs)))
       (if (eq? rest (cmp:=))
           (int-cmp a b)
           rest))
  )

(define (digits-<? da db)
  (eq? (digits-cmp da db) (cmp:<)))

;; ---------- addition -----------

(define (digits-add* a b acc carry)
  (match a b with
    () ()   -> (reverse (if (= carry 1) (list:cons 1 acc) acc))
    () digs -> (digits-add* (LIST 0) digs acc carry)
    digs () -> (digits-add* (LIST 0) digs acc carry)
    (d0 . tl0) (d1 . tl1)
    -> (let ((sum (+ d0 d1 carry)))
	 (if (>= sum big/base)
	     (digits-add* tl0 tl1 (list:cons (- sum big/base) acc) 1)
	     (digits-add* tl0 tl1 (list:cons sum acc) 0)))
    ))

(define (digits-add a b)
  (digits-add* a b '() 0))

(define big-add
  (big:zero) x              -> x
  x (big:zero)              -> x
  (big:pos da) (big:pos db) -> (big:pos (digits-add da db))
  (big:pos da) (big:neg db) -> (big-sub (big:pos da) (big:pos db))
  (big:neg da) (big:neg db) -> (big:neg (digits-add da db))
  (big:neg da) (big:pos db) -> (big-sub (big:pos db) (big:pos da))
  )

;; ---------- subtraction -----------

;; used for borrowing
(define digits-sub1
  ()       -> (raise (:UnderflowError))
  (1)      -> '()
  (0 . tl) -> (list:cons (- big/base 1) (digits-sub1 tl))
  (n . tl) -> (list:cons (- n 1) tl)
  )

;; assumes b < a
(define (digits-sub0 a b acc)
  (match a b with
    () ()   -> (reverse (remove-zeros acc))
    () digs -> (raise (:UnderflowError))
    digs () -> (reverse-onto acc digs)
    (d0 . tl0) (d1 . tl1)
    -> (let ((diff (- d0 d1)))
	 (if (< diff 0)
	     (digits-sub0 (digits-sub1 tl0) tl1 (list:cons (+ big/base diff) acc))
	     (digits-sub0 tl0 tl1 (list:cons diff acc))))
    ))

(define (digits-sub a b)
  (digits-sub0 a b '()))

(define (digits-sub-mag da db)
  (match (digits-cmp da db) with
    (cmp:>) -> (big:pos (digits-sub da db))
    (cmp:<) -> (big:neg (digits-sub db da))
    (cmp:=) -> (big:zero)))

(define big-sub
  (big:zero) x              -> (big-negate x)
  x (big:zero)              -> x
  (big:pos da) (big:pos db) -> (digits-sub-mag da db)
  (big:pos da) (big:neg db) -> (big-add (big:pos da) (big:pos db))
  (big:neg da) (big:neg db) -> (big-negate (big-add (big:pos da) (big:pos db)))
  (big:neg da) (big:pos db) -> (big-negate (big-add (big:pos da) (big:pos db)))
  )

;; ---------- utility -----------

(define big-negate
  (big:zero)  -> (big:zero)
  (big:pos x) -> (big:neg x)
  (big:neg x) -> (big:pos x)
  )

(define big-<?
  (big:zero)  (big:zero)  -> #f
  (big:zero)  (big:pos _) -> #t
  (big:zero)  (big:neg _) -> #f
  (big:pos _) (big:zero)  -> #f
  (big:neg _) (big:zero)  -> #t
  (big:pos _) (big:neg _) -> #f
  (big:neg _) (big:pos _) -> #t
  (big:pos a) (big:pos b) -> (eq? (digits-cmp a b) (cmp:<))
  (big:neg a) (big:neg b) -> (eq? (digits-cmp a b) (cmp:>))
  )

;; assumes positive n
(define int->digits
  0 acc -> (reverse acc)
  n acc -> (int->digits
	    (/ n big/base)
	    (list:cons (mod n big/base) acc))
  )

(define int->big
  0 -> (big:zero)
  n -> (let ((pos? (>= n 0))
             (absn (if pos? n (- 0 n))))
         (if pos?
             (big:pos (int->digits absn '()))
             (big:neg (int->digits absn '())))))

(define big=
  (big:zero)  (big:zero)  -> #t
  (big:zero)  _		  -> #f
  _ (big:zero)            -> #f
  (big:pos _) (big:neg _) -> #f
  (big:neg _) (big:pos _) -> #f
  (big:pos a) (big:pos b) -> (eq? (digits-cmp a b) (cmp:=))
  (big:neg a) (big:neg b) -> (eq? (digits-cmp a b) (cmp:=))
  )

(define (add-zeros digs n)
  (append digs (n-of n 0)))

;; prepend `n` zeros to `digs`

(define shift
  digs 0 -> digs
  digs n -> (list:cons 0 (shift digs (- n 1)))
  )

(define power-of-two?
  2 acc -> (maybe:yes acc)
  n acc -> (if (odd? n)
               (maybe:no)
               (power-of-two? (>> n 1) (+ 1 acc)))
  )

;; ---------- multiplication -----------

;; two half-digits multiplied by one half-digit
;; result: three half-digits.

;;            a1 a0
;;               b0
;;            -----
;;            x1 x0
;;         y1 y0
;;         --------
;;         r2 r1 r0

(define (mul2n1 a1 a0 b0)
  (let (((x1 x0) (split-dig (* b0 a0)))
        ((y1 y0) (split-dig (* b0 a1)))
        (r1 (+ x1 y0))
        ((c1 r1) (split-dig r1)))
    (:tuple (+ y1 c1) r1 x0))) ;; r2 r1 r0

;; multiply two 1-digit values into one 2-digit result.

;;            a1 a0
;;            b1 b0
;;            -----
;;         x2 x1 x0
;;      y2 y1 y0
;;      -----------
;;      r3 r2 r1 r0

(define (mul2-not a b)
  (let (((a1 a0) (split-dig a))
        ((b1 b0) (split-dig b))
        ((x2 x1 x0) (mul2n1 a1 a0 b0))
        ((y2 y1 y0) (mul2n1 a1 a0 b1))
        (r1 (+ x1 y0))
        ((c1 r1) (split-dig r1))
        (r2 (+ x2 y1 c1))
        ((c2 r2) (split-dig r2)))
    ;; (printf "x: " (F x2) " " (F x1) " " (F x0) "\n")
    ;; (printf "y: " (F y2) " " (F y1) " " (F y0) "\n")
    ;; (printf "r1: " (F r1) " r2: " (F r2) "\n")
    ;; (printf "c1: " (F c1) " c2: " (F c2) "\n")
    ;; now we have: (y2+c2) (x2+y1+c1) (x1+y0) x0
    ;; recombine them into full digits.
    (:tuple (unsplit-dig (+ y2 c2) r2)
            (unsplit-dig r1 x0))
    ))

;; use prim from include/header1.c
(define (mul2 a b)
  (%%cexp (int int -> (rsum (rlabel tuple (pre (product int int)) (rdefault abs))))
          "irk_mul2 (%0, %1)"
          a b))

;; multiply by a single digit

;; d * n   = p1.p0
;; + carry = p1.(p0+carry)
;;         = p4.p3

(define digits-mul1*
  ()        n carry acc
  -> (reverse (remove-zeros (list:cons carry acc)))
  (d0 . tl) n carry acc
  -> (let (((p1 p0) (mul2 n d0))
           (p2 (+ p0 carry))
           (c? (> p2 big/base)))
       ;;(printf "p0 " (hex p0) "\n")
       ;;(printf "c? " (bool c?) " p1: " (hex p1) " p2: " (hex p2) "\n")
       (when c?
         (set! p1 (+ 1 p1))
         (set! p2 (- p2 big/base)))
       ;;(printf "-- " (bool c?) " p1: " (hex p1) " p2: " (hex p2) "\n")
       (digits-mul1* tl n p1 (list:cons p2 acc))))

(define (digits-mul1 x n)
  (digits-mul1* x n 0 '()))

;; grade-school algorithm
(define (digits-mul-school x y)
  (define recur
    ()        n acc -> acc
    (y0 . tl) n acc
    -> (recur tl (+ n 1)
              (digits-add
               acc
               (shift (digits-mul1 x y0) n))))
  (canon (recur y 0 '())))

;; TODO: compute this.
(define KARATSUBA-CUTOFF 10)

(define (karatsuba da db)

  ;; http://www.keithschwarz.com/interesting/code/karatsuba/Karatsuba.python.html

  (define (add a b)
    (canon (digits-add a b)))

  (define (sub a b)
    (canon (digits-sub a b)))

  (define (pad-to digs n)
    (let ((len (length digs)))
      (if (< len n)
	  (add-zeros digs (- n len))
	  digs)))

  (define K
    () _ -> (LIST 0)
    _ () -> (LIST 0)
    a b
    -> (let ((n (max (length a) (length b))))
         (if (< n KARATSUBA-CUTOFF)
             (digits-mul-school a b)
             (let ((x (pad-to a n))
                   (y (pad-to b n))
                   (n1 (/ n 2))
                   (x0 (slice x n1 n))
                   (x1 (slice x 0 n1))
                   (y0 (slice y n1 n))
                   (y1 (slice y 0 n1))
                   (p0 (K x0 y0))
                   (p1 (K (add x0 x1) (add y0 y1)))
                   (p2 (K x1 y1))
                   (z0 p0)
                   (z1 (sub p1 (add p0 p2)))
                   (z2 p2)
                   (z0prod (shift z0 (* 2 n1)))
                   (z1prod (shift z1 n1))
                   (z2prod z2))
               (add (add z0prod z1prod) z2prod)))))
  (canon (K da db))
  )

(define digits-mul
  () _     -> (LIST 0)  ;; these handle internal
  _  ()    -> (LIST 0)  ;;   results of other algorithms
  (1) x    -> x
  x (1)    -> x
  x  (one) -> (digits-mul1 x one)
  (one) x  -> (digits-mul1 x one)
  x y      -> (karatsuba x y)
  )

;; XXX handle powers of two with shifting
(define big-mul
  (big:zero) x		    -> (big:zero)
  x (big:zero)              -> (big:zero)
  (big:pos da) (big:pos db) -> (big:pos (digits-mul da db))
  (big:neg da) (big:neg db) -> (big:pos (digits-mul da db))
  (big:pos da) (big:neg db) -> (big:neg (digits-mul da db))
  (big:neg da) (big:pos db) -> (big:neg (digits-mul da db))
  )

;; ---------- shifting -----------

;; part is xxxx
;; hhhhlll
;; abcdefg => abcd efgxxxx
;;         part^     ^push acc

;; << xxx.xxxxxxx.xxxxxxx 8
;;  xxxxx.xxxxxxx.xxxxx00

(define (digits-lshift* digs n)
  (let ((nlo (- big/bits n))
        (masklo (- (<< 1 nlo) 1)))
    (define L
      acc part ()
      -> (reverse (remove-zeros (list:cons part acc)))
      acc part (d . ds)
      -> (L (list:cons
             (logior part
                     (<< (logand d masklo) n))
             acc)
            (>> d nlo)
            ds))
    (L '() 0 digs)))

(define (digits-lshift digs n)
  (let (((q r) (divmod n big/bits)))
    (when (> r 0)
      (set! digs (digits-lshift* digs r)))
    (when (> q 0)
      (set! digs (shift digs q)))
    digs
    ))

;; ppp hhhhlll
;;     1234567
;;     ppp1234 567
;;
;; >> xxx.1234567.yyyyyyy 8
;;      x.xx12345.67yyyyy yyy

(define (digits-rshift* digs n)
  (let ((nlo (- big/bits n))
        (mask (- (<< 1 n) 1)))
    (define R
      acc part ()
      -> acc
      acc part (d . ds)
      -> (R (list:cons
             (logior (<< part nlo) ;; old part
                     (>> d n))
             acc)
            (logand d mask) ;; new part
            ds))
    (R '() 0 (reverse digs))))

(define (digits-rshift digs n)
  (let (((q r) (divmod n big/bits)))
    (when (> r 0)
      (set! digs (digits-rshift* digs r)))
    (when (> q 0)
      (set! digs (drop digs q)))
    (canon digs)
    ))


(define big-lshift
  (big:zero)  _ -> (big:zero)
  (big:pos n) s -> (big:pos (digits-lshift n s))
  (big:neg n) s -> (big:neg (digits-lshift n s))
  )

(define big-rshift
  (big:zero)  _ -> (big:zero)
  (big:pos n) s -> (digits->big (digits-rshift n s) #t)
  (big:neg n) s -> (digits->big (digits-rshift n s) #f)
  )

;; ---------- division -----------

;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.565&rep=rep1&type=pdf

(define (split-dig n)
  (:tuple (>> n big/halfbits) (logand big/halfmask n)))

(define (unsplit-dig a0 a1)
  (logior (<< a0 big/halfbits) a1))

;; assumptions: b >= big/halfmask (i.e. base/2)
(define (div2b1-not ah al b)
  ;;(printf "div2b1  ah: " (F ah) " al: " (F al) " b: " (F b) "\n")
  (let (((a1 a2) (split-dig ah))
        ((a3 a4) (split-dig al))
        ((b1 b2) (split-dig b))
        ((q1 R) (div3h2 a1 a2 a3 b1 b2))
        ((r1 r2) (split-dig R))
        ((q2 S) (div3h2 r1 r2 a4 b1 b2)))
    ;; (printf "q1 " (int q1) " q2 " (int q2) "\n")
    ;; (printf "q  " (int (unsplit-dig q1 q2)) "\n")
    ;; (printf "r  " (int S) "\n")
    (:tuple (unsplit-dig q1 q2) S)))

(define (div2b1 ah al b)
  (%%cexp (int int int -> (rsum (rlabel tuple (pre (product int int)) (rdefault abs))))
          "irk_div2b1 (%0, %1, %2)"
          ah al b))

(define (F n)
  (format (zpad big/repr-width (hex n))))

(define (H n)
  (format (zpad (>> big/repr-width 1) (hex n))))

;; XXX the paper mentions a special case that probably needs
;;  dealing with here.
(define (div3h2 a1 a2 a3 b1 b2)
  ;; (printf "div3h2  a: " (H a1) " " (H a2) " " (H a3) " b: " (H b1) " " (H b2) "\n")
  (let ((a (unsplit-dig a1 a2))
        (q (/ a b1))
        (c (- a (* q b1)))
        (D (* q b2))
        (R (- (unsplit-dig c a3) D)))
    ;; (printf " q: " (F q) "\n")
    ;; (printf "R0: " (F R) "\n")
    (when (< R 0) ;; q is too large by at least one
      (set! q (- q 1))
      (set! R (+ R (unsplit-dig b1 b2)))
      ;;(printf "R1: " (F R) "\n")
      (when (< R 0) ;; q is still too large
        (set! q (- q 1))
        (set! R (+ R (unsplit-dig b1 b2)))
        ;;(printf "R2: " (F R) "\n")
        ) ;; now R is correct
      )
    (:tuple q R)
    ))

;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.111.9736&rep=rep1&type=pdf

;; XXX consider using case to indicate digits vs int, i.e. `A` and `q`.

;; divide `a` and `b` when quotient can (mostly) fit into big/base.
(define (divschool0 a b)
  ;; (printf "  divschool0:\n"
  ;;         "  a : " (join "." (digits-repr a)) "\n"
  ;;         "  b : " (join "." (digits-repr b)) "\n")
  (if (not (eq? (cmp:<) (digits-cmp a (shift b 1))))
      (begin
        ;;(printf "  **** recursing ...\n")
        (let ((a-bB (digits-sub a (shift b 1)))
              ((q r) (divschool0 a-bB b)))
          ;; q = q+base
          ;; (printf " rq " (join "." (digits-repr (digits-add q (LIST 0 1)))) "\n")
          ;; (printf " rr " (join "." (digits-repr r)) "\n")
          (:tuple (digits-add q (LIST 0 1)) r)))
      (match (reverse a) (reverse b) with
        (an1 an0 . _) (bn0 . _)
        -> (let (((q r) (div2b1 an1 an0 bn0))
                 (t (digits-mul1 b q)))
             ;; (printf "  an1 " (hex an1) " an0 " (hex an0) " bn0 " (hex bn0) "\n")
             ;; (printf "  q " (hex q) "\n")
             ;; (printf "  r " (hex r) "\n")
             ;; (printf " t0 " (join "." (digits-repr t)) "\n")
             ;; (printf "  b " (join "." (digits-repr b)) "\n")
             (when (eq? (digits-cmp t a) (cmp:>))
               (set! q (- q 1))
               (set! t (digits-sub t b)))
             (when (eq? (digits-cmp t a) (cmp:>))
               (set! q (- q 1))
               (set! t (digits-sub t b)))
             ;; (printf " t1 " (join "." (digits-repr t)) "\n")
             ;; (printf "  q " (hex q) "\n")
             ;; (printf "  r " (join "." (digits-repr (digits-sub a t))) "\n")
             (:tuple (LIST q) (digits-sub a t)))
        _ _ -> (error "divschool0 too few digits?")
        )))

;; divide a and b that have been appropriately shifted
(define (divschool1 a b)
  (let ((m (length a))
        (n (length b)))
    ;; (printf "-----------------------------\n"
    ;;         "divschool1 m: " (int m) " n: " (int n) "\n"
    ;;         "a: " (join "." (digits-repr a)) "\n"
    ;;         "b: " (join "." (digits-repr b)) "\n")
    (cond ((< m n) (:tuple '() a))
          ((= m n)
           (match (digits-cmp a b) with
             (cmp:<) -> (:tuple '() a)
             _       -> (:tuple '(1) (digits-sub a b))))
          ((= m (+ 1 n)) (divschool0 a b))
          (else
           (let ((count (- m n 1))
                 (s  (take a count)) ;; note: LSB!
                 (a1 (drop a count))
                 ((q1 r1) (divschool0 a1 b))
                 ((q r) (divschool1
                         (digits-add (canon (shift r1 count)) s)
                         b)))
             ;; (printf "q: " (join "." (digits-repr (digits-add (shift q1 count) q))) "\n"
             ;;         "r: " (join "." (digits-repr r)) "\n")
             (:tuple (digits-add (shift q1 count) q) r)))
          )))

(define (shift-to-base/2 n a)
  (if (< n (>> big/base 1))
      (shift-to-base/2 (<< n 1) (+ a 1))
      a))

;; prepare for school division by shifting both dividend and divisor
;;   until the divisor is at least base/2.
(define (divschool2 a b)
  (let ((bhi (last b))
        (nshift (shift-to-base/2 bhi 0))
        (a0 (digits-lshift a nshift))
        (b0 (digits-lshift b nshift))
        ((q r) (divschool1 a0 b0)))
    ;; remainder must be unshifted.
    (:tuple q (digits-rshift r nshift))
    ))

;; with the switch to larger limbs, this algorithm is currently broken.
;; burnzieg needs to replace digits-div1, which can only work with
;; half-sized limbs.
;; once it is working, we need to measure a correct cut-off point between
;; BZ and school division.

;; Burnikel-Ziegler:
;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.565&rep=rep1&type=pdf
;; http://damien-guichard.developpez.com/tutoriels/ocaml/?page=page_6

;; divide by a single digit (grade school algorithm)
;; NOTE: dx in MSB order, output in LSB order
(define (digits-div1 dx n)
  ;;(printf "div1 " (join int->hex-string " " dx) " n= " (hex n) "\n")
  (define div1
    () carry acc
    -> (:tuple (canon acc) (int->digits carry '()))
    (d0 . tl) carry acc
    -> (let ((v (+ d0 (* carry big/base)))
	     (q (/ v n))
	     (r (mod v n))
	     )
	 (div1 tl r (list:cons q acc))))
  (div1 dx 0 '())
  )

(define (burnzieg da db)

  (let ((la (length da))
	(lb (length db)))
    (if (<= lb 2)
	(let ((b2 (match db with
		    (b0) -> b0
		    (b0 b1) -> (+ (* b0 big/base) b1)
		    _ -> (impossible))))
	  (digits-div1 da b2))
	(let ((n (/ (- lb 1) 2))
	      (a0 (slice da (- la n) la))
	      (a1 (slice da 0 (- la n))))
	  (if (digits-<? (reverse db) (reverse a1))
	      ;; simple case
	      (let (((q1 r1) (burnzieg a1 db))
                    ((q0 r0) (burnzieg
                              (reverse (digits-add (shift r1 n) (reverse a0)))
                              db)))
		(:tuple (digits-add (shift q1 n) q0) r0))
	      ;; remainder check case
	      (let ((b0 (slice db (- lb n) lb))
		    (b1 (slice db 0 (- lb n)))
                    ((q1 r1) (burnzieg a1 b1))
                    (a0pr1 (digits-add (shift r1 n) (reverse a0)))
                    (b0xq1 (digits-mul (reverse b0) q1)))
                (if (not (digits-<? a0pr1 b0xq1))
                    (:tuple q1
                            (digits-sub a0pr1 b0xq1))
                    (:tuple (digits-sub q1 '(1))
                            (digits-sub (reverse db)
                                        (digits-sub b0xq1 a0pr1)))))
              )))
    ))

(define (digits->big x pos?)
  (match (canon x) pos? with
    () _  -> (big:zero)
    y  #t -> (big:pos y)
    y  #f -> (big:neg y)
    ))

(define (digits-div da db pos?)
  ;;(match (burnzieg (reverse da) (reverse db)) with
  (match (divschool2 da db) with
    (:tuple quo rem)
    -> (:tuple (digits->big quo pos?) (digits->big rem #t))))

;; XXX handle powers of two with shifting
(define big-div
  x            (big:zero)   -> (raise (:ZeroDivisionError))
  (big:zero)   x            -> (:tuple (big:zero) (big:zero))
  (big:pos da) (big:pos db) -> (digits-div da db #t)
  (big:neg da) (big:neg db) -> (digits-div da db #t)
  (big:pos da) (big:neg db) -> (digits-div da db #f)
  (big:neg da) (big:pos db) -> (digits-div da db #f)
  )

;; ---------- interface -----------

(define (digit->dec p pad?)
  (let ((n (match p with
	     ()    -> 0
	     (a)   -> a
	     _ -> (impossible))))
    (if pad?
	(format (zpad big/decimal-pad (int n)))
	(format (int n)))))

(define (digits->dec dn)
  (let loop ((dn0 dn)
             (acc '()))
    (let (((q r) (divschool2 dn0 (LIST big/decimal-base))))
      ;;(printf "r: " (join "." (digits-repr r)) " q: " (join "." (digits-repr q)) "\n")
      (if (null? q)
          (list:cons (digit->dec r #f) acc)
          (loop q
                (list:cons (digit->dec r #t) acc)))
      )))

(define big->dec
  (big:zero)   -> "0"
  (big:pos dn) -> (format (join (digits->dec dn)))
  (big:neg dn) -> (format "-" (join (digits->dec dn)))
  )

;; not too much thought has gone into this.
;; I'm sure something much better can be done.
(define (dec->big* s)
  (let ((len (string-length s))
        (S 10) ;; chunk size - 10^10
        (base (int->big 10000000000)) ;; 10^10
        ((quo rem) (divmod len 10))
        (r (int->big 0)))
    (for-range i quo
      (let ((sub (substring s (* i S) (* (+ i 1) S)))
            (part (int->big (string->int sub))))
        (set! r (big-add part (big-mul r base)))
        ))
    (let ((sub (substring s (* quo S) len))
          (part (int->big (string->int sub))))
      (big-add part (big-mul r (big-pow (int->big 10) rem))))))

(define (dec->big s)
  (if (starts-with s "-")
      (big-negate (dec->big* (substring s 1 (string-length s))))
      (dec->big* s)))

;; ---------- DSL -----------

;; a mini-DSL for bignum expressions.  within (big ...) the
;; normal operators translate to bignum operators.

(defmacro big
  (big (<I> n))          -> (int->big n)
  (big (<*> a b))        -> (big-mul (big a) (big b))
  (big (<*> a b ...))    -> (big-mul (big a) (big (* b ...)))
  (big (</> a b))        -> (big-quo (big a) (big b))
  (big (<mod> a b))      -> (big-mod (big a) (big b))
  (big (<+> a b))        -> (big-add (big a) (big b))
  (big (<+> a b))        -> (big-add (big a) (big (+ b ...)))
  (big (<-> a b))        -> (big-sub (big a) (big b))
  (big (<pow> x n))      -> (big-pow (big x) n)
  (big (<dec> s))        -> (dec->big s)
  (big (<expmod> b e m)) -> (big-exp-mod (big b) (big e) (big m))
  (big (<>>> x n))       -> (big-rshift x n)
  (big (<<<> x n))       -> (big-lshift x n)
  (big (<!big> exp))     -> exp
  (big (op arg ...))     -> (bigl op () (arg ...))
  (big x)                -> x
  )

(defmacro bigl
  (bigl op (acc ...) ())             -> (op acc ...)
  (bigl op (acc ...) (arg args ...)) -> (bigl op (acc ... (big arg)) (args ...))
  )

;; ---------- other funs -----------

(define big-zero?
  (big:zero) -> #t
  _          -> #f
  )

(define big-neg?
  (big:neg _) -> #t
  _           -> #f
  )

(define big-odd?
  (big:zero)     -> #f
  (big:neg (dig0 . _)) -> (= (logand 1 dig0) 1)
  (big:pos (dig0 . _)) -> (= (logand 1 dig0) 1)
  _                    -> #f ;; impossible
  )

(define (big-pow x n) : (big int -> big)
  (cond ((< n 0) (raise (:NotImplementedError)))
        ((= n 0) (int->big 1))
        (else
         (let (((q r) (divmod n 2))
               (half (big-pow x q))
               (left (if (= r 0) (int->big 1) x)))
           (big-mul left (big-mul half half))))
        ))


(define (big-quo x n)
  (let (((q _) (big-div x n)))
    q))

(define (big-rem x n)
  (let (((_ r) (big-div x n)))
    r))

;; XXX handles negative x, what about negative n?
(define (big-mod x n)
  (let ((r (big-rem x n)))
    (if (big-neg? x)
        (big-sub n r)
        r)))

(define (big-exp-mod b e m)
  (printf "expmod b " (big-repr b) " e " (big-repr e) " m " (big-repr m) "\n")
  (match e with
    (big:zero) -> (int->big 1)
    _ -> (let ((e2 (big-rshift e 1))
               (x (big-exp-mod b e2 m))
               (t (big-mod (big-mul x x) m)))
           (printf "  x " (big-repr x) "\n"
                   "  t " (big-repr t) "\n")
           (if (big-odd? e)
               (big-mod (big-mul t b) m)
               t))
    ))

;; ---------- encoding -----------

;; takes advantage of the base 2**n encoding
(define (digits->b256 ds)
  (let ((r '())
        (val 0)
        (bits 0))
    (for-list dig ds
      (set! val (logior (<< dig bits) val))
      (set! bits (+ bits big/bits))
      (while (>= bits 8)
        (PUSH r (logand #xff val))
        (set! bits (- bits 8))
        (set! val (>> val 8))))
    (if (> bits 0)
        (PUSH r val))
    r))

;; two's-complement, base 256.
;; XXX sign extension
(define (big->bin n little-endian?)
  (let ((neg? (big-neg? n))
        (digs (big->digits (if neg? (big-add (int->big 2) n) n)))
        (rdigs0 (remove-zeros (digits->b256 digs)))
        (rdigs1 (if neg? (map lognot rdigs0) rdigs0)))
    (list->string
     (map int->char
          (if little-endian?
              (reverse rdigs1)
              rdigs1)))))

