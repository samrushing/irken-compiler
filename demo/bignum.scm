;; -*- Mode: Irken -*-

(datatype big
  (:zero)
  (:pos (vector int))
  (:neg (vector int))
  )

;; (define big/bits           56)
;; (define big/base           #x100000000000000)
;; (define big/halfbits       28)
;; (define big/mask           #xffffffffffffff)
;; (define big/halfmask       #xfffffff)
;; (define big/repr-width     14)
;; (define big/decimal-base   10000000000000000)
;; (define big/decimal-pad    16)

(define big/bits           60)
(define big/base           #x1000000000000000)
(define big/halfbits       30)
(define big/mask           #xfffffffffffffff)
(define big/halfmask       #x3fffffff)
(define big/repr-width     15)
(define big/decimal-base   1000000000000000000)
(define big/decimal-pad    18)

;; digits are in most-significant-first order.

(define (digits-repr digs)
  (let ((r '()))
    ;; iterate in reverse to accumulate
    (for-range-rev i (vector-length digs)
      (PUSH r (format (zpad big/repr-width (hex digs[i])))))
    (format (join "." r))))

(define big-repr
  (big:zero)     -> "B0"
  (big:pos digs) -> (format "B+" (digits-repr digs))
  (big:neg digs) -> (format "B-" (digits-repr digs))
  )

(define big->digits
  (big:zero)   -> #()
  (big:neg ds) -> ds
  (big:pos ds) -> ds
  )

;; ---------- utility -----------

(defmacro swap
  (swap a b)
  -> (let ((tmp a))
       (set! a b)
       (set! b tmp)))

(defmacro inc!
  (inc! n)   -> (set! n (+ n 1))
  (inc! n m) -> (set! n (+ n m))
  )

(defmacro dec!
  (dec! n)   -> (set! n (- n 1))
  (dec! n m) -> (set! n (- n m))
  )

(define (vcons v n)
  (let ((len (vector-length v))
        (r (make-vector (+ 1 len) 0)))
    (set! r[0] n)
    (for-range i len
      (set! r[(+ i 1)] v[i]))
    r))

(define (vtake v n)
  (let ((r (make-vector n v[0])))
    (for-range i n
      (set! r[i] v[i]))
    r))

(define (vdrop v n)
  (let ((len (vector-length v))
        (len1 (- len n))
        (r (make-vector len1 v[0])))
    (for-range i len1
      (set! r[i] v[(+ i n)]))
    r))

;; drop any leading zeros
(define (canon digs)
  (let ((len (vector-length digs))
        (i 0)
        (drop 0))
    (while (and (< i len) (= digs[i] 0))
      (inc! i))
    (if (> i 0)
        (vdrop digs i)
        digs)))

(define (split-dig n)
  (:tuple (>> n big/halfbits) (logand big/halfmask n)))

(define (unsplit-dig a0 a1)
  (logior (<< a0 big/halfbits) a1))

(define big-negate
  (big:zero)  -> (big:zero)
  (big:pos x) -> (big:neg x)
  (big:neg x) -> (big:pos x)
  )

;; assumes positive n
(define int->digits
  0 acc -> (list->vector acc)
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

;; append `n` zeros to `digs`
;; note: equivalent to "<< (n * big/bits)"
(define (shift digs n)
  (let ((len (vector-length digs))
        (r (make-vector (+ n len) 0)))
    (for-range i len
      (set! r[i] digs[i]))
    r))

(define power-of-two?
  2 acc -> (maybe:yes acc)
  n acc -> (if (odd? n)
               (maybe:no)
               (power-of-two? (>> n 1) (+ 1 acc))))

;; ---------- comparison -----------

;; this will fail if either list is non-canonical
;;  (i.e. contains zero padding).

(define (digits-cmp* a b i n)
  (cond ((= i n) (cmp:=))
        ((= a[i] b[i]) (digits-cmp* a b (+ i 1) n))
        (else (int-cmp a[i] b[i]))))

(define (digits-cmp a b)
  (let ((alen (vector-length a))
        (blen (vector-length b)))
    (if (= alen blen)
        (digits-cmp* a b 0 alen)
        (int-cmp alen blen))))

(define (digits<* a b i n)
  (cond ((= i n) #f)
        ((= a[i] b[i]) (digits<* a b (+ i 1) n))
        (else (< a[i] b[i]))))

(define (digits< a b)
  (let ((alen (vector-length a))
        (blen (vector-length b)))
    (match (int-cmp alen blen) with
      (cmp:<) -> #t
      (cmp:>) -> #f
      (cmp:=) -> (digits<* a b 0 alen)
      )))

;; ---------- addition -----------

;; XXX use digits-add! below
(define (digits-add* a b)
  (let ((alen (vector-length a))
        (blen (vector-length b))
        (r (make-vector alen 0))
        (carry 0)
        (dig 0)
        (i (- alen 1))
        (j (- blen 1)))
    (while (>= i 0)
      (set! dig (+ a[i] carry (if (>= j 0) b[j] 0)))
      (cond ((>= dig big/base)
             (set! r[i] (- dig big/base))
             (set! carry 1))
            (else
             (set! r[i] dig)
             (set! carry 0)))
      (dec! i)
      (dec! j))
    (if (= carry 0) r (vcons r 1))
    ))

(define (digits-add a b)
  ;; guarantee that a is longer or equal
  (match (int-cmp (vector-length a) (vector-length b)) with
    (cmp:<) -> (swap a b)
    _       -> #u)
  (digits-add* a b))

(define big-add
  (big:zero) x              -> x
  x (big:zero)              -> x
  (big:pos da) (big:pos db) -> (big:pos (digits-add da db))
  (big:pos da) (big:neg db) -> (big-sub (big:pos da) (big:pos db))
  (big:neg da) (big:neg db) -> (big:neg (digits-add da db))
  (big:neg da) (big:pos db) -> (big-sub (big:pos db) (big:pos da))
  )

;; ---------- subtraction -----------

;; assumes b < a
(define (digits-sub a b)
  (let ((alen (vector-length a))
        (blen (vector-length b))
        (r (make-vector alen 0))
        (borrow 0)
        (dig 0)
        (i (- alen 1))
        (j (- blen 1)))
    (while (>= i 0)
      (set! dig (- a[i] (if (>= j 0) b[j] 0) borrow))
      (cond ((< dig 0)
             (set! borrow 1)
             (set! dig (+ dig big/base)))
            (else
             (set! borrow 0)))
      (set! r[i] dig)
      (dec! i)
      (dec! j))
    (if (= borrow 1)
        (raise (:UnderflowError))
        (canon r))))

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

;; ---------- shifting -----------

(define bit-size
  0 acc -> acc
  n acc -> (bit-size (>> n 1) (+ acc 1))
  )

;; we do a little extra work to figure out
;;   the result size (to avoid a copy by `canon`).

;; msb: 000000xxxxxxxxxxx
;;        xxxxxxxxxxxxxyy n < bits - bit-size
;;   xxxxxxxxxxxyyyyyyyyy n > bits - bit-size
;;      xxxxxxxxxxxyyyyyy n = bits - bit-size

;; assumes: n < big/bits.
(define (digits-lshift* digs n)
  (let ((len (vector-length digs))
        ;; do we need an extra digit?
        (extra (if (> n (- big/bits (bit-size digs[0] 0))) 1 0))
        (rlen (+ len extra))
        (r (make-vector rlen 0))
        (nlo (- big/bits n))
        (masklo (- (<< 1 nlo) 1))
        (part 0))
    ;; we work right to left...
    (for-range-rev i len
      (set! r[(+ i extra)] (logior part (<< (logand digs[i] masklo) n)))
      (set! part (>> digs[i] nlo))
      )
    (if (= extra 1)
        (set! r[0] part))
    r))

(define (digits-lshift digs n)
  (let (((q r) (divmod n big/bits)))
    (when (> r 0)
      (set! digs (digits-lshift* digs r)))
    (when (> q 0)
      (set! digs (shift digs q)))
    digs
    ))

;; msb: 00000xxxxxxxxxxxx
;;      0000000000xxxxxxx n < bit-size
;;      00000000000000000 n = bit-size
;;      00000000000000000 n > bit-size

;; drop = 0:
;;    deadbeef
;;     deadbee
;; drop = 1:
;;    deadbeef
;;      deadbe

;; assumes: n < big/bits.
(define (digits-rshift* digs n)
  (let ((len (vector-length digs))
        (drop (if (< n (bit-size digs[0] 0)) 0 1))
        (rlen (- len drop))
        (r (make-vector rlen 0))
        (nlo (- big/bits n))
        (mask (- (<< 1 n) 1))
        (part 0))
    ;; left to right...
    (for-range i len
      ;; if drop == 1 and i == 0, we are only fetching part
      (if (not (and (= drop 1) (= i 0)))
          (set! r[(- i drop)] (logior (<< part nlo) (>> digs[i] n))))
      (set! part (logand digs[i] mask)))
    r))

(define (digits-rshift digs n)
  (if (eq? digs #())
      #()
      (let (((q r) (divmod n big/bits)))
        (when (> r 0)
          (set! digs (digits-rshift* digs r)))
        (when (> q 0)
          (set! digs (vtake digs (- (vector-length digs) q))))
        digs)))

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

;; ---------- multiplication -----------

;; note: these two functions are needed only when a
;;  mul2 primitive is unavailable.

;; XXX consider having these write into a result vector
;;     rather than allocating tuple results, not only
;;     would this be more efficient, but it would also
;;     allow mul2n1 to be inlined without allocating either.

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

;; note: this replaces mul2 and mul2n1 above.
;; use prim from include/header1.c
(define (mul2 a b r)
  (%%cexp (int int (vector int) -> undefined)
          "irk_mul2 (%0, %1, %2)"
          a b r))

;; for adding a partial product into the final sum.
;;
;; RRRRRRRRRRRRR
;;     AAAAAA--- this is with an offset of 3.
;;    CXXXXXX    X := R+A
;;               C is the carry added to
;;                 the digit one to the left.

;; XXX make digits-add use this.
(define (digits-add! r a off)
  (let ((rlen (vector-length r))
        (alen (vector-length a))
        (carry 0)
        (i (- rlen 1 off))
        (j (- alen 1)))
    (while (>= j 0)
      (set! r[i] (+ r[i] carry a[j]))
      (cond ((>= r[i] big/base)
             (set! r[i] (- r[i] big/base))
             (set! carry 1))
            (else
             (set! carry 0)))
      (dec! i)
      (dec! j))
    carry ;; note: returns final carry.
    ))

;;    AAAAA
;;    *   B
;;    -----
;;   CCCCCC

;; n = |A|
(define (digits-mul1 A b R)
  (let ((carry 0)
        (mulv #(0 0))
        (p2 0))
    (for-range-rev i (vector-length A)
      (mul2 A[i] b mulv)
      (set! p2 (+ mulv[1] carry))
      (cond ((>= p2 big/base)
             (set! R[(+ i 1)] (- p2 big/base))
             (set! carry (+ mulv[0] 1)))
            (else
             (set! R[(+ i 1)] p2)
             (set! carry mulv[0]))))
    (set! R[0] carry)))

(define (vcheck v)
  (let ((bad #f))
    (for-vector x v
      (when (> x big/base)
        (printf "bad digit " (zpad 14 (hex x)) "\n")
        (set! bad #t)
        ))
    bad))

;; grade-school algorithm
;;      AAAAA                       99999  note: carry to an extra
;; *      BBB                  *      999  digit is not possible.
;; ----------                  ----------
;;     000000                      899991
;;    111111                      899991
;; + 222222                    + 899991
;; ----------                  ----------
;;   PPPPPPPP                    99899001
;;

;; assumes |a| >= |b|
(define (digits-mul-school a b)
  (let ((alen (vector-length a))
        (blen (vector-length b))
        (part (make-vector (+ alen 1) 0)) ;; for each partial product |a|+1
        (r (make-vector (+ alen blen) 0)) ;; for the result |a|+|b|
        (carry 0))                        ;; carry for running sum.
    (for-range i blen
      (inc! r[(- blen i)] carry)
      ;; get partial product into <part>
      (digits-mul1 a b[(- blen i 1)] part)
      ;; add it into the final sum
      (set! carry (digits-add! r part i))
      )
    r))

;; XXX vector patterns are not implemented yet.
;; (define digits-mul
;;   #() _     -> #(0)  ;; these handle internal
;;   _  #()    -> #(0)  ;;   results of other algorithms
;;   #(1) x    -> x
;;   x #(1)    -> x
;;   ;; XXX karatsuba goes here.
;;   x y      -> (digits-mul-school x y)
;;   )

(define (digits-mul a b)
  (cond ((or (eq? #() a) (eq? #() b)) #(0))          ;; these handle internal results
        ((and (= 1 (vector-length a)) (= a[0] 1)) b) ;; of other algorithms.
        ((and (= 1 (vector-length b)) (= b[0] 1)) a)
        ;; XXX karatsuba goes here.
        (else
         (canon (digits-mul-school a b)))))

;; XXX handle powers of two with shifting
(define big-mul
  (big:zero) x		    -> (big:zero)
  x (big:zero)              -> (big:zero)
  (big:pos da) (big:pos db) -> (big:pos (digits-mul da db))
  (big:neg da) (big:neg db) -> (big:pos (digits-mul da db))
  (big:pos da) (big:neg db) -> (big:neg (digits-mul da db))
  (big:neg da) (big:pos db) -> (big:neg (digits-mul da db))
  )

;; ---------- division -----------

;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.565&rep=rep1&type=pdf
;; assumptions: b >= big/halfmask (i.e. base/2)

;; note: these two functions are needed only when a
;;  div2b1 primitive is unavailable.

;; XXX as commented above, consider having these write into a result vector.

(define (F n)
  (format (zpad big/repr-width (hex n))))

(define (H n)
  (format (zpad (>> big/repr-width 1) (hex n))))

;; divide 3 half-digits by two half-digits.

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

;; on most architectures, there's a two-word divide-with-remainder insn.
(define (div2b1 ah al b)
  (%%cexp (int int int -> (rsum (rlabel tuple (pre (product int int)) (rdefault abs))))
          "irk_div2b1 (%0, %1, %2)"
          ah al b))

;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.111.9736&rep=rep1&type=pdf

;; XXX consider using case to indicate digits vs int, i.e. `A` and `q`.

;; divide `a` and `b` when quotient can (mostly) fit into big/base.
;; assumes |a| >= 2, |b| >= 1
(define (divschool0 a b)
  ;; (printf "  divschool0:\n"
  ;;         "  a : " (digits-repr a) "\n"
  ;;         "  b : " (digits-repr b) "\n")
  ;;(if (not (eq? (cmp:<) (digits-cmp a (shift b 1))))
  (if (not (digits< a (shift b 1)))
      (begin
        ;; (printf "  **** recursing ...\n")
        (let ((a-bB (digits-sub a (shift b 1)))
              ((q r) (divschool0 a-bB b)))
          ;; q = q+base
          ;; (printf " rq " (digits-repr (digits-add q #(1 0))) "\n")
          ;; (printf " rr " (digits-repr r) "\n")
          (:tuple (digits-add q #(1 0)) r)))
      (let (((q r) (div2b1 a[0] a[1] b[0]))
            (t (digits-mul b #(q)))) ;; XXX was digits-mul1
        ;; (printf "  an1 " (hex a[0]) " an0 " (hex a[1]) " bn0 " (hex b[0]) "\n")
        ;; (printf "  q " (hex q) "\n")
        ;; (printf "  r " (hex r) "\n")
        ;; (printf " t0 " (digits-repr t) "\n")
        ;; (printf "  b " (digits-repr b) "\n")
        ;;(when (eq? (digits-cmp t a) (cmp:>))
        (when (digits< a t)
          (set! q (- q 1))
          (set! t (digits-sub t b)))
        ;;(when (eq? (digits-cmp t a) (cmp:>))
        (when (digits< a t)
          (set! q (- q 1))
          (set! t (digits-sub t b)))
        ;; (printf " t1 " (digits-repr t) "\n")
        ;; (printf "  q " (hex q) "\n")
        ;; (printf "  r " (digits-repr (digits-sub a t)) "\n")
        (:tuple #(q) (digits-sub a t)))
      ))

;; divide a and b that have been appropriately shifted
(define (divschool1 a b)
  (let ((m (vector-length a))
        (n (vector-length b)))
    ;; (printf "-----------------------------\n"
    ;;         "divschool1 m: " (int m) " n: " (int n) "\n"
    ;;         "a: " (digits-repr a) "\n"
    ;;         "b: " (digits-repr b) "\n")
    (cond ((< m n) (:tuple #() a))
          ((= m n)
           ;; (match (digits-cmp a b) with
           ;;   (cmp:<) -> (:tuple #() a)
           ;;   _       -> (:tuple #(1) (digits-sub a b))))
           (if (digits< a b)
               (:tuple #() a)
               (:tuple #(1) (digits-sub a b))))
          ((= m (+ 1 n)) (divschool0 a b))
          (else
           (let ((count (- m n 1))
                 (a1 (vtake a (+ n 1)))
                 (s  (vdrop a (+ n 1)))
                 ((q1 r1) (divschool0 a1 b))
                 ((q r) (divschool1
                         (digits-add (canon (shift r1 count)) s)
                         b)))
             ;; (printf "q: " (digits-repr (digits-add (shift q1 count) q)) "\n"
             ;;         "r: " (digits-repr r) "\n")
             (:tuple (digits-add (shift q1 count) q) r)))
          )))

(define (shift-to-base/2 n a)
  (if (< n (>> big/base 1))
      (shift-to-base/2 (<< n 1) (+ a 1))
      a))

;; prepare for school division by shifting both dividend and divisor
;;   until the divisor is at least base/2.
(define (divschool2 a b)
  (let ((nshift (shift-to-base/2 b[0] 0))
        (a0 (digits-lshift a nshift))
        (b0 (digits-lshift b nshift))
        ((q r) (divschool1 a0 b0)))
    ;; remainder must be unshifted.
    (:tuple q (digits-rshift r nshift))
    ))

(define (digits->big x pos?)
  (match (canon x) pos? with
    #() _ -> (big:zero)
    v  #t -> (big:pos v)
    v  #f -> (big:neg v)
    ))

(define (digits-div da db pos?)
  ;; XXX burnzieg goes here.
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

;; --------------- decimal conversion ------------

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

(define (digit->dec p pad?)
  (let ((n (if (eq? p #())
               0
               p[0])))
    (if pad?
	(format (zpad big/decimal-pad (int n)))
	(format (int n)))))

(define (digits->dec dn)
  (let loop ((dn0 dn)
             (acc '()))
    (let (((q r) (divschool2 dn0 #(big/decimal-base))))
      ;;(printf "r: " (join "." (digits-repr r)) " q: " (join "." (digits-repr q)) "\n")
      (if (eq? q #())
          (list:cons (digit->dec r #f) acc)
          (loop q
                (list:cons (digit->dec r #t) acc)))
      )))

(define big->dec
  (big:zero)   -> "0"
  (big:pos dn) -> (format (join (digits->dec dn)))
  (big:neg dn) -> (format "-" (join (digits->dec dn)))
  )

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

(define (vlast v)
  v[(- (vector-length v) 1)]
  )

(define big-odd?
  (big:zero)     -> #f
  (big:neg digs) -> (= (logand 1 (vlast digs)) 1)
  (big:pos digs) -> (= (logand 1 (vlast digs)) 1)
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
  ;;(printf "expmod b " (big-repr b) " e " (big-repr e) " m " (big-repr m) "\n")
  (match e with
    (big:zero) -> (int->big 1)
    _ -> (let ((e2 (big-rshift e 1))
               (x (big-exp-mod b e2 m))
               (t (big-mod (big-mul x x) m)))
           ;; (printf "  x " (big-repr x) "\n"
           ;;         "  t " (big-repr t) "\n")
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

(define remove-zeros
  (0 . tl) -> (remove-zeros tl)
  x	   -> x
  )

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

