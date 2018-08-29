;; -*- Mode: Irken -*-

(require "demo/bignum.scm")

;; this code is a nearly verbatim translation of Thomas Pornin's
;;   constant-time i31 code from BearSSL.
;;
;; please see: https://www.bearssl.org/constanttime.html
;;
;; for more info on the interfaces implemented here, see BearSSL's src/inner.h
;;   and src/int/i31_*.c
;;
;; This code uses Irken's 'native' integer type, which is a 63-bit signed integer.
;; [one bit is lost to tagging].  For this and many other reasons, the code generated
;; by the irken compiler is _similar_, but nowhere near identical to that in the
;; original C.  The constant-time nature of this code has also not yet been verified,
;; but because of the overhead added by Irken's runtime, there are fewer worries that
;; the C optimizer will cause problems.

;; note: some extra masking with `logand` is done here that is not in
;; the original code, because we are really doing all calcuations with
;; i64/i63 integers.

(define (NOT ctl)
  (logand 1 (logxor ctl 1)))

(defmacro MUX
  (MUX ctl x y) -> (logxor y (logand (- ctl) (logxor x y))))

(define (NEQ x y)
  (let ((q (logxor x y)))
    (logand 1 (>> (logior q (- q)) 31))))

(define (EQ x y)
  (NOT (NEQ x y)))

(define (GT x y)
  (let ((z (- y x)))
    (logand 1 (>> (logxor z (logand (logxor x y) (logxor x z))) 31))))

(define (GE x y) (NOT (GT y x)))
(define (LT x y) (GT y x))
(define (LE x y) (NOT (GT x y)))

(define (CMP x y)
  (logior
   (GT x y)
   (- (GT y x))))

(define (BIT-LENGTH x)
  (let ((k (NEQ x 0))
        (c 0))
    (set! c (GT x #xffff)) (set! x (MUX c (>> x 16) x)) (inc! k (<< c 4))
    (set! c (GT x #x00ff)) (set! x (MUX c (>> x  8) x)) (inc! k (<< c 3))
    (set! c (GT x #x000f)) (set! x (MUX c (>> x  4) x)) (inc! k (<< c 2))
    (set! c (GT x #x0003)) (set! x (MUX c (>> x  2) x)) (inc! k (<< c 1))
    (inc! k (GT x #x0001))
    k))

;; Note: these are constant-time only on modern processors.
(defmacro MUL31
  (MUL31 x y) -> (binary* x y)
  )

(define (MUL31-lo x y)
  (logand #x7fffffff (* x y)))

;; NOTE: in bearssl, this is a byte-level copy. here, it is used only
;;   for copying vectors.
(define (CCOPY ctl dst src len)
  (for-range i len
    (set! dst[i] (MUX ctl src[i] dst[i]))))

;; make a vector designed to hold an i31 of this many bits.
(define (i31/make bit-length)
  (let ((words (how-many bit-length 31))
        (v (make-vector (+ 1 words) 0)))
    (set! v[0] bit-length)
    v
    ))

;; note: untested
(define (i31/iszero? x)
  (let ((z 0)
        (u (>> (+ x[0] 31) 5)))
    (while (> u 0)
      (set! z (logior z x[u]))
      (dec! u))
    (>> (lognot (logior z (- z))) 31)))

(define (i31/add a b ctl)
  (let ((cc 0)
        (m (>> (+ a[0] 63) 5))
        (naw 0))
    (for-range* u 1 m
      (set! naw (+ a[u] b[u] cc))
      (set! cc (logand 1 (>> naw 31)))
      (set! a[u] (MUX ctl (logand naw #x7fffffff) a[u])))
    cc))

(define (i31/sub a b ctl)
  (let ((cc 0)
        (m (>> (+ a[0] 63) 5))
        (naw 0))
    (for-range* u 1 m
      (set! naw (- a[u] b[u] cc))
      (set! cc (logand 1 (>> naw 31)))
      (set! a[u] (MUX ctl (logand naw #x7fffffff) a[u])))
    cc))

(define (i31/bit-length x xlen)
  (let ((tw 0)
        (twk 0))
    (while (> xlen 0)
      (dec! xlen)
      (let ((c (EQ tw 0))
            (w x[(+ xlen 1)]))
        (set! tw (MUX c w tw))
        (set! twk (MUX c xlen twk))))
    (+ (<< twk 5) (BIT-LENGTH tw))))

(define (i31/zero x bit-len)
  (set! x[0] bit-len)
  (for-range i (>> (+ bit-len 31) 5)
    (set! x[(+ i 1)] 0)))

;; note: untested
(define (i31/rshift x count)
  (let ((len (>> (+ x[0] 31) 5))
        (r (>> x[1] count)))
    (when (not (= 0 len))
      (for-range* u 2 len
        (set! x[(- u 1)] (logand (logior (<< x[u] (- 31 count)) r) #x7FFFFFFF))
        (set! r (>> x[u] count)))
      (set! x[len] r)
      )))

(define (i31/reduce x a m)
  (let ((m-bitlen m[0])
        (mlen (>> (+ 31 m-bitlen) 5))
        (a-bitlen a[0])
        (alen (>> (+ 31 a-bitlen) 5)))
    (set! x[0] m-bitlen)
    (when (not (= m-bitlen 0))
      (if (< a-bitlen m-bitlen)
          (for-range* i 1 (+ 1 mlen)
            (set! x[i] (if (<= i alen) a[i] 0)))
          (begin
            (for-range* i 1 mlen
              (set! x[i] a[(+ i 1 (- alen mlen))]))
            (set! x[mlen] 0)
            (let loop ((u (+ 1 (- alen mlen))))
              (when (> u 0)
                (i31/muladd-small x a[u] m)
                (loop (- u 1)))))))))

;; smr: replacement for the uses of memmove
(define (scoot v amount start count)
  (match (int-cmp count 0) with
    (cmp:=) -> #u
    ;; scoot to the left
    ;; -2 [....01234...]
    ;; => [..01234xx...]
    (cmp:<)
    -> (for-range i count ;; left to right
         (set! v[(+ i start amount)]
               v[(+ i start)]))
    ;; scoot to the right
    ;; +2 [..01234.....]
    ;; => [..xx01234...]
    (cmp:>)
    -> (for-range-rev i count ;; right to left
         (set! v[(+ start i amount)]
               v[(+ start i)]))
    ))

;; version of br_divrem
(define (ctbig/divrem hi lo d)
  (let ((q 0)
        (ch (EQ hi d))
        (cf 0))
    (set! hi (MUX ch 0 hi))
    (for-range-rev k 32
      (let ((j (- 32 k))
            (w (logior (<< hi j) (>> lo k)))
            (ctl (logior (GE w d) (>> hi k)))
            (hi2 (>> (- w d) j))
            (lo2 (- lo (<< d k))))
        (set! hi (MUX ctl hi2 hi))
        (set! lo (MUX ctl lo2 lo))
        (set! q (logior q (<< ctl k)))))
    (set! cf (logior (GE lo d) hi))
    (set! q (logior q cf))
    (:tuple q (MUX cf (- lo d) lo))
    ))

(define (ctbig/div hi lo d)
  (match (ctbig/divrem hi lo d) with
    (:tuple q r) -> q))

(define (ctbig/rem hi lo d)
  (match (ctbig/divrem hi lo d) with
    (:tuple q r) -> r))

(define (i31/muladd-small x z m)
  (when (not (= m[0] 0))
    (if (<= m[0] 31)
        (let ((hi (>> x[1] 1))
              (lo (logior (<< x[1] 31) z)))
          (set! x[1] (ctbig/rem hi lo m[1])))
        (let ((mlen (>> (+ 31 m[0]) 5))
              (mblr (logand 31 m[0]))
              (hi x[mlen])
              (a0 0) (a1 0) (b0 0) (g 0) (q 0))
          (cond ((= mblr 0)
                 (set! a0 x[mlen])
                 (scoot x 1 1 (- mlen 1))
                 (set! x[1] z)
                 (set! a1 x[mlen])
                 (set! b0 m[mlen]))
                (else
                 (set! a0 (logand #x7fffffff (logior (<< x[mlen] (- 31 mblr)) (>> x[(- mlen 1)] mblr))))
                 (scoot x 1 1 (- mlen 1))
                 (set! x[1] z)
                 (set! a1 (logand #x7fffffff (logior (<< x[mlen] (- 31 mblr)) (>> x[(- mlen 1)] mblr))))
                 (set! b0 (logand #x7fffffff (logior (<< m[mlen] (- 31 mblr)) (>> m[(- mlen 1)] mblr))))
                 ))
          (set! g (ctbig/div (>> a0 1) (logior a1 (<< a0 31)) b0))
          (set! q (MUX (EQ a0 b0) #x7fffffff (MUX (EQ g 0) 0 (- g 1))))
          (let ((cc 0)
                (tb 1)
                (over 0)
                (under 0)
                (mw 0)
                (zl 0)
                (zw 0)
                (xw 0)
                (nxw 0))
            (for-range* u 1 (+ 1 mlen)
              (set! mw m[u])
              (set! zl (+ (MUL31 mw q) cc))
              (set! cc (>> zl 31))
              (set! zw (logand zl #x7fffffff))
              (set! xw x[u])
              (set! nxw (logand #xffffffff (- xw zw)))
              (inc! cc (>> nxw 31))
              (set! nxw (logand nxw #x7fffffff))
              (set! x[u] nxw)
              (set! tb (MUX (EQ nxw mw) tb (GT nxw mw))))
            (set! over (GT cc hi))
            (set! under (logand (lognot over) (logior tb (LT cc hi))))
            (i31/add x m over)
            (i31/sub x m under)
            #u
            )))))

(define (i31/ninv31 x)
  (let ((y (- 2 x)))
    (set! y (* y (- 2 (* y x))))
    (set! y (* y (- 2 (* y x))))
    (set! y (* y (- 2 (* y x))))
    (set! y (* y (- 2 (* y x))))
    (logand #x7fffffff (MUX (logand 1 x) (- y) 0))
    ))

(define (i32-zero x bit-len)
  (set! x[0] bit-len)
  (for-range i (>> (+ bit-len 31) 5)
    (set! x[(+ 1 i)] 0)))

;; this is a hack/experiment to see if I can avoid some heap allocation
;;  in the for loop by using a loop variable that already exists in the
;;  surrounding environment.  [better work in the optimizer would make
;;  this unnecessary].
(defmacro for-range-no-let
  (for-range-no-let vname lo hi body ...)
  -> (begin
       (set! vname lo)
       (let $loop ()
         (if (= vname hi)
             #u
             (begin body ...
                    (inc! vname)
                    ($loop))))))

(define (i31/montymul d x y m m0i)
  (let ((len (>> (+ m[0] 31) 5))
        (len4 (logand len (lognot 3)))
        (dh 0) (zh 0) (f 0) (z 0) (v 0) (r 0)
        (u 0)
        )
    (i32-zero d m[0])
    (for-range-no-let u 0 len
      (set! v 0)
      (set! f (MUL31-lo (+ d[1] (MUL31-lo x[(+ u 1)] y[1])) m0i))
      ;; this unrolling does provide a slight advantage, even with irken's output.
      (while (< v len4)
        (set! z (+ d[(+ v 1)] (MUL31 x[(+ u 1)] y[(+ v 1)]) (MUL31 f m[(+ v 1)]) r))
        (set! r (logand #xffffffff (>> z 31)))
        (set! d[(+ v 0)] (logand z #x7fffffff))
        (set! z (+ d[(+ v 2)] (MUL31 x[(+ u 1)] y[(+ v 2)]) (MUL31 f m[(+ v 2)]) r))
        (set! r (logand #xffffffff (>> z 31)))
        (set! d[(+ v 1)] (logand z #x7fffffff))
        (set! z (+ d[(+ v 3)] (MUL31 x[(+ u 1)] y[(+ v 3)]) (MUL31 f m[(+ v 3)]) r))
        (set! r (logand #xffffffff (>> z 31)))
        (set! d[(+ v 2)] (logand z #x7fffffff))
        (set! z (+ d[(+ v 4)] (MUL31 x[(+ u 1)] y[(+ v 4)]) (MUL31 f m[(+ v 4)]) r))
        (set! r (logand #xffffffff (>> z 31)))
        (set! d[(+ v 3)] (logand z #x7fffffff))
        (inc! v 4))
      (while (< v len)
        (set! z (+ d[(+ v 1)] (MUL31 x[(+ u 1)] y[(+ v 1)]) (MUL31 f m[(+ v 1)]) r))
        (set! r (logand #xffffffff (>> z 31)))
        (set! d[v] (logand z #x7fffffff))
        (inc! v))
      (set! zh (+ dh r))
      (set! d[len] (logand #x7fffffff zh))
      (set! dh (>> zh 31))
      )
    (set! d[0] m[0])
    (i31/sub d m (logior (NEQ dh 0) (NOT (i31/sub d m 0))))
    ))

(define (i31->monty x m)
  (let loop ((k (>> (+ m[0] 31) 5)))
    (when (> k 0)
      (i31/muladd-small x 0 m)
      (loop (- k 1)))))

(define (monty->i31 x m m0i)
  (let ((len (>> (+ m[0] 31) 5)))
    (for-range u len
      (let ((f (MUL31-lo x[1] m0i))
            (cc 0))
        (for-range v len
          (let ((z (+ x[(+ v 1)] (MUL31 f m[(+ v 1)]) cc)))
            (set! cc (>> z 31))
            (when (not (= 0 v))
              (set! x[v] (logand z #x7fffffff)))))
        (set! x[len] (logand #xffffffff cc)) ;; XXX is this mask necessary?
        ))
    ))

;; `e` is a string in u256 form.
(define (i31/modpow x e elen m m0i t1 t2)
  (let ((mlen (>> (+ m[0] 63) 5)) ;; in words, NOT bytes
        (ex 0)
        (ctl 0))
    (for-range i mlen
      (set! t1[i] x[i]))
    (i31->monty t1 m)
    (i31/zero x m[0])
    (set! x[1] 1)
    (for-range k (<< elen 3)
      (set! ex (char->int (string-ref e (- elen 1 (>> k 3)))))
      (set! ctl (logand (>> ex (logand k 7)) 1))
      (i31/montymul t2 x t1 m m0i)
      (CCOPY ctl x t2 mlen)
      (i31/montymul t2 t1 t1 m m0i)
      (for-range i mlen
        (set! t1[i] t2[i])))))

(define (i31/mulacc d a b)
  (let ((alen (>> (+ a[0] 31) 5))
        (blen (>> (+ b[0] 31) 5))
        (f 0)
        (cc 0)
        (z 0))
    (set! d[0] (+ a[0] b[0]))
    (for-range u blen
      (set! f b[(+ 1 u)])
      (set! cc 0)
      (for-range v alen
        (set! z (+ d[(+ 1 u v)] (MUL31 f a[(+ 1 v)]) cc))
        (set! cc (>> z 31))
        (set! d[(+ 1 u v)] (logand z #x7fffffff))
        (set! d[(+ 1 u alen)] (logand #xffffffff cc)) ;; mask needed?
        ))))

;; we violate the bignum abstraction a bit here because there is
;; [currently] no other way to mask out portions the lowest limb.
(define (digits->i31 v)
  (let ((v0 v)
        (i31 (list:nil)))
    (while (> (vector-length v0) 0)
      (PUSH i31 (logand v0[(- (vector-length v0) 1)] #x7fffffff))
      (set! v0 (digits-rshift v0 31)))
    (let ((nwords (length i31))
          (ri31 (reverse i31))
          (result (make-vector (+ 1 nwords) 0)))
      (for-range i nwords
        (set! result[(+ i 1)] (nth ri31 i)))
      (set! result[0] (i31/bit-length result nwords))
      result)))

(define (big->i31 n)
  (match n with
    (big:pos _) -> (digits->i31 (big->digits n))
    otherwise   -> (raise (:CTBig/ExpectedPositive otherwise))
    ))

(define (i31->big v)
  (let ((r (big:zero)))
    (for-range-rev* i 1 (vector-length v)
      (set! r (big-add (big-lshift r 31) (int->big v[i]))))
    r))

;; rsasp1:
;; (let ((m1 (big-exp-mod m skey.dP skey.p))
;;       (m2 (big-exp-mod m skey.dQ skey.q))
;;       (h (big-mod (big-mul skey.qInv (big-sub m1 m2)) skey.p)))
;;   (big-add m2 (big-mul skey.q h))))

(define (i31/rsa-crt x sk)
  (let ((x31 (big->i31 x))
        (p31 (big->i31 sk.p))
        (q31 (big->i31 sk.q))
        (dPs (big->u256 sk.dP))
        (dQs (big->u256 sk.dQ))
        (p0i (i31/ninv31 p31[1]))
        (q0i (i31/ninv31 q31[1]))
        (s1 (i31/make p31[0]))
        (s2 (i31/make (+ p31[0] q31[0]))) ;; room for result
        (t1 (i31/make p31[0]))
        (t2 (i31/make q31[0])))
    (i31/reduce s2 x31 q31)
    (i31/modpow s2 dQs (string-length dQs) q31 q0i t1 t2)
    (i31/reduce s1 x31 p31)
    (i31/modpow s1 dPs (string-length dPs) p31 p0i t1 t2)
    (i31/reduce t2 s2 p31)
    (i31/add s1 p31 (i31/sub s1 t2 1))
    (i31->monty s1 p31)
    (i31/reduce t1 (big->i31 sk.qInv) p31)
    (i31/montymul t2 s1 t1 p31 p0i)
    (i31/mulacc s2 q31 t2)
    (i31->big s2)
    ))
