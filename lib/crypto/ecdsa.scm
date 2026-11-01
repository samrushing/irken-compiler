;; -*- Mode: Irken -*-

;; ECDSA signature *verification* on the NIST prime curves P-256 and
;;  P-384 (FIPS 186-4, SEC 1 4.1.4), using lib/bignum.scm.
;;
;; Everything here is public data (keys, signatures, messages), so
;;  none of it is constant-time.  Do not use this code for signing:
;;  for that, see lib/crypto/p256.scm.

(require "lib/bignum.scm")
(require "lib/codecs/hex.scm")
(require "lib/asn1/ber.scm")

;; a big from an unsigned big-endian string.
(define (os->big s)
  (let loop ((i 0))
    (cond ((= i (string-length s)) big/0)
          ((= 0 (char->int (string-ref s i))) (loop (+ i 1)))
          (else (u256->big (substring s i (string-length s)))))))

(define (hex->big s) (os->big (hex->string s)))

;; both curves have a = -3.
(define ec-curve-p256
  {name = "P-256"
   oid  = '(1 2 840 10045 3 1 7)
   size = 32 ;; bytes in a field element
   p  = (hex->big "ffffffff00000001000000000000000000000000ffffffffffffffffffffffff")
   b  = (hex->big "5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b")
   n  = (hex->big "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551")
   gx = (hex->big "6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296")
   gy = (hex->big "4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5")
   })

(define ec-curve-p384
  {name = "P-384"
   oid  = '(1 3 132 0 34)
   size = 48
   p  = (hex->big (format "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe"
                          "ffffffff0000000000000000ffffffff"))
   b  = (hex->big (format "b3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875a"
                          "c656398d8a2ed19d2a85c8edd3ec2aef"))
   n  = (hex->big (format "ffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf"
                          "581a0db248b0a77aecec196accc52973"))
   gx = (hex->big (format "aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a38"
                          "5502f25dbf55296c3a545e3872760ab7"))
   gy = (hex->big (format "3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c0"
                          "0a60b1ce1d7e819d7a431d7c90ea0e5f"))
   })

(define (ec-curve-by-oid oid)
  (match oid with
    (1 2 840 10045 3 1 7) -> (maybe:yes ec-curve-p256)
    (1 3 132 0 34)        -> (maybe:yes ec-curve-p384)
    _                     -> (maybe:no)
    ))

;; --- field arithmetic mod p (inputs already reduced) ---

(define (fzero? a) (= 0 (big->bits a)))
(define (f= a b) (eq? (cmp:=) (big-cmp a b)))
(define (fmul a b p) (big-mod (big-mul a b) p))
(define (fadd a b p)
  (let ((c (big-add a b)))
    (if (big< c p) c (big-sub c p))))
(define (fsub a b p)
  (if (big< a b)
      (big-sub (big-add a p) b)
      (big-sub a b)))
;; p is prime, so a^-1 = a^(p-2).
(define (finv a p) (big-exp-mod a (big-sub p big/2) p))

;; --- points, in Jacobian coordinates: (x, y) = (X/Z^2, Y/Z^3) ---

(define ec-infinity {x=big/1 y=big/1 z=big/0})

;; dbl-2001-b, for a = -3
(define (ec-double P c)
  (if (fzero? P.z)
      P
      (let ((p c.p)
            (delta (fmul P.z P.z p))
            (gamma (fmul P.y P.y p))
            (beta  (fmul P.x gamma p))
            (alpha (fmul big/3 (fmul (fsub P.x delta p) (fadd P.x delta p) p) p))
            (x3 (fsub (fmul alpha alpha p) (fmul big/8 beta p) p))
            (yz (fadd P.y P.z p))
            (z3 (fsub (fsub (fmul yz yz p) gamma p) delta p))
            (y3 (fsub (fmul alpha (fsub (fmul big/4 beta p) x3 p) p)
                      (fmul big/8 (fmul gamma gamma p) p)
                      p)))
        {x=x3 y=y3 z=z3})))

;; add-2007-bl
(define (ec-add P Q c)
  (cond ((fzero? P.z) Q)
        ((fzero? Q.z) P)
        (else
         (let ((p c.p)
               (z1z1 (fmul P.z P.z p))
               (z2z2 (fmul Q.z Q.z p))
               (u1 (fmul P.x z2z2 p))
               (u2 (fmul Q.x z1z1 p))
               (s1 (fmul P.y (fmul Q.z z2z2 p) p))
               (s2 (fmul Q.y (fmul P.z z1z1 p) p))
               (h (fsub u2 u1 p))
               (r (fsub s2 s1 p)))
           (if (fzero? h)
               (if (fzero? r) (ec-double P c) ec-infinity)
               (let ((hh  (fmul h h p))
                     (hhh (fmul h hh p))
                     (v   (fmul u1 hh p))
                     (x3 (fsub (fsub (fmul r r p) hhh p) (fadd v v p) p))
                     (y3 (fsub (fmul r (fsub v x3 p) p) (fmul s1 hhh p) p))
                     (z3 (fmul (fmul P.z Q.z p) h p)))
                 {x=x3 y=y3 z=z3}))))))

;; the low <len> bits of <n>, most significant first.
(define (big->bitlist n len)
  (let loop ((n n) (i 0) (acc (list:nil)))
    (if (= i len)
        acc
        (loop (big-rshift n 1) (+ i 1) (list:cons (big-odd? n) acc)))))

;; u1*G + u2*Q, by Shamir's trick.
(define (ec-mul2 u1 G u2 Q c)
  (let ((GQ (ec-add G Q c))
        (nbits (max (big->bits u1) (big->bits u2))))
    (let loop ((b1 (big->bitlist u1 nbits))
               (b2 (big->bitlist u2 nbits))
               (R ec-infinity))
      (match b1 b2 with
        (x . t1) (y . t2)
        -> (let ((R2 (ec-double R c)))
             (loop t1 t2
                   (match x y with
                     #t #t -> (ec-add R2 GQ c)
                     #t #f -> (ec-add R2 G c)
                     #f #t -> (ec-add R2 Q c)
                     #f #f -> R2)))
        _ _ -> R
        ))))

;; the affine x coordinate.
(define (ec-affine-x P c)
  (let ((zi (finv P.z c.p)))
    (fmul P.x (fmul zi zi c.p) c.p)))

(define (ec-on-curve? x y c)
  (let ((p c.p)
        (y2 (fmul y y p))
        (x3 (fmul x (fmul x x p) p))
        (rhs (fadd (fsub x3 (fmul big/3 x p) p) c.b p)))
    (f= y2 rhs)))

;; an uncompressed SEC1 point (04 || x || y) -> (maybe point)
(define (ec-decode-point s c)
  (if (and (= (string-length s) (+ 1 (* 2 c.size)))
           (= 4 (char->int (string-ref s 0))))
      (let ((x (os->big (substring s 1 (+ 1 c.size))))
            (y (os->big (substring s (+ 1 c.size) (+ 1 (* 2 c.size))))))
        (if (and (big< x c.p) (big< y c.p) (ec-on-curve? x y c))
            (maybe:yes {x=x y=y z=big/1})
            (maybe:no)))
      (maybe:no)))

;; Ecdsa-Sig-Value ::= SEQUENCE { r INTEGER, s INTEGER }
(define (ecdsa-decode-sig der)
  (define int->b
    (ber:INTEGER n)  -> (if (> n 0) (maybe:yes (int->big n)) (maybe:no))
    (ber:BINTEGER s) -> (if (>0 (logand #x80 (char->int (string-ref s 0))))
                            (maybe:no) ;; negative
                            (maybe:yes (os->big s)))
    _ -> (maybe:no))
  (match (ber->asn1 der) with
    (ber:SEQUENCE (r s))
    -> (match (int->b r) (int->b s) with
         (maybe:yes r) (maybe:yes s) -> (maybe:yes (:tuple r s))
         _ _ -> (maybe:no))
    _ -> (maybe:no)
    ))

;; the leftmost bits of the digest, as an integer (SEC1 4.1.4 step 5).
(define (ecdsa-digest->big digest c)
  (let ((e (os->big digest))
        (dbits (* 8 (string-length digest)))
        (nbits (big->bits c.n)))
    (if (> dbits nbits)
        (big-rshift e (- dbits nbits))
        e)))

;; verify the DER-encoded signature <sig> over the message whose hash
;;  is <digest>, with the public key <pub> (an uncompressed point)
;;  on curve <c>.
(define (ecdsa-verify c digest sig pub)
  (let ((n c.n))
    (define (in-range? x)
      (and (not (fzero? x)) (big< x n)))
    (match (ec-decode-point pub c) (ecdsa-decode-sig sig) with
      (maybe:yes Q) (maybe:yes (:tuple r s))
      -> (if (and (in-range? r) (in-range? s))
             (let ((e  (ecdsa-digest->big digest c))
                   (w  (big-exp-mod s (big-sub n big/2) n))
                   (u1 (big-mod (big-mul e w) n))
                   (u2 (big-mod (big-mul r w) n))
                   (G  {x=c.gx y=c.gy z=big/1})
                   (R  (ec-mul2 u1 G u2 Q c)))
               (if (fzero? R.z)
                   #f
                   (f= r (big-mod (ec-affine-x R c) n))))
             #f)
      _ _ -> #f
      )))
