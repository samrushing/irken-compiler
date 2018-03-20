;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")
(include "lib/codecs/hex.scm")

;; test the base-256 codec for bigints.

(define tests-passed 0)

(defmacro assert2
  (assert2 exp)
  -> (if (not exp)
         (begin
           (printf "assertion failed: " (repr (car (%%sexp exp))) "\n")
           (raise (:AssertionFailed)))
         (set! tests-passed (+ tests-passed 1))))


;; --- slow/simple/correct versions to test against ---

(define big->int
  (big:pos digs) -> digs[0]
  x -> (raise (:RSA/BadInt x))
  )

(define (string->big s)
  (let ((r (big:zero)))
    (for-string ch s
      (set! r (big (+ (<< r 8) (I (char->int ch))))))
    r))

(define (big->string n)
  (let ((r '()))
    (while (not (big-zero? n))
      (PUSH r (big->int (big-rem n (int->big 256))))
      (set! n (big (>> n 8))))
    (list->string (map int->char (ensure-sign r #f)))
    ))

;; ----------------------------------------------------

(define b0 (dec->big "13202455343528466359104627256975295510014578435308304597954989088188026717470201290946525235856749006727963029290848782054061389728944631810577045840060851"))
(define s0 (hex->string "00fc1441e94f62e41e9f24813e10cd6cc189c21cadf8e6577b33f8fd037789736fd7572ef8a2abc174186c0f8b66aa01784e1b1fca3efc8c74adac361975dde1b3"))

(define (test-against-slow)
  (let ((sa (big->string b0))
        (sb (big->b256 b0)))
    (assert2 (string=? sa sb))
    (assert2 (big= (string->big sb) b0))
    (assert2 (big= (b256->big sb) b0))
    ))

(define (test-large)
  (assert2 (big= b0 (b256->big s0))))

(define (test-range)
  (for-range* i 1 20
    (let ((s0 (list->string (map int->char (n-of i #x2f))))
          (n0 (b256->big s0))
          (n1 (string->big s0)))
      (assert2 (big= n0 n1))
      )))

(define (test-text)
  (let ((text0 "rubber ducky")
        (text1 (b256->big text0))
        (text2 (big->b256 text1)))
    (assert2 (string=? text2 text0))
    ))

(define (test-samples)
  (assert2 (big= (b256->big "\xf3\xbb") (big (I -3141))))
  (assert2 (big= (b256->big "\xff") (big (I -1))))
  (assert2 (big= (b256->big "\xfe") (big (I -2))))
  (assert2 (big= (b256->big "\x00\xfe") (big (I 254))))
  )

;; 256-bit RSA primes: (pq p q)
(define prime-samples
  '(("00cfe0c624655bab4fbfa9a63340f87a7244e4497a188eba449428ab8630e2d47f"
     "00f6f58ae4b5c1fb569497f0a07aac4953"
     "00d77cf8542414c8827fad57c7984de6a5")
    ("00cdbc28db9b4a57018f3e353cc0edcf9421ecd7cf1aa19a34a54a42701b290fd5"
     "00fad1d82bff4986112ceab8372b1c1607"
     "00d1fbf07e91bd57e44cfaa1247a835443")
    ("00ca59782a48b2fb93ce2984d27621b64fce8644730fe6bd960d9b997ce83a26e1"
     "00fc6edfb8550408fde94cc49aa385dea3"
     "00cd356d963e1a1737883f1aca140ed0ab")
    ("00ac98e1438bb3f69ba6389ee130008467a9f2275ead785b3ede7b3b4c3836e78f"
     "00d259995bf72de1f1c14c2d0d1cc9b915"
     "00d20dd9bd160e7a3b039f4e53166a3f13")
    ("009d40dea641d0827c19e94602da9cc0b487a0814ed8a4f49ba75f9734fdbd0137"
     "00cc6f51a44e22a6fc223a8059ef502c79"
     "00c4eaf86e30b11c0b160fcf10e8c7cf2f")
    ))

(define (test-primes)
  (for-list nums prime-samples
    (match nums with
      (modulus p q)
      -> (let ((p0 (b256->big (hex->string p)))
               (q0 (b256->big (hex->string q)))
               (m0 (b256->big (hex->string modulus)))
               (p1 (b256->big (big->b256 (big-negate p0))))
               (q1 (b256->big (big->b256 (big-negate q0))))
               )
           (assert2 (big (= (* p0 q0) m0)))
           (assert2 (string=? (string->hex (big->b256 (big (* p0 q0)))) modulus))
           (assert2 (big (= (* p1 q1) m0))) ;; round-trip negative ints.
           )
      _ -> (impossible)
      )))

(test-against-slow)
(test-large)
(test-range)
(test-samples)
(test-primes)

(printf "passed " (int tests-passed) " tests.\n")
