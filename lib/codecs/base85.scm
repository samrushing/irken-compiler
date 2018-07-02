;; -*- Mode: Irken -*-

;; https://en.wikipedia.org/wiki/Ascii85
;; base85 is a 4->5 encoding

;; we implement this as two sets of codecs ...
;;
;;   1) a uint32 <=> char[4] codec
;;   2) a char[5] <=> uint32 codec
;;
;; ... which are chained together.

;; since there are several approaches to padding in the
;;  various 'standards', this issue is left to the user.
;; [hint: use padding/unpadding generators]

;; Note: no support for the special meanings for the 'z'
;;  and 'y' chars.

;; char * 4 -> uint32
(define (ch->u32 gen)
  (let ((v 0)
        (n 0))
    (makegen emit
      (for ch gen
        (set! v (logior (<< v 8) (char->int ch)))
        (inc! n)
        (when (= n 4)
          (emit v)
          (set! v 0)
          (set! n 0))))))

;; uint32 -> char * 4
(define (u32->ch gen)
  (makegen emit
    (define (put n)
      (emit (int->char (logand #xff n))))
    (for val gen
      (put (>> val 24))
      (put (>> val 16))
      (put (>> val  8))
      (put (>> val  0))
      )))

;; uint32 -> char * 5
(define (u32->b85 gen32)
  (let ((vals (list:nil)))
    (makegen emit
      (for val gen32
        (for-range i 5
          (PUSH vals (int->char (+ 33 (mod val 85))))
          (set! val (/ val 85)))
        (for-each emit vals)
        (set! vals (list:nil))
        ))))

;; char * 5 -> uint32
(define (b85->u32 gen)
  (let ((val 0)
        (n 0))
    (makegen emit
      (for ch gen
        (set! val (+ (* val 85) (- (char->int ch) 33)))
        (inc! n)
        (when (= n 5)
          (emit val)
          (set! val 0)
          (set! n 0))
        ))))

(define (b85-enc gen)
  (u32->b85 (ch->u32 gen)))

(define (b85-dec gen)
  (u32->ch (b85->u32 gen)))

(define (string->b85 s)
  (let ((result (list:nil)))
    (for ch (b85-enc (string-generator s))
      (PUSH result ch))
    (list->string result)))

(define (b85->string s)
  (let ((result (list:nil)))
    (for ch (b85-dec (string-generator s))
      (PUSH result ch))
    (list->string result)))

