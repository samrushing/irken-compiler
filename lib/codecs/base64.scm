;; -*- Mode: Irken -*-

(define *base64-digits* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(define *base64-reverse*
  (let ((v (make-vector 256 -1)))
    (for-range i 64
      (set! v[(char->ascii (string-ref *base64-digits* i))] i))
    v))

;; we create a base64 codec by combining two sub-codecs.
;; the intermediate codec converts 3 bytes to a 26-bit integer:
;; AABBCCN made of 3 8-bit bytes with a 2-bit suffix containing
;; the number of bytes present.  If there are fewer than 3 bytes,
;; they are left-justified (i.e., the first byte is always bits 16-24).
;;
;; given a ch<->u26 codec, we now combine this with a u26<->b64 codec.
;;
;; two codecs are simpler than a single codec, because each codec need
;;   only pay attention to its part of the stream state, rather than a
;;   combination of both.

;; char * 3 -> u26 * 1
(define (ch->u26 gen)
  (let ((v 0)
        (n 0))
    (makegen emit
      (for ch gen
        (set! v (logior (<< v 8) (char->int ch)))
        (inc! n)
        (when (= n 3)
          (emit (logior (<< v 2) 3))
          (set! v 0)
          (set! n 0)))
      (when (not (= 0 n))
        (emit (logior (<< v (+ 2 (* 8 (- 3 n)))) n)))
      )))

;; u26 * 1 -> char * 3
(define (u26->ch gen)
  (makegen emit
    (define (put n shift)
      (emit (int->char (logand #xff (>> n shift)))))
    (for val gen
      (let ((n (logand 3 val))
            (v (>> val 2)))
        (for-range i n
          (put v (- 16 (* 8 i))))
        ))))

;; u26 * 1 -> b64 * 4
(define (u26->b64 gen26)
  (let ((vals (list:nil)))
    (makegen emit
      (for val gen26
        (let ((n (how-many (* 8 (logand 3 val)) 6))
              (v (>> val 2)))
          (for-range i 4
            (push! vals (string-ref *base64-digits* (logand #x3f v)))
            (set! v (>> v 6)))
          (for-each emit (take vals n))
          (set! vals (list:nil))
          ;; emit padding if necessary.
          (match n with
            3 -> (emit #\=)
            2 -> (begin (emit #\=) (emit #\=))
            _ -> #u
            )))
      )))

;; b64 * 4 -> u26 * 1
;; 00,11,22,33 -> AABBCC.N
(define (b64->u26 genb64)
  (let ((val 0)
        (n 0))
    (makegen emit
      (for ch genb64
        (let ((ch0 *base64-reverse*[(char->int ch)]))
          (when (>= ch0 0)
            (set! val (logior (<< val 6) ch0))
            (inc! n)
            (when (= n 4)
              (emit (logior (<< val 2) 3))
              (set! val 0)
              (set! n 0))
            )))
      (match n with
        3 -> (emit (logior (<< val  8) 2))
        2 -> (emit (logior (<< val 14) 1))
        _ -> #u
        )
      )))

(define (b64-enc gen)
  (u26->b64 (ch->u26 gen)))

(define (b64-dec gen)
  (u26->ch (b64->u26 gen)))

(define (b64-decode s)
  (let ((dst (make-string (* 3 (how-many (string-length s) 4))))
        (i 0))
    (for ch (b64-dec (string-generator s))
      (string-set! dst i ch)
      (inc! i))
    (if (= i (string-length dst))
        dst
        (substring dst 0 i))))

(define (b64-encode s)
  (let ((dst (make-string (* 4 (how-many (string-length s) 3))))
        (i 0))
    (for ch (b64-enc (string-generator s))
      (string-set! dst i ch)
      (inc! i))
    dst))
