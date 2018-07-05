;; -*- Mode: Irken -*-

;; RFC 3174
;;
;; Note: SHA-1 is no longer considered secure.
;;
;; this module is here only to support legacy protocols like websockets/rfc6455.
;;

(define (sha1 gen)

  (define (u8-at s pos)
    (char->int (string-ref s pos)))

  (define (set-u8-at! s pos n)
    (string-set! s pos (int->char (logand #xff n))))

  (define (be->u32 s pos)
    (logior*
     (<< (u8-at s (+ pos 0)) 24)
     (<< (u8-at s (+ pos 1)) 16)
     (<< (u8-at s (+ pos 2))  8)
     (<< (u8-at s (+ pos 3))  0)))

  (define (u32->be s pos n)
    (set-u8-at! s (+ pos 0) (>> n 24))
    (set-u8-at! s (+ pos 1) (>> n 16))
    (set-u8-at! s (+ pos 2) (>> n  8))
    (set-u8-at! s (+ pos 3) (>> n  0)))

  ;; input: string generator.
  ;; output: 512-bit stream padded as per SHA-1.
  (define (b512-gen gen)
    (let ((block (make-string 64))
          (total 0)
          (bpos 0)
          (need 64))

      (define (n-zeros n)
        (format (repeat n "\x00")))

      (makegen emit

        (define (feed s m?)
          (let loop ((spos 0)
                     (have (string-length s)))
            (cond ((>= have need) ;; [more than] enough
                   (buffer-copy s spos need block bpos)
                   (emit block)
                   (set! bpos 0)
                   (dec! have need)
                   (inc! spos need)
                   (set! need 64)
                   (when (> have 0)
                     (loop spos have)))
                  (else ;; not enough
                   (buffer-copy s spos have block bpos)
                   (dec! need have)
                   (inc! bpos have))))
          (when m? (inc! total (* 8 (string-length s)))))

        (for s gen (feed s #t))
        (feed "\x80" #f)
        (when (< need 8)
          (feed (n-zeros need) #f))
        (assert (> need 8))
        (let ((npad (- need 8))
              (zpad (n-zeros npad))
              (blen (n-zeros 8)))
          (feed zpad #f)
          (u32->be blen 0 (>> total 32))
          (u32->be blen 4 (logand #xffffffff total))
          (feed blen #f)
          (assert (= need 64))
          ))))

  (define (rotl32 x n)
    (logand #xffffffff (logior (<< x n) (>> x (- 32 n)))))

  (let ((h0 #x67452301)
        (h1 #xefcdab89)
        (h2 #x98badcfe)
        (h3 #x10325476)
        (h4 #xc3d2e1f0)
        (w (make-vector 80 0)))

    (define (mask n) (logand #xffffffff n))
    (define (f0 b c d) (logior (logand b c) (logand (lognot b) d)))
    (define (f1 b c d) (logxor* b c d))
    (define (f2 b c d) (logior* (logand b c) (logand b d) (logand c d)))

    (for block (b512-gen gen)
      (for-range i 16
        (set! w[i] (be->u32 block (* i 4))))
      (for-range* i 16 80
        (set! w[i] (rotl32 (logxor* w[(- i 3)] w[(- i 8)] w[(- i 14)] w[(- i 16)]) 1)))
      (let ((a h0) (b h1) (c h2) (d h3) (e h4) (tmp 0))
        (define (round lo hi f k)
          (for-range* i lo hi
            (set! tmp (mask (+ (rotl32 a 5) (f b c d) e w[i] k)))
            (set! e d) (set! d c) (set! c (rotl32 b 30)) (set! b a) (set! a tmp)
            ))
        (round  0 20 f0 #x5a827999)
        (round 20 40 f1 #x6ed9eba1)
        (round 40 60 f2 #x8f1bbcdc)
        (round 60 80 f1 #xca62c1d6)
        (set! h0 (mask (+ h0 a)))
        (set! h1 (mask (+ h1 b)))
        (set! h2 (mask (+ h2 c)))
        (set! h3 (mask (+ h3 d)))
        (set! h4 (mask (+ h4 e)))))

    (let ((r (make-string 20)))
      (u32->be r  0 h0)
      (u32->be r  4 h1)
      (u32->be r  8 h2)
      (u32->be r 12 h3)
      (u32->be r 16 h4)
      r)))

