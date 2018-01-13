;; -*- Mode: Irken -*-

;; a faithful transcription of the code from NaCl.
;;
;; the main difference is that we're using 'native' Irken 63-bit signed integers,
;;   so we need to do some masking in places.

(defmacro XOR
  (XOR a)         -> a
  (XOR a b)       -> (logxor a b)
  (XOR a b c ...) -> (logxor a (XOR b c ...))
  )
(defmacro IOR
  (IOR a)         -> a
  (IOR a b)       -> (logior a b)
  (IOR a b c ...) -> (logior a (IOR b c ...))
  )
(defmacro AND
  (AND a b)      -> (logand a b))
(defmacro MASK
  (MASK n)       -> (AND #xffffffff n))
(defmacro SHR
  (SHR x c)      -> (>> x c))
(defmacro ROTR
  (ROTR x c)     -> (logior (>> x c) (<< x (- 32 c))))
(defmacro Ch
  (Ch x y z)     -> (XOR (AND x y) (AND (lognot x) z)))
(defmacro Maj
  (Maj x y z)    -> (XOR (AND x y) (AND x z) (AND y z)))
(defmacro Sigma0
  (Sigma0 x)     -> (XOR (ROTR x  2) (ROTR x 13) (ROTR x 22)))
(defmacro Sigma1
  (Sigma1 x)     -> (XOR (ROTR x  6) (ROTR x 11) (ROTR x 25)))
(defmacro sigma0
  (sigma0 x)     -> (XOR (ROTR x  7) (ROTR x 18) (SHR x 3)))
(defmacro sigma1
  (sigma1 x)     -> (XOR (ROTR x 17) (ROTR x 19) (SHR x 10)))
(defmacro M
  (M w0 w14 w9 w1)
  -> (set! w0 (MASK (+ (sigma1 w14) w9 (sigma0 w1) w0))))
(defmacro EXPAND
  (EXPAND)
  -> (begin
       (M w0  w14 w9  w1 )
       (M w1  w15 w10 w2 )
       (M w2  w0  w11 w3 )
       (M w3  w1  w12 w4 )
       (M w4  w2  w13 w5 )
       (M w5  w3  w14 w6 )
       (M w6  w4  w15 w7 )
       (M w7  w5  w0  w8 )
       (M w8  w6  w1  w9 )
       (M w9  w7  w2  w10)
       (M w10 w8  w3  w11)
       (M w11 w9  w4  w12)
       (M w12 w10 w5  w13)
       (M w13 w11 w6  w14)
       (M w14 w12 w7  w15)
       (M w15 w13 w8  w0 )))
(defmacro F
  (F w k)
  -> (begin
       (set! T1 (MASK (+ h (Sigma1 e) (Ch e f g) k w)))
       (set! T2 (MASK (+ (Sigma0 a) (Maj a b c))))
       (set! h g)
       (set! g f)
       (set! f e)
       (set! e (MASK (+ d T1)))
       (set! d c)
       (set! c b)
       (set! b a)
       (set! a (MASK (+ T1 T2)))
       ))

(define sha256-iv
  (format "\x6a\x09\xe6\x67\xbb\x67\xae\x85\x3c\x6e\xf3\x72\xa5\x4f\xf5\x3a"
          "\x51\x0e\x52\x7f\x9b\x05\x68\x8c\x1f\x83\xd9\xab\x5b\xe0\xcd\x19"))

(define (make-sha256)

  (let ((H {state = (copy-string sha256-iv 32)
            left = (make-string 64)
            len = 0
            total = 0
            }))

    (define (GS s i)
      (char->int (string-ref s i)))

    (define (load-bigendian x off)
      (IOR
       (<< (GS x (+ 3 off))  0)
       (<< (GS x (+ 2 off))  8)
       (<< (GS x (+ 1 off)) 16)
       (<< (GS x (+ 0 off)) 24)))

    (define (SS! s i n)
      (string-set! s i (int->char n)))

    (define (store-bigendian s off n)
      (for-range i 4
        (SS! s (+ off 3) (AND #xff (>> n  0)))
        (SS! s (+ off 2) (AND #xff (>> n  8)))
        (SS! s (+ off 1) (AND #xff (>> n 16)))
        (SS! s (+ off 0) (AND #xff (>> n 24)))))

    (define (sha256-hashblocks statebytes in inoff inlen)
      (let ((state (make-vector 8 0))
            (a 0)
            (b 0)
            (c 0)
            (d 0)
            (e 0)
            (f 0)
            (g 0)
            (h 0)
            (T1 0)
            (T2 0))
        (set! a (load-bigendian statebytes  0)) (set! state[0] a)
        (set! b (load-bigendian statebytes  4)) (set! state[1] b)
        (set! c (load-bigendian statebytes  8)) (set! state[2] c)
        (set! d (load-bigendian statebytes 12)) (set! state[3] d)
        (set! e (load-bigendian statebytes 16)) (set! state[4] e)
        (set! f (load-bigendian statebytes 20)) (set! state[5] f)
        (set! g (load-bigendian statebytes 24)) (set! state[6] g)
        (set! h (load-bigendian statebytes 28)) (set! state[7] h)
        (while (>= inlen 64)
          (let ((w0  (load-bigendian in (+ inoff 0)))
                (w1  (load-bigendian in (+ inoff 4)))
                (w2  (load-bigendian in (+ inoff 8)))
                (w3  (load-bigendian in (+ inoff 12)))
                (w4  (load-bigendian in (+ inoff 16)))
                (w5  (load-bigendian in (+ inoff 20)))
                (w6  (load-bigendian in (+ inoff 24)))
                (w7  (load-bigendian in (+ inoff 28)))
                (w8  (load-bigendian in (+ inoff 32)))
                (w9  (load-bigendian in (+ inoff 36)))
                (w10 (load-bigendian in (+ inoff 40)))
                (w11 (load-bigendian in (+ inoff 44)))
                (w12 (load-bigendian in (+ inoff 48)))
                (w13 (load-bigendian in (+ inoff 52)))
                (w14 (load-bigendian in (+ inoff 56)))
                (w15 (load-bigendian in (+ inoff 60))))

            (F w0  #x428a2f98)
            (F w1  #x71374491)
            (F w2  #xb5c0fbcf)
            (F w3  #xe9b5dba5)
            (F w4  #x3956c25b)
            (F w5  #x59f111f1)
            (F w6  #x923f82a4)
            (F w7  #xab1c5ed5)
            (F w8  #xd807aa98)
            (F w9  #x12835b01)
            (F w10 #x243185be)
            (F w11 #x550c7dc3)
            (F w12 #x72be5d74)
            (F w13 #x80deb1fe)
            (F w14 #x9bdc06a7)
            (F w15 #xc19bf174)

            (EXPAND)

            (F w0  #xe49b69c1)
            (F w1  #xefbe4786)
            (F w2  #x0fc19dc6)
            (F w3  #x240ca1cc)
            (F w4  #x2de92c6f)
            (F w5  #x4a7484aa)
            (F w6  #x5cb0a9dc)
            (F w7  #x76f988da)
            (F w8  #x983e5152)
            (F w9  #xa831c66d)
            (F w10 #xb00327c8)
            (F w11 #xbf597fc7)
            (F w12 #xc6e00bf3)
            (F w13 #xd5a79147)
            (F w14 #x06ca6351)
            (F w15 #x14292967)

            (EXPAND)

            (F w0  #x27b70a85)
            (F w1  #x2e1b2138)
            (F w2  #x4d2c6dfc)
            (F w3  #x53380d13)
            (F w4  #x650a7354)
            (F w5  #x766a0abb)
            (F w6  #x81c2c92e)
            (F w7  #x92722c85)
            (F w8  #xa2bfe8a1)
            (F w9  #xa81a664b)
            (F w10 #xc24b8b70)
            (F w11 #xc76c51a3)
            (F w12 #xd192e819)
            (F w13 #xd6990624)
            (F w14 #xf40e3585)
            (F w15 #x106aa070)

            (EXPAND)

            (F w0  #x19a4c116)
            (F w1  #x1e376c08)
            (F w2  #x2748774c)
            (F w3  #x34b0bcb5)
            (F w4  #x391c0cb3)
            (F w5  #x4ed8aa4a)
            (F w6  #x5b9cca4f)
            (F w7  #x682e6ff3)
            (F w8  #x748f82ee)
            (F w9  #x78a5636f)
            (F w10 #x84c87814)
            (F w11 #x8cc70208)
            (F w12 #x90befffa)
            (F w13 #xa4506ceb)
            (F w14 #xbef9a3f7)
            (F w15 #xc67178f2)

            (set! a (MASK (+ a state[0])))
            (set! b (MASK (+ b state[1])))
            (set! c (MASK (+ c state[2])))
            (set! d (MASK (+ d state[3])))
            (set! e (MASK (+ e state[4])))
            (set! f (MASK (+ f state[5])))
            (set! g (MASK (+ g state[6])))
            (set! h (MASK (+ h state[7])))

            (set! state[0] a)
            (set! state[1] b)
            (set! state[2] c)
            (set! state[3] d)
            (set! state[4] e)
            (set! state[5] f)
            (set! state[6] g)
            (set! state[7] h))

          (inc! inoff 64)
          (dec! inlen 64))

        (store-bigendian statebytes  0 state[0])
        (store-bigendian statebytes  4 state[1])
        (store-bigendian statebytes  8 state[2])
        (store-bigendian statebytes 12 state[3])
        (store-bigendian statebytes 16 state[4])
        (store-bigendian statebytes 20 state[5])
        (store-bigendian statebytes 24 state[6])
        (store-bigendian statebytes 28 state[7])

        ))

    (define (update buf)
      (let ((blen (string-length buf))
            (off 0))
        ;; H may contain 0..63 bytes of unprocessed data
        ;; do we have enough bytes to feed at least one block?
        (if (>= (+ blen H.len) 64)
            ;; do we need to copy some bytes to H.left?
            (if (> H.len 0)
                (let ((rem (- 64 H.len)))
                  (buffer-copy buf 0 rem H.left H.len)
                  ;; feed one full block
                  (sha256-hashblocks H.state H.left 0 64)
                  (inc! H.total 64)
                  (inc! off rem)))
            ;; not enough bytes to feed, append to H.left
            (begin
              (buffer-copy buf 0 blen H.left H.len)
              (inc! H.len blen)
              (set! off blen)))
        ;; anything remaining to feed?
        (when (< off blen)
          (let ((nrem (- blen off))
                ((q r) (divmod nrem 64)))
            ;; feed in `q` blocks
            (sha256-hashblocks H.state buf off (* q 64))
            (inc! H.total (* q 64))
            (if (> r 0)
                (begin
                  (set! H.len r)
                  (buffer-copy buf (* q 64) r H.left 0))
                ;; nothing remains
                (set! H.len 0))))
        ))

    (define (digest)
      (let ((out (make-string 32))
            (padded (make-string 128))
            (total (+ H.len H.total))
            (bits (<< total 3)))
        (for-range i H.len
          (string-set! padded i (string-ref H.left i)))
        (SS! padded H.len #x80)
        (if (< H.len 56)
            (let ((i (+ H.len 1)))
              (while (< i 56)
                (SS! padded i 0)
                (inc! i))
              (SS! padded 56 (AND #xff (>> bits 56)))
              (SS! padded 57 (AND #xff (>> bits 48)))
              (SS! padded 58 (AND #xff (>> bits 40)))
              (SS! padded 59 (AND #xff (>> bits 32)))
              (SS! padded 60 (AND #xff (>> bits 24)))
              (SS! padded 61 (AND #xff (>> bits 16)))
              (SS! padded 62 (AND #xff (>> bits  8)))
              (SS! padded 63 (AND #xff (>> bits  0)))
              (sha256-hashblocks H.state padded 0 64))
            (let ((i (+ H.len 1)))
              (while (< i 120)
                (SS! padded i 0)
                (inc! i))
              (SS! padded 120 (AND #xff (>> bits 56)))
              (SS! padded 121 (AND #xff (>> bits 48)))
              (SS! padded 122 (AND #xff (>> bits 40)))
              (SS! padded 123 (AND #xff (>> bits 32)))
              (SS! padded 124 (AND #xff (>> bits 24)))
              (SS! padded 125 (AND #xff (>> bits 16)))
              (SS! padded 126 (AND #xff (>> bits  8)))
              (SS! padded 127 (AND #xff (>> bits  0)))
              (sha256-hashblocks H.state padded 0 128)
              ))
        ;; clear things up...
        (set! H.total total)
        (set! H.left "")
        (set! H.len 0)
        (for-range i 32
          (string-set! out i (string-ref H.state i)))
        out
        ))

    (define (total)
      H.total
      )

    ;; this record is our interface: 3 functions.
    {update=update digest=digest total=total}

    ))
