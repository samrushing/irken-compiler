;; -*- Mode: Irken -*-

;; https://en.wikipedia.org/wiki/Mersenne_Twister

;; note: this assumes a 64-bit machine, where we can safely
;;   do full 32-bit arithmetic.

(define (mt19937 seed)

  (define (init seed)
    (let ((state {buffer=(make-vector 624 0) pos=625}))
      (set! state.buffer[0] seed)
      (for-range* i 1 624
        (let ((x0 state.buffer[(- i 1)])
              (x1 (logand #xffffffff (+ i (* 1812433253 (logxor x0 (>> x0 30)))))))
          (set! state.buffer[i] x1)))
      state))

  (let ((state (init seed)))

    (define (twist)
      (for-range i 624
        (let ((x0 state.buffer[i])
              (x1 state.buffer[(mod (+ i 1) 624)])
              (x (logior (logand x0 #x80000000) (logand x1 #x7fffffff)))
              (xa (if (odd? x) (logxor (>> x 1) #x9908b0df) (>> x 1))))
          (set! state.buffer[i] (logxor state.buffer[(mod (+ i 397) 624)] xa))
          ))
      (set! state.pos 0))

    (define (fetch)
      (if (>= state.pos 624)
          (twist))
      (set! state.pos (+ state.pos 1))
      state.buffer[(- state.pos 1)])

    (define (rand)
      (let ((y (fetch)))
        (set! y (logxor y (>> y 11)))
        (set! y (logxor y (logand (<< y 7)  #x9d2c5680)))
        (set! y (logxor y (logand (<< y 15) #xefc60000)))
        (set! y (logxor y (>> y 18)))
        (logand y #xffffffff)
        ))
    rand
    ))

(define (mt19937-generator seed num)
  (makegen emit
    (let ((rng (mt19937 seed)))
      (for-range i num
        (emit (rng))))))

;; assumes nbits is .LE. word size
;; convert a 32-bit generator into an `nbits`-generator.
(define (generate-random-bits nbits seed)
  ;; bit buffer record := {val=int have=int width=int}
  (define (buf/new width)
    (assert (< width (* 8 (get-word-size))))
    {val=0 have=0 width=width})
  (define (buf/push buf val bits)
    (set! buf.val (logior (<< buf.val bits) (logand val (- (<< 1 bits) 1))))
    (inc! buf.have bits)
    (assert (<= buf.have buf.width)))
  (define (buf/pull buf bits)
    (assert (<= bits buf.have))
    (let ((r (>> buf.val (- buf.have bits)))
          (left (- buf.have bits)))
      (set! buf.val (logand buf.val (- (<< 1 left) 1)))
      (set! buf.have left)
      r))
  (let ((rng (mt19937 seed))
        (ibuf (buf/new 32))
        (obuf (buf/new nbits)))
    (makegen emit
      (forever
       (while (< obuf.have nbits)
         (let ((need (- nbits obuf.have)))
           (if (= ibuf.have 0) (buf/push ibuf (rng) 32)) ;; refill
           (let ((chunk (min need ibuf.have)))
             (buf/push obuf (buf/pull ibuf chunk) chunk))))
       (emit (buf/pull obuf nbits))
       ))))
