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
