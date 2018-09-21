;; -*- Mode: Irken -*-

;;  0  1  2  3 ...
;; 24 25 26 27 ...
;; 48 49 50 51 ...
;; ...

;; bits: UDLR
;;       3210
;; i.e., R is bit 0, L is bit 1...

;; create a fully-populated grid of walls.
(define (make-grid m n)
  ;; convert from (x,y) to node number.
  (define (coord->node x y) (+ x (* m y)))
  (let ((G (make-vector (* m n) 0)))
    (for-range x m
      (for-range y n
        (let ((edges 0))
          (define (add n)
            (set! edges (logior n edges)))
          (when (> x 0)       (add #b0010))
          (when (> y 0)       (add #b1000))
          (when (< x (- m 1)) (add #b0001))
          (when (< y (- n 1)) (add #b0100))
          (set! G[(coord->node x y)] edges)
          )))
    G ;; return the graph
    ))

(define *random-seed* 0)

(define (get-seed)
  (when (= 0 *random-seed*)
    (set! *random-seed* (logand #xffffffff (read-cycle-counter))))
  *random-seed*)

(define (make-maze m n seed bias)
  (let ((G (make-grid m n))
        (G0 (DFS G m n seed bias)))
    G0))

(define (bit-set? n i)
  (not (= (logand n (<< 1 i)) 0)))

(defmacro set-bit!
  (set-bit n i)
  -> (set! n (logior n (<< 1 i)))
  )

;; biased random 0..3.
;; -63..+63 negative numbers bias horizontal passageways - positive, vertical.
(define (biased-rand-wall seed bias)
  (let ((gen (generate-random-bits 8 seed))
        (bias0 (if (<0 bias) (- bias) bias)))
    (makegen emit
      (forever
       (match (gen) with
         (maybe:yes n)
         -> (let ((nlo (logand #b11 n)))
              (emit
               (if (= bias 0)
                   nlo
                   ;; if bias = 5, then we want 64-5/64 to be untouched.
                   (if (< (>> n 2) bias0)
                       (if (<0 bias)
                           (logand #b01 nlo) ;; horizontal passageways
                           (logior #b10 nlo)) ;; vertical passageways
                       nlo))))
         (maybe:no)
         -> (impossible)
         )))))

;; depth-first search.
(define (DFS G m n seed bias)

  ;; assumes <s> is non-zero!
  (define choose-random-edge
    ;;(let ((rng (generate-random-bits 2 seed)))
    (let ((rng (biased-rand-wall seed bias)))
      (lambda (s)
        (let loop ((mn (rng)))
          (match mn with
            (maybe:yes n)
            -> (if (bit-set? s n) n (loop (rng)))
            (maybe:no)
            -> (impossible)
            )))))

  (define rand-range
    (let ((rng (mt19937 seed)))
      (lambda (n)
        (mod (rng) n))))

  ;; return the node in a given direction.
  ;; UDLR
  (define move
    x #b0001 -> (+ x 1)
    x #b0010 -> (- x 1)
    x #b0100 -> (+ x m)
    x #b1000 -> (- x m)
    x _      -> (impossible)
    )

  ;; remove the wall[s] between `a` and `b`
  (define (remove-wall! a b)
    (define (remove a b)
      (let ((walls G[a]))
        (cond ((= b (- a m)) ;; U
               (set! walls (logxor #b1000 walls)))
              ((= b (+ a m)) ;; D
               (set! walls (logxor #b0100 walls)))
              ((= b (- a 1)) ;; L
               (set! walls (logxor #b0010 walls)))
              ((= b (+ a 1)) ;; R
               (set! walls (logxor #b0001 walls)))
              )
        (set! G[a] walls)))
    (remove a b)
    (remove b a))

  ;; body of DFS
  (let ((size (vector-length G))
        (start (rand-range size))
        (fifo (queue/make))
        ;; XXX bitfield if we really cared.
        (visited (make-vector size #f)))

    ;; which walls lead to unvisited nodes?
    (define (unvisited-from node)
      (let ((edges G[node])
            (r 0))
        (for-range i 4
          (when (bit-set? edges i)
            (let ((next (move node (<< 1 i))))
              (when (not visited[next])
                (set-bit! r i)))))
        r))

    (define (search current)
      (match (unvisited-from current) with
        0 -> (match (queue/pop! fifo) with
               (maybe:yes node) -> (search node)
               (maybe:no)       -> #u)
        n -> (let ((dir (choose-random-edge n))
                   (next (move current (<< 1 dir))))
               (queue/add! fifo current)
               (remove-wall! current next)
               (set! visited[next] #t)
               (search next))
        ))
    (search start)
    G
    ))

