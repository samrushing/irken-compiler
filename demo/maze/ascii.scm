;; -*- Mode: Irken -*-

;; each cell is 3x2 chars.

(define (graph->ascii G m n solution)

  (define (make-grid)
    (let ((O (make-vector (+ 2 (* 3 m)) #(#\space))))
      (for-range x (+ 2 (* 3 m))
        (set! O[x] (make-vector (+ 2 (* 2 n)) #\space)))
      O))

  ;; XXX I think the 'right' way to do this is to
  ;; modify the input graph so that the outer walls
  ;; are present.

  (let ((O (make-grid)))

    (define (set x y xo yo ch)
      (set! O[(+ 1 xo (* 3 x))][(+ 1 yo (* 2 y))] ch))

    (define (get x y xo yo)
      (let ((x0 (+ 1 xo (* 3 x)))
            (y0 (+ 1 yo (* 2 y))))
        (cond ((> x0 (+ 1 (* 3 m))) #\space)
              ((> y0 (+ 1 (* 2 n))) #\space)
              (else O[x0][y0]))))

    (define (draw-boundary)
      (for-range x m
        (set x 0 1 0 #\-)
        (set x 0 2 0 #\-)
        (set x n 1 0 #\-)
        (set x n 2 0 #\-))
      (for-range y n
        (set 0 y 0 0 #\|)
        (set 0 y 0 1 #\|)
        (set m y 0 0 #\|)
        (set m y 0 1 #\|)))

    (define (draw)
      (for-range y (+ 2 (* 2 n))
        (let ((line (make-vector (+ 2 (* 3 m)) #\space)))
          (for-range x (+ 2 (* 3 m))
            (set! line[x] O[x][y]))
          (printf (list->string (vector->list line)) "\n")
          )))

    (define (draw-walls)
      (for-range x m
        (for-range y n
          (let ((bits G[(+ x (* m y))]))
            ;; at each cell, we draw only U and L.
            ;; bits: UDLR
            (when (bit-set? bits 3)
              (set x y 1 0 #\-)
              (set x y 2 0 #\-))
            (when (bit-set? bits 1)
              (set x y 0 1 #\|))
            ))))

    (define (draw-intersections)
      (for-range x (+ 1 m)
        (for-range y (+ 1 n)
          (match (get x y -1 0) (get x y 1 0) (get x y 0 -1) (get x y 0 1) with
            ;;   2
            ;; 0 - 1
            ;;   3
            #\- _ #\| _  -> (set x y 0 0 #\+)
            #\- _ _ #\|  -> (set x y 0 0 #\+)
            _ #\- #\| _  -> (set x y 0 0 #\+)
            _ #\- _ #\|  -> (set x y 0 0 #\+)
            #\- #\- _ _  -> (set x y 0 0 #\-)
            #\- _ _ _    -> (set x y 0 0 #\-)
            _ #\- _ _    -> (set x y 0 0 #\-)
            _ _  #\| #\| -> (set x y 0 0 #\|)
            _ _  _ #\|   -> (set x y 0 0 #\|)
            _ _  #\| _   -> (set x y 0 0 #\|)
            _ _ _ _      -> #u
            ))))

    (draw-walls)
    (draw-boundary)
    (draw-intersections)
    (draw)
    ))

