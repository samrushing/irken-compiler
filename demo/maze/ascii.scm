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

;; --- random output formats ---

;; note: this is still twice as large as it needs to be,
;;  since there is redundancy in the wall flags. I think
;;  we can cut this in half.
(define (graph->hex G m n)
  (for-range y n
    (for-range x m
      (printf (zpad 1 (hex G[(+ x (* m y))]))))
    (printf "\n")))

(define (bytegen G m n)
  (makegen emit
    (let ((byte 0)
          (bits 0))
      (for-range y n
        (for-range x m
          (let ((wall G[(+ x (* y m))])
                (v (match (bit-set? wall 3) (bit-set? wall 1) with
                     #t #t -> 3
                     #t #f -> 2
                     #f #t -> 1
                     #f #f -> 0)))
            (set! byte (logior (<< byte 2) v))
            (inc! bits 2)
            (when (= 8 bits)
              (emit (int->char byte))
              (set! byte 0)
              (set! bits 0))
            )))
      (when (> bits 0)
        (emit (int->char byte)))
      )))

;; the most compact (ASCII) encoding available.
;; we use 2 bits per cell, pack into 8-bit
;; strings and base85-encode.

(define (graph->base85 G m n)
  (let ((result '()))
    (for ch (b85-enc (bytegen G m n))
      (push! result ch))
    (printf (list->string (reverse result)) "\n")
    ))

(define (graph->base64 G m n)
  (let ((result '()))
    (for ch (b64-enc (bytegen G m n))
      (push! result ch))
    (printf (list->string (reverse result)) "\n")
    ))

;; raw binary, 2 bits per cell.
;; to use: (graph->bin "/tmp/maze.bin" maze w h)
(define (graph->bin path G m n)
  (let ((file (stdio/open-write path))
        (buffer (make-string 8192))
        (bytes 0))
    (for ch (bytegen G m n)
      (string-set! buffer bytes ch)
      (inc! bytes)
      (when (= bytes (string-length buffer))
        (stdio/write file buffer)
        (set! bytes 0)))
    (when (> bytes 0)
      (stdio/write file (substring buffer 0 bytes))
      #u
      )
    ))
