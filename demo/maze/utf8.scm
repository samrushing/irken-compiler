;; -*- Mode: Irken -*-

;;
;; our boxes are stored in the graph thus:
;;     U
;;   +---+
;; L |   | R
;;   +---+
;;     D
;;
;; where the cell data represents the walls.
;; [with redundancy]

;; but the box-drawing characters represent the
;; _intersections_.
;;
;;      |
;;    --+--
;;      |
;;
;; so the information needed at each point comes
;;  from the FOUR surrounding boxes.
;;
;; given an m x n array of _boxes_, we need an
;; (m+1) x (n+1) array of _corners_ (without redundancy)
;;
;;
;; to draw this intersection:
;;
;;     A|B
;;    --+--
;;     D|C
;;
;; we need access to a pair of boxes, either A,C or B,D
;; let's do A,C.
;;
;;    AR|
;;    D |
;;   ---+---
;;      | U
;;      |LC
;;
;; there are 16 cases. let's number the bits UDLR as usual.
;;
;; 0000
;; 0001 ╶
;; 0010 ╴
;; 0011 ─
;; 0100 ╷
;; 0101 ┌
;; 0110 ┐
;; 0111 ┬
;; 1000 ╵
;; 1001 └
;; 1010 ┘
;; 1011 ┴
;; 1100 │
;; 1101 ├
;; 1110 ┤
;; 1111 ┼

;; demo/maze/maze -utf8 -seed 12 16 16
;; ┌───┬─────┬───────┬─────┬───────┐
;; │ ╷ ╵ ╶─┬─┘ ┌─────┤ ┌─┐ │ ╶───┐ │
;; ├─┴─┬─╴ ╵ ╶─┘ ┌─┐ │ ╵ │ ├───╴ │ │
;; │ ╷ └───────╴ │ │ │ ┌─┘ │ ╶───┤ │
;; │ └─┬───╴ ┌───┘ │ └─┘   └───┐ │ │
;; ├─╴ ├─╴ ┌─┘ ┌───┴─╴ ╷ ╷ ╶─┐ └─┤ │
;; ├───┘ ╷ │ ╷ ╵ ┌─┬───┘ │ ╷ └─┐ │ │
;; │ ╶─┬─┴─┤ │ ┌─┘ │ ╶───┤ │ ╷ │ ╵ │
;; ├─╴ │ ╷ └─┤ │ ╶─┴───╴ │ │ │ ├───┤
;; │ ┌─┘ ├─┐ └─┴───┬───┬─┘ │ │ └─┐ │
;; │ ╵ ╶─┤ │ ╷ ╶─┐ │ ╷ ├─╴ │ └─┐ ╵ │
;; │ ┌───┘ ├─┴─╴ │ │ │ ╵ ┌─┴─╴ ├───┤
;; │ │ ╶───┤ ╶───┴─┤ └───┼─────┤ ╷ │
;; │ └───┐ │ ┌─┬─╴ ├───╴ │ ╷ ╶─┘ │ │
;; │ ┌───┘ │ │ │ ┌─┘ ┌───┤ ├─────┘ │
;; │ ╵ ╶───┤ ╵ │ └───┘ ╷ ╵ │ ╶─────┤
;; └───────┴───┴───────┴───┴───────┘


(define intersections #(" " "╶" "╴" "─" "╷" "┌" "┐" "┬" "╵" "└" "┘" "┴" "│" "├" "┤" "┼"))

(define (graph-utf8-gen G m n)

  (define (walls-at x y)
    ;; this handles queries one cell outside the borders
    ;;  in order to calculate the intersections.
    (match (< x 0) (< y 0) (= x m) (= y n) with
      #f #f #f #f  -> G[(+ x (* y m))]
      #t #f #f #f  -> #b0001 ;; left wall
      #f #t #f #f  -> #b0100 ;; top wall
      #f #f #t #f  -> #b0010 ;; right wall
      #f #f #f #t  -> #b1000 ;; bottom wall
      _  _  _  _   -> #b0000 ;; corners
      ))

  (define (bools->int bools)
    (define bool->int
      #t -> 1
      #f -> 0
      )
    (fold (lambda (x a) (logior (bool->int x) (<< a 1))) 0 bools)
    )

  (define (inter-at x y)
    (let ((A (walls-at x y))
          (C (walls-at (+ x 1) (+ y 1)))
          (U (bit-set? A 0))
          (D (bit-set? C 1))
          (L (bit-set? A 2))
          (R (bit-set? C 3)))
      (:tuple (bools->int (list U D L R)) R)
      ))

  (add-outer-walls G m n)
  (makegen emit
    (for-range y (+ n 1)
      (let ((line (list:nil)))
        (for-range x (+ m 1)
          (let (((val r?) (inter-at (- x 1) (- y 1)))
                (s intersections[val]))
            ;; little hack to make each cell two chars wide,
            ;;  which gives a relatively square aspect ratio.
            (push! line (format s (if r? "─" " ")))))
        ;; emit one line at a time.
        (emit (string-concat (reverse (list:cons "\r\n" line))))))
    ))

;; the inverse or dual would be the tree represented by the clear
;;  paths through the maze.  This is of course much easier to draw.

(define (graph-utf8-dual-gen G m n)

  (define (invert n)
    (logxor #b1111 n))

  (add-outer-walls G m n)
  (makegen emit
    (for-range y n
      (let ((line (list:nil)))
        (for-range x m
          (let ((n (invert G[(+ x (* y m))]))
                (r? (bit-set? n 0)))
            (push! line (format intersections[n] (if r? "─" " ")))
            ))
        (emit (string-concat (reverse (list:cons "\r\n" line)))))
      )))

(define (graph->utf8 G m n)
  (for s (graph-utf8-gen G m n)
    (print-string s)))

(define (graph->tree G m n)
  (for s (graph-utf8-dual-gen G m n)
    (print-string s)))
