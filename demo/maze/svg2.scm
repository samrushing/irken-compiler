;; -*- Mode: Irken -*-

;; The idea here is to have a more compact and efficient SVG
;;  representation of the maze.
;;
;; We abuse the 'marker' facility to build the maze out of a grid
;;  of three different marker types.

;; A previous version of this code used the `use` tag in a similar
;;  way, but this didn't shrink the output file nearly as much and
;;  was much slower to draw.

;; since there is redundancy, we need only deal with UL and ignore DR.
;; [The 'upper' and 'left' walls will be the 'down' and 'right' walls
;; of adjacent cells.]

;; name     bit
;;   U       3
;; L   R   1   0
;;   D       2
;;

;; there are four cell types:
;; (), (U), (L), (UL)
;;
;; and only three drawable elements:
;; U, L, UL

(define (graph-svg-gen G m n S solution)

  (define SS (format (int S)))

  ;; translate & scale
  (define (T n)
    ;; S = scale factor
    (+ S (* S n)))

  (defmacro emitf
    (emitf x ...)
    -> (emit (format x ...)))

  (makegen emit

    ;; draw a column of `name` markers along column `x` at `points`.
    (define (markers name x points)
      (when (not (null? points))
        (emitf "<g marker-start=\"url(#" name ")\"\n"
               "   marker-mid=\"url(#" name ")\"\n"
               "   marker-end=\"url(#" name ")\">\n"
               "  <polyline fill=\"none\" stroke=\"none\" points=\""
               (join (lambda (y) (format (int (T x)) " " (int (T y))))
                     "," points)
               "\"/></g>\n")))

    (emitf "<svg version=\"1.1\" width=\"" (int (+ 10 (T m)))
            "\" height=\"" (int (+ 30 (T n))) ;; extra room for text at the bottom.
            "\" xmlns=\"http://www.w3.org/2000/svg\""
            ">\n")

    ;; first let's define our three shapes...
    (emitf "<defs>\n"
            " <marker id=\"U\" viewBox=\"0 0 " SS " " SS "\"\n"
            "   markerWidth=\"" SS "\" markerHeight=\"" SS "\"\n"
            "   refX=\"0\" refY=\"0\"\n"
            "  >\n"
            "    <path fill=\"none\" stroke=\"black\" d=\"M0 0 h " SS "\"/>\n"
            "  </marker>\n"
            " <marker id=\"L\" viewBox=\"0 0 " SS " " SS "\"\n"
            "   markerWidth=\"" SS "\" markerHeight=\"" SS "\"\n"
            "   refX=\"0\" refY=\"0\"\n"
            "  >\n"
            "    <path fill=\"none\" stroke=\"black\" d=\"M0 0 v " SS "\"/>\n"
            "  </marker>\n"
            " <marker id=\"UL\" viewBox=\"0 0 " SS " " SS "\"\n"
            "   markerWidth=\"" SS "\" markerHeight=\"" SS "\"\n"
            "   refX=\"0\" refY=\"0\"\n"
            "  >\n"
            "    <path fill=\"none\" stroke=\"black\" d=\"M0 " SS " v -" SS " h " SS "\"/>\n"
            "  </marker>\n"
            "</defs>\n")

    ;; draw the maze itself...
    (emitf "<g stroke=\"black\" fill=\"none\">\n")
    (for-range x m
      (let ((u '())
            (l '())
            (ul '()))
        (for-range y n
          (let ((walls G[(+ x (* y m))]))
            (match (bit-set? walls 3) (bit-set? walls 1) with
              ;; U L
              #t #f -> (PUSH u y)
              #f #t -> (PUSH l y)
              #t #t -> (PUSH ul y)
              _ _   -> #u
              )))
        ;; draw a column of each of the markers placed.
        (markers "U" x u)
        (markers "L" x l)
        (markers "UL" x ul)
        ))

    ;; draw the boundary
    (emitf "<path d=\"M" SS " " SS
            " h " (int (* m S))
            " v " (int (* n S))
            " h " (int (- (* m S)))
            " v " (int (- (* n S)))
            "\"/>\n")

    (emitf "</g>\n")

    ;; draw the solution (if present)
    (when (not (null? solution))
      (emitf "<polyline fill=\"none\" stroke=\"red\" points=\"")
      (for-list node solution
        (let (((y x) (divmod node m))
              (x0 (+ (/ S 2) (T x)))
              (y0 (+ (/ S 2) (T y))))
          (emitf (int x0) " " (int y0) ",")))
      (emitf "\"/>\n"))

    (emitf "</svg>\n")
    ))

(define (graph->svg G m n S solution)
  (for s (graph-svg-gen G m n S solution)
    (print-string s)))

