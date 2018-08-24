;; -*- Mode: Irken -*-

(define (graph->svg G m n S solution)

  ;; translate & scale
  (define (T n)
    ;; S = scale factor
    (+ S (* S n)))

  ;; translate & scale a point.
  (define (Tp p)
    {x=(T p.x) y=(T p.y)})

  (define (node->point n)
    (let (((y x) (divmod n m)))
      {x=(T x) y=(T y)}))

  ;; assumes: a < b
  (define (wall a b)
    (let (((y0 x0) (divmod a m)) ;; convert from node number to (x,y)
          ((y1 x1) (divmod b m))
          ;; translate all the coordinates.
          (x2 (T x0))
          (y2 (T y0))
          (x3 (T x1))
          (y3 (T y1)))
      (cond ((= x2 x3) ;; vertical connection, horizontal wall
             (:tuple {x=x2 y=(+ y2 S)} {x=(+ x2 S) y=(+ y2 S)}))
            ((= y2 y3) ;; horizontal connection, vertical wall
             (:tuple {x=(+ x2 S) y=y2} {x=(+ x2 S) y=(+ y2 S)}))
            (else
             (impossible))
            )))

  (define (polyline path)
    (printf "<polyline fill=\"none\" stroke=\""
            ;;(alternating-color)
            "black"
            "\" points=\""
            (join point-repr "," (simplify-path (list:nil) path))
            "\"/>\n"))

  (define (polyline* path)
    (polyline (map Tp path)))

  (define move
    x #b0001 -> (+ x 1)
    x #b0010 -> (- x 1)
    x #b0100 -> (+ x m)
    x #b1000 -> (- x m)
    x _      -> (impossible)
    )

  (printf "<svg version=\"1.1\" width=\"" (int (+ 10 (T m)))
          "\" height=\"" (int (+ 30 (T n)))
          "\" xmlns=\"http://www.w3.org/2000/svg\""
          ">\n")
  (let ((pj (path-joiner)))
    (for-range x m
      (for-range y n
        (let ((node (+ x (* m y)))
              (walls G[node]))
          (for-range i 4
            (when (bit-set? walls i)
              (let ((next (move node (<< 1 i))))
                (when (< node next)
                  (let (((fp tp) (wall node next)))
                    (pj.add fp tp)))))))))
    (for-map k v (pj.get)
      (polyline v.path)))
  ;; draw the boundary
  (polyline* (LIST {x=0 y=0} {x=m y=0} {x=m y=n} {x=0 y=n} {x=0 y=0}))
  (printf "<text x=\"40\" y=\"" (int (+ 20 (T n))) "\" class=\"small\">"
          (int m) "x" (int n)
          " seed " (int *random-seed*) "</text>\n")
  ;; draw the solution (if present)
  (when (not (null? solution))
    (printf "<polyline fill=\"none\" stroke=\"red\" points=\"")
    (for-list node solution
      (let (((y x) (divmod node m))
            (x0 (+ (/ S 2) (T x)))
            (y0 (+ (/ S 2) (T y))))
        (printf (int x0) " " (int y0) ",")))
    (printf "\"/>\n"))
  (printf "</svg>\n"))

(define alternating-color
  (let ((i -1)
        (colors (LIST "red" "orange" "yellow" "green" "blue" "indigo" "violet")))
    (lambda ()
      (set! i (mod (+ i 1) 7))
      (nth colors i)
      )))

(typealias point {x=int y=int})
(typealias path {s=point e=point path=(list point)})

(define (point=? p0 p1)
  (eq? (cmp:=) (magic-cmp p0 p1)))

(define (point-repr p) : (point -> string)
  (format (int p.x) " " (int p.y)))

(define (path-repr p) : (path -> string)
  (format "{s=" (point-repr p.s) " e=" (point-repr p.e)
          " path=(" (join point-repr "," p.path) ")}"))

;; join vertical/horizontal runs
(define simplify-path
  acc ()
  -> acc
  acc (p0 p1 p2 . tl)
  -> (if (or (and (= p0.x p1.x) (= p0.x p2.x))  ;; vertical line
             (and (= p0.y p1.y) (= p0.y p2.y))) ;; horizontal line
         (simplify-path acc (list:cons p0 (list:cons p2 tl)))
         (simplify-path (list:cons p0 acc) (list:cons p1 (list:cons p2 tl))))
  acc (hd . tl)
  -> (simplify-path (list:cons hd acc) tl)
  )

;; Take in individual line segments and join together their ends (when
;;   possible) to create longer paths.  This makes the SVG output much
;;   smaller - about 1/4 of the size.

(define (path-joiner)
  (let ((fwdmap (tree/empty))
        (revmap (tree/empty)))

    (define (add-line p0 p1)
      (add-path {s=p0 e=p1 path=(LIST p0 p1)}))

    (define (add-path path0)
      (match (tree/member fwdmap magic-cmp path0.e) with
        (maybe:yes path1) ;; s0 ... e0=s1 ... e1
        -> (add-path (merge-se path0 path1))
        (maybe:no)
        -> (match (tree/member revmap magic-cmp path0.s) with
             (maybe:yes path1) ;; s1 ... e1=s0 ... e0
             -> (add-path (merge-se path1 path0))
             (maybe:no)
             -> (match (tree/member fwdmap magic-cmp path0.s) with
                  (maybe:yes path1) ;; e1 ... s1=s0 ... e0
                  -> (add-path (merge-ss path0 path1))
                  (maybe:no)
                  -> (match (tree/member revmap magic-cmp path0.e) with
                       (maybe:yes path1) ;; s0 ... e0=e1 ... s1
                       -> (add-path (merge-ee path0 path1))
                       (maybe:no)
                       -> (begin
                            (tree/insert! fwdmap magic-cmp path0.s path0)
                            (tree/insert! revmap magic-cmp path0.e path0))
                       )))))

    (define (delete-path! path)
      (tree/delete! fwdmap magic-cmp path.s)
      (tree/delete! revmap magic-cmp path.e)
      )

    (define (merge-se path0 path1)
      ;; s0 ... e0=s1 ... e1
      (delete-path! path0)
      (delete-path! path1)
      {s=path0.s e=path1.e path=(append path0.path (rest path1.path))}
      )

    (define (merge-ss path0 path1)
      ;; e1 ... s1=s0 ... e0
      (delete-path! path0)
      (delete-path! path1)
      {s=path1.e e=path0.e path=(append (reverse path1.path) (rest path0.path))}
      )

    (define (merge-ee path0 path1)
      ;; s0 ... e0=e1 ... s1
      (delete-path! path0)
      (delete-path! path1)
      {s=path0.s e=path1.s path=(append path0.path (rest (reverse path1.path)))}
      )

    ;; we need return only one version of the map.
    (define (get) fwdmap)

    {add=add-line get=get}
    ))
