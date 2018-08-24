;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/mtwist.scm")
(include "lib/getopt.scm")

(include "demo/maze/gen.scm")
(include "demo/maze/ascii.scm")
(include "demo/maze/svg.scm")
(include "demo/maze/solve.scm")

(define *random-seed* 0)

(define (get-seed)
  (when (= 0 *random-seed*)
    (set! *random-seed* (read-cycle-counter)))
  *random-seed*)

(define (make-maze m n scale)
  (let ((G (make-grid m n))
        (G0 (DFS G m n)))
    G0))

(define program-options
  (makeopt
   (arg 's (int 10))
   (flag 'ascii)
   (flag 'hex)
   (flag 'solve)
   (arg 'seed (int 31415926535))
   (pos 'w (int 115))
   (pos 'h (int 85))
   ))

(define (get-options)
  (if (< sys.argc 3)
      (usage)
      (try
       (process-options program-options (rest (vector->list sys.argv)))
       except
       (:Getopt/MissingArg _ _) -> (usage)
       (:Getopt/UnknownArg _ _) -> (usage)
       )))

(define (usage)
  (printf "SVG/ASCII Maze Generator.\n\n")
  (printf "Usage: " sys.argv[0] " <width> <height>\n"
          "  [-s=10]  scale\n"
          "  [-seed=31415926535] random seed\n"
          "  [-ascii] ASCII output\n"
          "  [-hex] HEX output\n"
          "  [-solve] add the solution to SVG output\n"
          "example: $ " sys.argv[0] " 50 50\n")
  (%exit #f -1))

;; 14MB svg:
;;   $ maze 1000 700 -s 10
;;
;; fits 8.5"x11" in landscape mode:
;;   $ maze 115 85 -s 10

(define (main)
  (let ((opts (get-options))
        (w 115)
        (h 85)
        (s 10)
        (hex #f)
        (ascii #f)
        (solve? #f)
        )
    (when-maybe v (get-bool-opt opts 'hex)
      (set! hex #t))
    (when-maybe v (get-bool-opt opts 'ascii)
      (set! ascii #t))
    (when-maybe v (get-bool-opt opts 'solve)
      (set! solve? #t))
    (when-maybe v (get-int-opt opts 'h)
      (set! h v))
    (when-maybe v (get-int-opt opts 'w)
      (set! w v))
    (when-maybe v (get-int-opt opts 's)
      (set! s v))
    (when-maybe v (get-int-opt opts 'seed)
      (set! *random-seed* v))

    (let ((maze (make-maze w h s))
          (solution (if solve? (BFS maze w h) (list:nil))))
      (cond (ascii (graph->ascii maze w h solution))
            (hex (graph->hex maze w h))
            (else (graph->svg maze w h s solution))))
    ))

(main)
