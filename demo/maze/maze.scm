;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/map.scm")
(require "lib/mtwist.scm")
(require "lib/getopt.scm")
(require "lib/codecs/base85.scm")
(require "lib/codecs/base64.scm")

(require "demo/maze/gen.scm")
(require "demo/maze/ascii.scm")
(require "demo/maze/js.scm")
(require "demo/maze/solve.scm")

;; use the standard svg renderer.
;;(require "demo/maze/svg.scm")

;; use the 'marker-based' svg renderer.
(require "demo/maze/svg2.scm")

(define program-options
  (makeopt
   (arg 's (int 10))
   (flag 'ascii)
   (flag 'js)
   (flag 'hex)
   (flag 'b85)
   (flag 'b64)
   (flag 'solve)
   (arg 'seed (int 31415926535))
   (arg 'bias (int 0))
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
          "  [-js] javascript output\n"
          "  [-b85] base85 output\n"
          "  [-b64] base64 output\n"
          "  [-hex] HEX output\n"
          "  [-bias] -100..100 % bias horizontal..vertical\n"
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
        (js #f)
        (b85 #f)
        (b64 #f)
        (solve? #f)
        (bias 0)
        (seed (get-seed))
        )
    (when-maybe v (get-bool-opt opts 'hex)
      (set! hex #t))
    (when-maybe v (get-bool-opt opts 'ascii)
      (set! ascii #t))
    (when-maybe v (get-bool-opt opts 'js)
      (set! js #t))
    (when-maybe v (get-bool-opt opts 'b85)
      (set! b85 #t))
    (when-maybe v (get-bool-opt opts 'b64)
      (set! b64 #t))
    (when-maybe v (get-bool-opt opts 'solve)
      (set! solve? #t))
    (when-maybe v (get-int-opt opts 'h)
      (set! h v))
    (when-maybe v (get-int-opt opts 'w)
      (set! w v))
    (when-maybe v (get-int-opt opts 's)
      (set! s v))
    (when-maybe v (get-int-opt opts 'seed)
      (set! seed v))
    (when-maybe v (get-int-opt opts 'bias)
      (printf "v " (int v) "\n")
      (when (and (>= v -100) (<= v 100))
        (set! bias (/ (* v 64) 100))))

    (let ((maze (make-maze w h seed bias))
          (solution (if solve? (BFS maze w h) (list:nil))))
      (cond (ascii (graph->ascii maze w h solution))
            (js (graph->js maze w h s solution))
            (b85 (graph->base85 maze w h))
            (b64 (graph->base64 maze w h))
            (hex (graph->hex maze w h))
            (else (graph->svg maze w h s solution)))
      )
    ))

(main)
