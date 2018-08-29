;; -*- Mode: Irken -*-

(define js-script
"
function draw_line (ctx, x0, y0, x1, y1) {
    ctx.beginPath();
    ctx.moveTo (x0, y0);
    ctx.lineTo (x1, y1);
    ctx.stroke();
}
function draw_maze (data, w, h, s) {
    var canvas = document.getElementById('maze');
    if (canvas.getContext) {
        var ctx = canvas.getContext('2d');
        ctx.lineWidth = 1;
        ctx.lineCap = 'round';
        ctx.strokeStyle = '#000000';
        // magic to make the lines not blurry.
        ctx.translate (0.5, 0.5);
        for (y=0; y < h; y++) {
            for (x=0; x < w; x++) {
                switch (data[y][x]) {
                case 'U':
                    draw_line (ctx, x*s, y*s, x*s+s, y*s);
                    break;
                case 'L':
                    draw_line (ctx, x*s, y*s, x*s, y*s+s);
                    break;
                case 'T':
                    draw_line (ctx, x*s, y*s, x*s+s, y*s);
                    draw_line (ctx, x*s, y*s, x*s, y*s+s);
                    break;
                default:
                    break;
                }
            }
        }
        ctx.beginPath();
        ctx.moveTo (0, 0);
        ctx.lineTo (w*s, 0);
        ctx.lineTo (w*s, h*s);
        ctx.lineTo (0, h*s);
        ctx.lineTo (0, 0);
        ctx.stroke();
    }
}
")

(define (graph-js-gen G m n scale solution)

  (defmacro emitf
    (emitf x ...)
    -> (emit (format x ...)))

  (makegen emit

    (define (print-data)
      (let ((lines '())
            (line '()))
        (for-range y n
          (for-range x m
            (let ((walls G[(+ x (* y m))]))
              (match (bit-set? walls 3) (bit-set? walls 1) with
                #t #t -> (push! line #\T)
                #t #f -> (push! line #\U)
                #f #t -> (push! line #\L)
                #f #f -> (push! line #\space)
                )))
          (push! lines (list->string (reverse line)))
          (set! line '()))
        (emitf "maze_data = [\n")
        (for-list line (reverse lines)
          (emitf "  \"" line "\",\n"))
        (emitf "];\n")
        ))

    (emitf "<canvas id=\"maze\" "
           "width=\"" (int (+ scale (* scale m))) "\" "
           "height=\"" (int (+ scale (* scale n))) "\">"
           (int m) "x" (int n) " seed " (int *random-seed*)
           "</canvas>\n")
    (emitf "<script>\n"
           js-script)
    (print-data)
    (emitf "draw_maze (maze_data, " (int m) ", " (int n) ", " (int scale) ");\n"
           "</script>\n"
           "<br/>" (int m) "x" (int n) " seed " (int *random-seed*) "\n")
    ))

(define (graph->js G m n S solution)
  (for s (graph-js-gen G m n S solution)
    (print-string s)))
