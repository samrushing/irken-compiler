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
        ctx.lineWidth = 2;
        ctx.lineCap = 'round';
        ctx.strokeStyle = 'black';
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

(define (graph->js G m n scale solution)

  (define (print-data)
    (let ((lines '())
          (line '()))
      (for-range y n
        (for-range x m
          (let ((walls G[(+ x (* y m))]))
            (match (bit-set? walls 3) (bit-set? walls 1) with
              #t #t -> (PUSH line #\T)
              #t #f -> (PUSH line #\U)
              #f #t -> (PUSH line #\L)
              #f #f -> (PUSH line #\space)
              )))
        (PUSH lines (list->string (reverse line)))
        (set! line '()))
      (printf "maze_data = [\n")
      (for-list line (reverse lines)
        (printf "  \"" line "\",\n"))
      (printf "];\n")
      ))

  (printf "<canvas id=\"maze\" "
          "width=\"" (int (+ scale (* scale m))) "\" "
          "height=\"" (int (+ scale (* scale n))) "\">"
          (int m) "x" (int n) " seed " (int *random-seed*)
          "</canvas>\n")
  (printf "<script>\n"
          js-script)
  (print-data)
  (printf "draw_maze (maze_data, " (int m) ", " (int n) ", " (int scale) ");\n"
          "</script>\n"
          "<br>" (int m) "x" (int n) " seed " (int *random-seed*) "\n")
  )

