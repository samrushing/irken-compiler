;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/deque.scm")
(require "doom/ssh/test/csi.scm")

(require "demo/maze/gen.scm")
(require "demo/maze/utf8.scm")

;; can we slap together a quick line editor using ANSI/vt100 escape sequences?
;; what's the minimum necessary?
;; let's start with just backspace.
;; then move on to cursor positioning.

(define (line-edit chan)

  (define (chargen)
    (makegen emit
      (let loop ((buf (chan.read)))
        (when (> (string-length buf) 0)
          (for-string ch buf
            (emit ch))
          (loop (chan.read))))))

  (let ((gen (chargen))
        (history (deque/empty))
        (cx 0)
        (cy 0))

    (define (history-rotate up?)
      (let ((hist0 ((if up? deque/rotate-forward deque/rotate-backward) history)))
        (set! history hist0)
        (deque/peek-front history)))

    (define (getch)
      (match (gen) with
        (maybe:yes ch) -> ch
        (maybe:no)     -> #\eof
        ))

    (define (do-hist)
      (let ((n 0))
        (for-list line (deque->list history)
          (chan.write (format (ansi blue (int n)) " " (string line) "\r\n"))
          (inc! n))))

    (define (do-env)
      (let ((env (chan.getenv))
            (pairs (list:nil)))
        (for-map k v env
          (push! pairs (format "  " k "=" v)))
        (chan.write (format "env {\r\n" (join "\r\n" (reverse pairs)) "\r\n}\r\n"))))

    (define (do-maze)
      (let (((w h) (chan.get-size))
            (w0 (- (/ w 2) 1))
            (h0 (- h 3))
            (G (make-maze w0 h0 (get-seed) 0)))
        (printf "rendering " (int w0) " x " (int h0) " maze...\n")
        (for s (graph-utf8-gen G w0 h0)
          (chan.write s))))

    (define (maybe-command line)
      (match line with
        "dsr" -> (chan.write (csi->string (csi:dsr)))
        "hist" -> (do-hist)
        "env" -> (do-env)
        "maze" -> (do-maze)
        _ -> #u)
      (deque/push-front! history line)
      )

    (define (read-line)

      (let ((len 1024)
            (line (make-string len)) ;; may want to clear it.
            (pos 0))

        (define (handle-escape)
          (match (gen) with
            (maybe:yes #\[)
            -> (match (csi-get getch) with
                 (result:ok csi)
                 -> (handle-csi csi)
                 (result:err (:tuple p i k))
                 -> (printf (bold "[bad CSI: " (string p) " " (string i) " " (char (int->char k)) "]\n"))
                 )
            (maybe:yes ch)
            -> (printf (bold "[unknown escape sequence " (char ch) "]\n"))
            (maybe:no)
            -> (printf (bold "[end of file during CSI]\n"))
            ))

        (define (handle-device-status x y)
          (set! cx x)
          (set! cy y)
          (printf (bold "[DS " (int x) " " (int y) "]\n"))
          )

        (define (handle-csi csi)
          (match csi with
            (csi:cuu 0) -> (next-or-prev-line #t)
            (csi:cud 0) -> (next-or-prev-line #f)
            (csi:ds x y) -> (handle-device-status x y)
            _ -> (printf (bold (repr (csi->sexp csi))) "\n")
            ))

        (define (printable? ch)
          (let ((code (char->int ch)))
            (and (>= code #x20) (<= code #x7e))))

        (define (set-char! ch)
          (chan.write (char->string ch))
          (string-set! line pos ch)
          (inc! pos)
          (when (= pos len)
              (raise (:LineEdit/Overflow))))

        (define (set-line! val)
          (for-range i (string-length val)
            (string-set! line i (string-ref val i))))

        (define backspace-and-clear
          (string-append
           (csi->string (csi:cub 1))  ;; back one
           (csi->string (csi:el 0)))) ;; erase line to end

        (define (replace-line val)
          (chan.write (string-append "\r" (csi->string (csi:el 2)) val))
          (set-line! val)
          (set! pos (string-length val)))

        (define (next-or-prev-line up?)
          (match (history-rotate up?) with
            (maybe:yes line) -> (replace-line line)
            (maybe:no) -> (replace-line "")
            ))

        (define (clear-display)
          (chan.write
           (format
            (csi->string (csi:ed 2))    ;; clear display
            (csi->string (csi:cup 1 1)) ;; cursor pos at 1,1
            ))
          (replace-line "")
          )

        (let loop ()
          (match (gen) with
            (maybe:no)
            -> (raise (:LineEdit/EOF))
            (maybe:yes ch)
            -> (match ch with
                 #\return -> (substring line 0 pos)
                 #\u04 -> "exit"                                        ;; ctrl-d
                 #\u0c -> (begin (clear-display) (loop))                ;; ctrl-l
                 #\u01 -> (begin (set! pos 0) (chan.write "\r") (loop)) ;; ctrl-a
                 #\u10 -> (begin (next-or-prev-line #t) (loop))         ;; ctrl-p
                 #\u0e -> (begin (next-or-prev-line #f) (loop))         ;; ctrl-n
                 #\u7f -> (cond ((= pos 0) (chan.write "\x07") (loop))  ;; backspace
                                (else (chan.write backspace-and-clear)
                                      (dec! pos)
                                      (loop)))
                 #\u1b -> (begin (handle-escape) (loop))
                 ch -> (if (printable? ch)
                           (begin (set-char! ch) (loop))
                           (begin (printf "[" (zpad 2 (hex (char->int ch))) "]\n") (loop)))
                 )
            ))))

    (let loop ()
      (let ((line (read-line))
            (llen (string-length line)))
        (chan.write (format "\r\n>>> " (string line) "\r\n"))
        (match line with
          "exit" -> (chan.write (format (ansi (bold blue) "bye.") "\r\n"))
          "quit" -> (chan.write (format (ansi (bold blue) "bye.") "\r\n"))
          other  -> (begin (maybe-command line) (loop)))
        ))
    (chan.close)
    (printf "exiting repl loop...\n")
    ))

