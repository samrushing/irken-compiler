;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "doom/ssh/test/csi.scm")

;; can we slap together a quick line editor using ANSI/vt100 escape sequences?
;; what's the minimum necessary?
;; let's start with just backspace.
;; then move on to cursor positioning.

(define (make-line-editor chan)

  (define (chargen)
    (makegen emit
      (let loop ((buf (chan.read)))
        (when (> (string-length buf) 0)
          (for-string ch buf
            (emit ch))
          (loop (chan.read))))))

  (let ((gen (chargen))
        (history (list:nil)) ;; most recent first
        (hpos -1)            ;; -1: not browsing history
        (len 1024)
        (line (make-string len))
        (pos 0)
        (prompt "")
        (reading? #f)
        (cx 0)
        (cy 0))

    (define erase-line (csi->string (csi:el 2)))

    ;; print a message on its own line *above* the line being edited,
    ;;  then redraw the prompt and whatever the user has typed so far.
    ;;  safe to call from another thread (e.g. a chat broadcast).
    (define (interject msg)
      (chan.write
       (if reading?
           (string-append "\r" erase-line msg "\r\n" prompt (substring line 0 pos))
           (string-append msg "\r\n"))))

    ;; move through history: up? goes to older entries.
    (define (history-rotate up?)
      (let ((n (length history)))
        (set! hpos (if up? (min (+ hpos 1) (- n 1)) (max (- hpos 1) -1)))
        (if (< hpos 0)
            (maybe:no)
            (maybe:yes (nth history hpos)))))

    (define (getch)
      (match (gen) with
        (maybe:yes ch) -> ch
        (maybe:no)     -> #\eof
        ))

    (define (read-line)

      (let ((_ (set! pos 0))
            (_ (set! reading? #t)))

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
          (chan.write (string-append "\r" erase-line prompt val))
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
                 #\return -> (let ((result (substring line 0 pos)))
                              (set! hpos -1)
                              (set! pos 0)
                              (set! reading? #f)
                              (when (> (string-length result) 0)
                                (push! history result))
                              (maybe:yes result))
                 #\u04 -> (begin (set! reading? #f) (maybe:no))       ;; ctrl-d
                 #\u0c -> (begin (clear-display) (loop))                ;; ctrl-l
                 #\u01 -> (begin (set! pos 0) (chan.write (string-append "\r" prompt)) (loop)) ;; ctrl-a
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

    ;; like read-line, but displays `p` first, and redraws it as needed.
    (define (prompt-read p)
      (set! prompt p)
      (chan.write p)
      (read-line))

    {read-line=read-line prompt-read=prompt-read interject=interject}
    ))

;; the original interface: just the read-line function.
(define (line-edit chan)
  (let ((ed (make-line-editor chan)))
    ed.read-line))
