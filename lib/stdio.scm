;; -*- Mode: Irken -*-

(require-ffi 'stdio)

(define (stdio/open-read path)
  (let ((r (stdio/fopen (cstring path) (cstring "rb"))))
    (if (not (cref-null? r))
        r
        (raise (:StdioOpenRead "failed to open file for read" path)))))

(define (stdio/open-write path)
  (let ((r (stdio/fopen (cstring path) (cstring "wb"))))
    (if (not (cref-null? r))
        r
        (raise (:StdioOpenWrite "failed to open file for write" path)))))

(define (stdio/close FILE*)
  (stdio/fclose FILE*))

(define (stdio/read FILE* size)
  (let ((buffer (%c-aref char (halloc char size) 0))
        (nbytes (stdio/fread (%c-cast void buffer) 1 size FILE*)))
    (%cref->string #f buffer nbytes)))

(define (stdio/read-char FILE*)
  (let ((r (stdio/fgetc FILE*)))
    (if (< r 0)
        #\eof
        (if (> r 255)
            (begin
              (printf "whoa, this didn't work right" (hex r) "\n")
              (%exit #f -1))
            (int->char r)))))

(define (stdio/read-line FILE*)
  (let loop ((line '()))
    (match (stdio/read-char FILE*) with
      #\eof     -> (maybe:no)
      #\newline -> (maybe:yes (list->string (reverse line)))
      ch        -> (loop (list:cons ch line))
      )))

(define (stdio/read-lines FILE*)
  (let loop ((lines '()))
    (match (stdio/read-line FILE*) with
      (maybe:yes line) -> (loop (list:cons line lines))
      (maybe:no)       -> (reverse lines)
      )))

(define (stdio/flush FILE*)
  (stdio/fflush FILE*))

(define (stdio/write FILE* s)
  (let ((slen (string-length s))
        (s* (cstring s)))
    (stdio/fwrite (%c-cast void s*) 1 slen FILE*)))

;; generates blocks, not characters.
(define (make-stdio-generator FILE*)
  (makegen emit
    (let loop ((buf (stdio/read FILE* 4096)))
      (when (> 0 (string-length buf))
        (emit buf)
        (loop (stdio/read FILE* 4096))))))

(define (stdio-char-generator FILE*)
  (makegen emit
    (let loop ((ch (stdio/read-char FILE*)))
      (when (not (eq? ch #\eof))
        (emit ch)
        (loop (stdio/read-char FILE*))))))

(define (stdio/line-generator FILE*)
  (makegen emit
    (let loop ((mline (stdio/read-line FILE*)))
      (when-maybe line mline
        (emit line)
        (loop (stdio/read-line FILE*))))))
