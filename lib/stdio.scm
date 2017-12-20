;; -*- Mode: Irken -*-

(require-ffi 'stdio)

(define (stdio/open-read path)
  (stdio/fopen (cstring path) (cstring "rb")))

(define (stdio/open-write path create? mode)
  (stdio/fopen (cstring path) (cstring "wb")))

(define (stdio/close FILE*)
  (stdio/fclose FILE*))

(define (stdio/read FILE* size)
  (let ((buffer (%c-aref char (halloc char size) 0))
        (nbytes (stdio/fread buffer 1 size FILE*)))
    (%cref->string #f buffer nbytes)))

(define (stdio/read-char FILE*)
  (let ((r (stdio/fgetc FILE*)))
    (if (< r 0)
        #\eof
        (int->char r))))

(define (stdio/read-line FILE*)
  (let loop ((ch (stdio/read-char FILE*))
         (r '()))
    (if (eq? ch #\newline)
        (list->string (reverse r))
        (loop (stdio/read-char FILE*) (list:cons ch r)))))

(define (stdio/flush FILE*)
  (stdio/fflush FILE*))

(define (stdio/write FILE* s)
  (let ((slen (string-length s))
        (s* (cstring s)))
    (stdio/fwrite s* 1 slen FILE*)))

;; generates blocks, not characters.
(define (make-stdio-generator FILE*)
  (make-generator
   (lambda (consumer)
     (let loop ((buf (stdio/read FILE* 4096)))
       (if (= 0 (string-length buf))
           (consumer (maybe:no))
           (consumer (maybe:yes buf)))
       (loop (stdio/read FILE* 4096))))))

(define (stdio-char-generator FILE*)
  (make-generator
   (lambda (consumer)
     (let ((done #f))
       (while (not done)
         (let ((ch (stdio/read-char FILE*)))
           (if (eq? ch #\eof)
               (set! done #t)
               (consumer (maybe:yes ch)))))
       (forever (consumer (maybe:no)))
       ))))