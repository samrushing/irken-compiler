;; -*- Mode: Irken -*-

;; a buffered reader for line-oriented protocols (HTTP/1.1, SMTP,
;;  IMAP...), over anything with a recv method: a doom socket, or a
;;  connection from tls13/client.
;;
;; the end of the stream may show up either way: a plain socket's recv
;;  returns "", a TLS connection's raises Doom/EOF.
;;
;; reading past the end raises (:Reader/Closed what).  a line longer
;;  than reader-max-line raises (:Reader/LineTooLong n).

(define reader-max-line 65536)

(define (reader/make conn)
  (let ((buf "")
        (pos 0)
        (eof? #f))

    ;; get more data; #f at the end of the stream.
    (define (fill)
      (if eof?
          #f
          (let ((s (try
                    (conn.recv)
                    except
                    (:Doom/EOF _) -> "")))
            (if (= 0 (string-length s))
                (begin (set! eof? #t) #f)
                (begin
                  (set! buf (string-append (substring buf pos (string-length buf)) s))
                  (set! pos 0)
                  #t)))))

    ;; bytes received but not yet read.  (after STARTTLS this must be
    ;;  zero: anything already here was sent in the clear.)
    (define (ready) (- (string-length buf) pos))

    ;; a line, without its CRLF (or bare LF).
    (define (read-line)
      (let ((i (string-find-from "\n" buf pos)))
        (cond ((>= i 0)
               (let ((end (if (and (> i pos) (eq? (string-ref buf (- i 1)) #\return)) (- i 1) i))
                     (line (substring buf pos end)))
                 (set! pos (+ i 1))
                 line))
              ((> (ready) reader-max-line)
               (raise (:Reader/LineTooLong (ready))))
              ((fill) (read-line))
              (else (raise (:Reader/Closed "connection closed mid-line"))))))

    ;; exactly <n> bytes.
    (define (read-n n)
      (let ((parts (list:nil))
            (left n))
        (while (> left 0)
          (when (and (= 0 (ready)) (not (fill)))
            (raise (:Reader/Closed "connection closed mid-data")))
          (let ((k (min left (ready))))
            (push! parts (substring buf pos (+ pos k)))
            (inc! pos k)
            (dec! left k)))
        (string-concat (reverse parts))))

    ;; everything until the peer closes.
    (define (read-all)
      (let ((parts (list (substring buf pos (string-length buf)))))
        (set! buf "")
        (set! pos 0)
        (while (fill)
          (push! parts buf)
          (set! buf "")
          (set! pos 0))
        (string-concat (reverse parts))))

    {read-line=read-line read-n=read-n read-all=read-all ready=ready}
    ))

;; a uniform {send recv close} view of a doom socket or a TLS
;;  connection, so that a protocol can swap one for the other
;;  (STARTTLS) without changing the type of its connection.
(define (doom/stream conn)
  {send  = (lambda (s) (conn.send s) #u)
   recv  = (lambda () (conn.recv))
   close = (lambda () (conn.close) #u)
   })
