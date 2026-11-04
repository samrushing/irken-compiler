;; -*- Mode: Irken -*-

;; a minimal HTTP/1.1 client (rfc9112): one request per connection
;;  ("Connection: close"), over anything with send & recv (e.g. a
;;  connection from tls13/client).
;;
;; the response body may be delimited by Content-Length, by chunked
;;  transfer-coding, or by the server closing the connection.

(require "lib/codecs/hex.scm")

;; a buffered reader over <conn>.recv
(define (h1/reader conn)
  (let ((buf "")
        (pos 0)
        (eof? #f))

    ;; get more data; #f at end of stream.
    (define (fill)
      (if eof?
          #f
          (try
           (let ((s (conn.recv)))
             (set! buf (string-append (substring buf pos (string-length buf)) s))
             (set! pos 0)
             #t)
           except
           (:Doom/EOF _)
           -> (begin (set! eof? #t) #f))))

    (define (ready) (- (string-length buf) pos))

    ;; a line, without its CRLF (or bare LF).
    (define (read-line)
      (let ((i (string-find-from "\n" buf pos)))
        (if (= i -1)
            (if (fill)
                (read-line)
                (raise (:H1/BadResponse "connection closed mid-line")))
            (let ((end (if (and (> i pos) (eq? (string-ref buf (- i 1)) #\return)) (- i 1) i))
                  (line (substring buf pos end)))
              (set! pos (+ i 1))
              line))))

    ;; exactly <n> bytes.
    (define (read-n n)
      (let ((parts (list:nil))
            (left n))
        (while (> left 0)
          (when (and (= 0 (ready)) (not (fill)))
            (raise (:H1/BadResponse "connection closed mid-body")))
          (let ((k (min left (ready))))
            (push! parts (substring buf pos (+ pos k)))
            (inc! pos k)
            (dec! left k)))
        (string-concat (reverse parts))))

    ;; everything until the server closes.
    (define (read-all)
      (let ((parts (list (substring buf pos (string-length buf)))))
        (set! buf "")
        (set! pos 0)
        (while (fill)
          (push! parts buf)
          (set! buf "")
          (set! pos 0))
        (string-concat (reverse parts))))

    {read-line=read-line read-n=read-n read-all=read-all}
    ))

(define (h1/parse-hex s)
  (let ((n 0))
    (for-string ch s
      (set! n (+ (* n 16) (hexdigit ch))))
    n))

;; header names are case-insensitive: we store them lowercased.
(define (h1/header name headers)
  (let loop ((hs headers))
    (match hs with
      () -> (maybe:no)
      ((:tuple n v) . rest) -> (if (string=? n name) (maybe:yes v) (loop rest))
      )))

;; send a request and read the whole response.  <headers> is a list
;;  of (:tuple name value); <body> may be "".  returns {status headers
;;  body}, like h2/client.
(define (h1/request conn method authority path headers body)

  (let ((r (h1/reader conn)))

    (define (read-headers)
      (let loop ((acc (list:nil)))
        (let ((line (r.read-line)))
          (if (= 0 (string-length line))
              (reverse acc)
              (let ((colon (string-find ":" line)))
                (when (< colon 1)
                  (raise (:H1/BadResponse (format "bad header line: " (string line)))))
                (loop (list:cons
                       (:tuple (downcase (substring line 0 colon))
                               (string-trim-both (substring line (+ colon 1) (string-length line))))
                       acc)))))))

    ;; "HTTP/1.1 200 OK" -> "200"
    (define (read-status)
      (let ((line (r.read-line)))
        (match (string-split line #\space) with
          (version code . _)
          -> (if (and (starts-with version "HTTP/1.") (= 3 (string-length code)))
                 code
                 (raise (:H1/BadResponse (format "bad status line: " (string line)))))
          _ -> (raise (:H1/BadResponse (format "bad status line: " (string line)))))))

    ;; skip any 1xx (informational) responses.
    (define (read-head)
      (let ((status (read-status))
            (hs (read-headers)))
        (if (and (eq? (string-ref status 0) #\1) (not (string=? status "101")))
            (read-head)
            {status=status headers=hs})))

    (define (read-chunked)
      (let loop ((parts (list:nil)))
        (let ((line (r.read-line))
              ;; ignore chunk extensions
              (size-str (string-trim-both (first (string-split line #\;))))
              (size (try
                     (h1/parse-hex size-str)
                     except
                     (:Hex/BadDigit _ _)
                     -> (raise (:H1/BadResponse (format "bad chunk size: " (string line)))))))
          (if (= size 0)
              (begin
                (read-headers) ;; trailers, discarded
                (string-concat (reverse parts)))
              (let ((data (r.read-n size)))
                (when (not (= 0 (string-length (r.read-line))))
                  (raise (:H1/BadResponse "missing CRLF after chunk")))
                (loop (list:cons data parts)))))))

    (define (read-body status hs)
      (cond ((or (string=? method "HEAD")
                 (string=? status "204")
                 (string=? status "304"))
             "")
            ((match (h1/header "transfer-encoding" hs) with
               (maybe:yes te) -> (ends-with (downcase te) "chunked")
               (maybe:no)     -> #f)
             (read-chunked))
            (else
             (match (h1/header "content-length" hs) with
               (maybe:yes n) -> (r.read-n (string->int (string-trim-both n)))
               (maybe:no)    -> (r.read-all)))))

    (conn.send
     (string-concat
      (append
       (list method " " path " HTTP/1.1\r\n"
             "host: " authority "\r\n"
             "connection: close\r\n")
       (map (lambda (h) (match h with (:tuple name val) -> (format name ": " val "\r\n"))) headers)
       (if (or (> (string-length body) 0) (string=? method "POST") (string=? method "PUT"))
           (list "content-length: " (int->string (string-length body)) "\r\n")
           (list:nil))
       (list "\r\n" body))))

    (let ((head (read-head)))
      {status=head.status headers=head.headers body=(read-body head.status head.headers)})
    ))
