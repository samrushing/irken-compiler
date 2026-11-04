;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "doom/doom.scm")

;; ACME http-01 (rfc8555 8.3): answer
;;  /.well-known/acme-challenge/<token> with the file <token> in
;;  <acme-dir> (where an ACME client such as demo/acme writes them).
(define acme-challenge-prefix "/.well-known/acme-challenge/")

(define (acme-token-ok? token)
  (and (> (string-length token) 0)
       (every? (lambda (ch) (or (digit? ch) (letter? ch) (char=? ch #\-) (char=? ch #\_)))
               (string->list token))))

(define (read-small-file path)
  (with-file f (file/open-read path)
    (let loop ((parts (list:nil)))
      (let ((s (file/read-buffer f)))
        (if (= 0 (string-length s))
            (string-concat (reverse parts))
            (loop (list:cons s parts)))))))

;; <acme-dir> is (maybe dir).
(define (redirect-channel addr sockfun acme-dir)
  (let ((sock (sockfun 8192 8192))
        (eoh-state 0)
        (stat (*stats*.add addr)))
    (debugf "redirect-channel starting...\n")

    (define (feed-part part)
      (for-string ch part
        (match eoh-state ch with
          0 #\return  -> (set! eoh-state 1)
          1 #\newline -> (set! eoh-state 2)
          2 #\return  -> (set! eoh-state 3)
          3 #\newline -> (set! eoh-state 4)
          4 _         -> #u
          _ _         -> (set! eoh-state 0)
          )))

    (define (get-header name lines)
      (let loop ((lines lines))
        (match lines with
          (hd . tl)
          -> (match (string-split hd #\space) with
               (key . rest)
               -> (if (string=? (downcase key) name)
                      (maybe:yes (string-trim-both (substring hd 6 (string-length hd))))
                      (loop tl))
               _
               -> (loop tl))
          _
          -> (maybe:no)
          )))

    (define (get-host lines)
      (let ((mhostport (get-header "host:" lines)))
        (match mhostport with
          (maybe:yes hostport)
          -> (match (string-split hostport #\:) with
               (host port) -> (maybe:yes host)
               (host)      -> (maybe:yes host)
               _           -> (maybe:no))
          (maybe:no)
          -> (maybe:no)
          )))

    ;; the request target is usually origin-form (/path), but may be
    ;;  absolute-form (http://host/path), which scanners probing for open
    ;;  proxies send.  RFC 9112 3.2.2: then the host comes from the target,
    ;;  not the Host header.  -> (maybe (:tuple host-override path))
    (define (split-target target)
      (define (from-authority rest)
        (let ((slash (string-find "/" rest)))
          (if (= slash -1)
              (:tuple (maybe:yes rest) "/")
              (:tuple (maybe:yes (substring rest 0 slash))
                      (substring rest slash (string-length rest))))))
      (cond ((starts-with target "/") (maybe:yes (:tuple (maybe:no) target)))
            ((starts-with target "http://")
             (maybe:yes (from-authority (substring target 7 (string-length target)))))
            ((starts-with target "https://")
             (maybe:yes (from-authority (substring target 8 (string-length target)))))
            (else (maybe:no)))) ;; e.g. "*", or garbage

    ;; drop any :port from an authority.
    (define (strip-port hostport)
      (match (string-split hostport #\:) with
        (host . _) -> host
        _          -> hostport))

    (define (process-request header)
      (let ((lines (string-split-string header "\r\n"))
            (request (string-split (first lines) #\space))
            (mhost (get-host lines)))
        (define (redirect method target)
          (match (split-target target) with
            (maybe:yes (:tuple (maybe:yes hostport) path))
            -> (send-redirect method (strip-port hostport) path)
            (maybe:yes (:tuple (maybe:no) path))
            -> (match mhost with
                 (maybe:yes host) -> (send-redirect method host path)
                 (maybe:no) -> (note-event stat (conn-event:redirect (format "no host: " (string header)))))
            (maybe:no)
            -> (note-event stat (conn-event:redirect (format "bad target: " (string header))))))
        (match request with
          (method target "HTTP/1.1")
          -> (match acme-dir (split-target target) with
               (maybe:yes dir) (maybe:yes (:tuple _ path))
               -> (if (starts-with path acme-challenge-prefix)
                      (send-challenge dir path)
                      (redirect method target))
               _ _ -> (redirect method target))
          _
          -> (note-event stat (conn-event:redirect (format "bad header: " (string header))))
          )))

    ;; HTTP/1.1 response with a small body.
    (define (send-reply status body)
      (sock.send (format "HTTP/1.1 " status "\r\n"
                         "Content-Type: text/plain\r\n"
                         "Content-Length: " (int (string-length body)) "\r\n"
                         "Connection: close\r\n"
                         "\r\n"
                         body)))

    (define (send-challenge dir path)
      (let ((token (substring path (string-length acme-challenge-prefix) (string-length path)))
            (file (format dir "/" token)))
        (note-event stat (conn-event:redirect (format "acme-challenge " token)))
        (if (and (acme-token-ok? token) (isfile? file))
            (send-reply "200 OK" (read-small-file file))
            (send-reply "404 Not Found" ""))))

    (define (send-redirect method host path)
      (let ((reply (format "HTTP/1.1 301 Moved Permanently\r\n"
                           "Content-Length: 0\r\n"
                           "Location: https://" host path "\r\n"
                           "\r\n")))
        (debugf "sending reply " (string reply) "\n")
        (note-event stat (conn-event:redirect (format method " " path)))
        (sock.send reply)))

    (define (read-header)
      (let loop ((parts (list:nil))
                 (data (sock.recv)))
        ;;(debugf "read-header: data = " (string data) "\n")
        (if (= (string-length data) 0)
            (maybe:no)
            (begin
              (feed-part data)
              (if (= eoh-state 4)
                  (maybe:yes (string-concat (reverse (list:cons data parts))))
                  (loop (list:cons data parts) (sock.recv)))
              ))))

    (try
     (begin
       (match (read-header) with
         (maybe:yes header)
         -> (process-request header)
         (maybe:no)
         -> #u
         )
       (sock.close))
     except (:OSError _)
     -> (sock.close)
     )))

;; <acme-dir> is (maybe dir): see redirect-channel.
(define (serve-redirect ip port acme-dir)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    ;; a small backlog fails under a burst of connections: macOS (and
    ;;  FreeBSD) reset connections once the accept queue is full.
    (sock.listen SOMAXCONN)
    (debugf "starting redirect server... on " (address/unparse addr) "\n")
    (debugf "  server fd = " (int (sock.get-fd)) "\n")
    (let loop ()
      (debugf "accept wait...\n")
      (try
       (let (((sockfun addr) (sock.accept))
             (addr-string (address/unparse addr)))
         (debugf "redirect client: addr " addr-string "\n")
         (poller/fork (lambda () (redirect-channel addr-string sockfun acme-dir)))
         (loop))
       except (:OSError errno)
       -> (cond ((= errno ECONNABORTED) (loop))
                ;; out of file descriptors: back off for a moment, rather
                ;;  than leave the accept loop (which stops the server).
                ((or (= errno EMFILE) (= errno ENFILE))
                 (debugf "accept: out of file descriptors (" (int errno) ")\n")
                 (timeq/sleep 100)
                 (loop))
                (else
                 (debugf "OSError on accepting socket: " (int errno) "\n")))
       )
      )
    (debugf "after accept loop.\n")
    ))
    
;; (define (main)
;;   (serve-redirect "0.0.0.0" 80)
;;   (poller/wait-and-schedule)
;;   )

;; (main)
