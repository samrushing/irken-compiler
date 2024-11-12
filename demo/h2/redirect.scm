;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "doom/doom.scm")

(define (redirect-channel addr sockfun)
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

    (define (process-request header)
      (let ((lines (string-split-string header "\r\n"))
            (request (string-split (first lines) #\space))
            (mhost (get-host lines)))
        (match request mhost with
          (method path "HTTP/1.1") (maybe:yes host)
          -> (let ((reply (format "HTTP/1.1 301 Moved Permanently\r\n"
                                "Content-Length: 0\r\n"
                                "Location: https://" host path "\r\n"
                                "\r\n")))
               (debugf "sending reply " (string reply) "\n")
               (note-event stat (conn-event:redirect (format method " " path)))
               (sock.send reply))
          _ _
          -> (note-event stat (conn-event:redirect (format "bad header: " (string header))))
          )))

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

(define (serve-redirect ip port)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (debugf "starting redirect server... on " (address/unparse addr) "\n")
    (debugf "  server fd = " (int (sock.get-fd)) "\n")
    (let loop ()
      (debugf "accept wait...\n")
      (try
       (let (((sockfun addr) (sock.accept))
             (addr-string (address/unparse addr)))
         (debugf "redirect client: addr " addr-string "\n")
         (poller/fork (lambda () (redirect-channel addr-string sockfun)))
         (loop))
       except (:OSError errno)
       -> (if (= errno ECONNABORTED)
              (loop)
              (debugf "OSError on accepting socket: " (int errno) "\n"))
       )
      )
    (debugf "after accept loop.\n")
    ))
    
;; (define (main)
;;   (serve-redirect "0.0.0.0" 80)
;;   (poller/wait-and-schedule)
;;   )

;; (main)
