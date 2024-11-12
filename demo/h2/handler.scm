;; -*- Mode: Irken -*-

;; --------------------------------------------------------------------------------
;; various handlers/meta-handlers/etc, related utils.
;; --------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------
;;  request decoding
;; --------------------------------------------------------------------------------

;; XXX maybe use a multi-map for headers?
(define lookup-header
  key ()
  -> (maybe:no)
  key ((:tuple name val) . rest)
  -> (if (string=? name key)
         (maybe:yes val)
         (lookup-header key rest)))

(define (decode-params params)
  (let ((kvmap (tree/empty))
        (parts0 (string-split params #\&)))
    (for-list part0 parts0
      (let ((parts1 (string-split part0 #\=)))
        (match parts1 with
          (key val) -> (tree/insert! kvmap string-compare key val)
          _         -> #u))) ;; are params required to have values?  maybe should be key=""?
    kvmap))

(define (decode-hex s)
  ;; decode %XX in string s
  (let ((len (string-length s))
        (acc (list:nil))
        (high 0)
        (pos 0)
        (state 0)) ;; 0 = normal 1 = read-% 2 = first dig
    (let/cc exit
      (for-range i len
        (match state (string-ref s i) with
          0 #\% -> (begin (push! acc (substring s pos i)) (set! state 1))
          0  c  -> #u
          1  h  -> (match (alist/lookup hex-map h) with
                     (maybe:yes v) -> (begin (set! high (<< v 4)) (set! state 2))
                     (maybe:no)    -> (exit (maybe:no)))
          2  h  -> (match (alist/lookup hex-map h) with
                     (maybe:yes v) -> (begin (push! acc (char->string (int->char (logior high v))))
                                             (set! state 0)
                                             (set! pos (+ i 1)))
                     (maybe:no)    -> (exit (maybe:no)))
          _  _  -> (impossible)))

      (if (= state 0) ;; normal
          (maybe:yes (string-concat (reverse (list:cons (substring s pos len) acc))))
          (maybe:no))
      )))

;; path ? params # fragment
(define (decode-uri uri)
  (let/cc return
    (let ((fragment "")
          (pathquery "/"))
      (match (string-split uri #\#) with
        (p f) -> (begin (set! pathquery p) (set! fragment f))
        (p)   -> (set! pathquery p)
        ()    -> #u
        _     -> (return (maybe:no)))
      (let ((base "")
            (params ""))
        (match (string-split pathquery #\?) with
          (b p) -> (begin (set! base b) (set! params p))
          (b)   -> (set! base b)
          ()    -> #u
          _     -> (return (maybe:no)))
        (if-maybe v (decode-hex base)     (set! base v)     (return (maybe:no)))
        (if-maybe v (decode-hex params)   (set! params v)   (return (maybe:no)))
        (if-maybe v (decode-hex fragment) (set! fragment v) (return (maybe:no)))
        (maybe:yes
         (:tuple (string-split base #\/)
                 (decode-params params)
                 fragment))
        ))))

;; XXXX it'd be nice if this decoding was done once and only once.
;; but I feel like this code doesn't belong in h2.scm.  maybe the
;; handler protocol can be something like (handle request decoded)?

;; your all-in-one request unpacking/decoding interface.
;; request -> method parts params fragment
;; struct  -> string (list string) (map string string) string
(define (unpack-request request)
  (let ((mpath (lookup-header ":path" request.headers))
        (mmethod (lookup-header ":method" request.headers)))
    (match mmethod mpath with
      (maybe:yes method) (maybe:yes uri)
      -> (match (decode-uri uri) with
           (maybe:yes (:tuple parts params fragment))
           -> (maybe:yes (:tuple method parts params fragment))
           _
           -> (maybe:no)
           )
      _ _ 
      -> (maybe:no)
      )))

;; handler protocol:
;; (handler request) -> (maybe fun) ; (fun request)

;; --------------------------------------------------------------------------------

;; this can be used to get an exact match, using string=?
;;  or a prefix-match using string-starts-with, etc..

(define (header-matches? request = key value)
  (match (lookup-header key request.headers) with
    (maybe:yes val0)
    -> (= val0 value)
    (maybe:no)
    -> #f
    ))

;; --------------------------------------------------------------------------------

;; try several handlers, in order.

(define (multi-handler handlers)
  (lambda (request)
    (let loop ((handlers handlers))
      (match handlers with
        (hd . tl)
        -> (match (hd request) with
             (maybe:yes fun) -> (maybe:yes fun)
             (maybe:no)      -> (loop tl))
        ()
        -> (maybe:no)
       ))))

;; --------------------------------------------------------------------------------

(define sexp-css
  (string-join
   (list
    "pre { line-height: 125%; }"
    "body  { background: #f8f8f8; }"
    "body .z { color: #408080; font-style: italic }"
    "body .C { color: #008000; font-weight: bold }"
    "body .s { color: #BA2121; }"
    "body .i { color: #000090; font-weight: bold }"
    "body .c { color: #800080; }"
    "body .p { color: #909090; }"
    "body .b { color: #0000C0; font-weight:bold }")
   "\n"))

(%backend c
  ;; XXX hack, tracking down possible gc bug.
  (define (^gc-flip) ;; marked to not be inlined
    (%%cexp (-> int) "gc_flip(0)")))
(%backend arm
  (define (^gc-flip) ;; fake it til you make it
    0))

(define (status-handler addr options)

  (define (disposition events)
    (let ((r (list:nil)))
      (for-list event events
        (match event with
          (conn-event:tls-error (maybe:yes desc) s)  -> (push! r (format "tls-error: " (sym (tls-alert-desc->name desc)) " " s))
          (conn-event:tls-error _ s)      -> (push! r (format "tls-error: " (string s)))
          (conn-event:h2-error s)         -> (push! r (format "h2-error: " (string s)))
          (conn-event:os-error s)         -> (push! r (format "os-error: " (string s)))
          (conn-event:tls-good sexp)      -> (push! r (format "good tls"))
          _                               -> #u))
      (rope/join-string ", " (map rope:leaf r))))

  (define (maybe-get-tls-data events)
    (let loop ((events events))
      (match events with
        ((conn-event:tls-good exp) . tl) -> (maybe:yes exp)
        (hd . tl) -> (loop tl)
        () -> (maybe:no)
        )))

  (define (count-requests events)
    (let ((r 0))
      (for-list event events
        (match event with
          (conn-event:h2-request _) -> (inc! r)
          _ -> #u))
      r))

  (define (gen-host-table)
    (makegen emit
      (let ((hosts (*stats*.get-all)))
        (emit (format "<html><body><style type=\"text/css\">\n"
                      table-css
                      "</style><table><tr><th>addr</th><th>hostname</th><th>requests</th><th>disposition</th></tr>\n"))
        (for-list host hosts
          (let ((hostname (*dnscache* host.addr)))
            (emit
             (rope->string
              (html (tr (td ((a (href (format "/status/addr/" host.addr))) host.addr))
                        (td hostname)
                        ((td (align "right"))
                         ((a (href (format "/status/requests/" host.addr "/")))
                          (&f (int (count-requests host.events)))))
                        (td (&= (disposition host.events)))))
              ))))
        (emit (format "</table></body></html>")))))

  (define (handle-addrs request)
    (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
    (request.send-with-generator (gen-host-table)))

  (define (handle-host request addr)
    (match (*stats*.get addr) with
      (maybe:yes entry)
      -> (match (maybe-get-tls-data entry.events) with
           (maybe:yes exp)
           -> (begin
                (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
                (request.send-data
                 (rope->string
                  (html
                   "<!doctype html>"
                   (html
                    (head (style sexp-css))
                    (body (h2 addr)
                          (&pp exp)
                          ))))
                 #t))
           (maybe:no)
           -> (begin
                (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
                (request.send-data "<html><body><p>no TLS success event</p></body></html>" #t)))
      (maybe:no)
      -> (begin
           (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
           (request.send-data "<html><body><p>no such host</p></body></html>" #t))
      ))

  (define (format-headers headers)
    (define header->sexp
      (:tuple name val) -> (sexp (sym (string->symbol name)) (string val)))
    (define (header->html header)
      (html (&pp (header->sexp header))))
    ;;(html (&join " " (map header->html headers)))
    (rope/join (rope:leaf " ") (map header->html headers))
    )

  (define (format-one-request qnum headers)
    (define (get-one k)
      (if-maybe v (lookup-header k headers) v "none"))
    (define (get-pseudos)
      (list
       (get-one ":method")
       (get-one ":scheme")
       (get-one ":authority")
       (get-one ":path")))
    (format "<tr><td><a href=\"./" (int qnum) "\"</a>" (int qnum) "</td><td>" (join "</td><td>" (get-pseudos)) "</td></tr>\n")
    )

  (define (gen-requests-table addr events)
    (makegen emit
      (emit (format "<html><body><style type=\"text/css\">\n"
                    table-css
                    "</style><h1>requests for " addr "</h1>\n"
                    "<h2>" (*dnscache* addr) "</h2>\n"
                    "<table><tr><th>num</th><th>method</th><th>scheme</th><th>authority</th><th>path</th></tr>\n"))
      (let loop ((i 1)
                 (events events))
        (match events with
          (headers . tl)
          -> (begin (emit (format-one-request i headers)) (loop (+ i 1) tl))
          _
          -> #u
          ))
      (emit (format "</table></body></html>\n"))))

  (define (handle-requests request addr)
    (match (*stats*.get addr) with
      (maybe:yes entry)
      -> (let loop ((acc (list:nil))
                    (events entry.events))
           (match events with
             ((conn-event:h2-request headers) . tl)
             -> (loop (list:cons headers acc) tl)
             (_ . tl)
             -> (loop acc tl)
             ()
             -> (begin
                  (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
                  (request.send-with-generator (gen-requests-table addr acc)))
             ))
      (maybe:no)
      -> (begin
           (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
           (request.send-data "<html><body><p>no such host</body></html>" #t))
      ))

  (define (safe-string->int s)
    (try
     (string->int s)
     except
     (:String/BadInt _)
     -> 1
     ))

  (define (find-single-request addr needle)
    (match (*stats*.get addr) with
        (maybe:yes entry)
        -> (let loop ((index 1)
                      (events (reverse entry.events)))
             (match events with
               ((conn-event:h2-request headers) . tl)
               -> (if (= needle index)
                      (maybe:yes headers)
                      (loop (+ index 1) tl))
               (_ . tl)
               -> (loop index tl)
               ()
               -> (maybe:no)
               ))
        (maybe:no)
        -> (maybe:no)
      ))

  (define (handle-single-request request addr num)

    (define format-one-header
      (:tuple key val)
      -> (html (tr (td key) (td val)))
      )

    (match (find-single-request addr (safe-string->int num)) with
      (maybe:yes headers)
      -> (let ((reply (html "<!doctype html>"
                            (html
                             (head (style table-css))
                             (body (table (tr (th "key") (th "value"))
                                          (&join format-one-header "\n" headers)))))))
           (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
           (request.send-data (rope->string reply) #t))
      (maybe:no)
      -> (send-status request "404")
      ))

  (define (handle-requests-gc request)
    (let ((count (^gc-flip)))
      (request.send-headers "200" (list (:tuple "content-type" "text/plain")) #t)
      (request.send-data (format "gc collected " (int count) " words\n") #t)
      ))

  (define (handle-toplevel request)
    (let ((timestamp (format-timestamp *start-time*.sec))
          (reply 
           (html
            "<!doctype html>"
            (html
             (head (style sexp-css))
             (body (h2 (format "HTTP/2 Server Running on " addr))
                   (h3 (format "started on " timestamp))
                   (p ((a (href "addrs/")) "last 1000 connections"))
                   (p ((a (href "gc")) "manual gc"))
                   (p ((a (href "shutdown")) "shut the server down"))
                   (p (b "options:") (&pp (args->sexp options))))))))
      (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
      (request.send-data (rope->string reply) #t)
      ))

  (define (handle-shutdown request)
    (let ((reply (html "<!doctype html>" (html (body (h2 "shutting down..."))))))
      (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
      (request.send-data (rope->string reply) #t)
      (timeq/sleep 500) ;; give peace a chance...
      (%exit #f 1)
      ))

  (lambda (request)
    (if (header-matches? request starts-with ":path" "/status") ;; quick test
        (match (unpack-request request) with
          (maybe:yes (:tuple method parts params fragment))
          -> (begin
               ;;(debugf "status handler " (string method) " " (join "/" parts) "\n")
               (match method parts with
                 "GET" ("" "status" "addr" addr . tl)
                 -> (maybe:yes (lambda (request) (handle-host request addr)))
                 "GET" ("" "status" "requests" addr "")
                 -> (maybe:yes (lambda (request) (handle-requests request addr)))
                 "GET" ("" "status" "requests" addr num)
                 -> (maybe:yes (lambda (request) (handle-single-request request addr num)))
                 "GET" ("" "status" "gc")
                 -> (maybe:yes handle-requests-gc)
                 "GET" ("" "status" "addrs" "")
                 -> (maybe:yes handle-addrs)
                 "GET" ("" "status" "shutdown")
                 -> (maybe:yes handle-shutdown)
                 "GET" ("" "status" "")
                 -> (maybe:yes handle-toplevel)
                 _ _
                 -> (maybe:no)
                 ))
          _
          -> (maybe:no)
          )
        (maybe:no)
        ))
  )
  

;; --------------------------------------------------------------------------------

;; wraps file-handler with a hostname prefix/directory.
                       
(define hostname-legal?
  (let ((element-regex (regex/compile "[A-Za-z0-9-][A-Za-z0-9-]*[A-Za-z0-9]")))
    (lambda (s)
      (let ((parts (string-split s #\.)))
	(and (< (string-length s) 254)
	     (all? (lambda (x) (and (< (string-length x) 64) (> (string-length x) 0))) parts)
	     (all? (lambda (x) (element-regex x)) parts))))
    ))

(define (host-handler root)

  (define (get-host authority)
    (match (string-split authority #\:) with
      (host port) -> host
      (host)      -> host
      _           -> ""
      ))

  (lambda (request)
    (match (lookup-header ":authority" request.headers) with
      (maybe:yes authority)
      -> (let ((host (get-host authority)))
	   (if (hostname-legal? host)
	     (let ((joined (path/make root host)))
	       (if (not (isdir? joined))
                   (maybe:no)
		   (maybe:yes (lambda (request) (file-handler joined request)))))
             (maybe:no)))
      (maybe:no)
      -> (maybe:no)
      )))

;; --------------------------------------------------------------------------------
(define (dummy-handler prefix)
  (lambda (request)
    (if (header-matches? request starts-with ":path" prefix)
        (maybe:yes (lambda (request)
                     (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
                     (request.send-data "<html><body><h1>dummy handler</h1></body></html>" #t)))
        (maybe:no))))

;; --------------------------------------------------------------------------------
;; place `<link rel="icon" href="favicon.svg">` in your html.

(define lambda-favicon
"<svg viewBox=\"0 0 512 512\" xmlns=\"http://www.w3.org/2000/svg\">
  <g id=\"_10_Lambda\" data-name=\"10 Lambda\">
    <path d=\"m426.4 374a42.79 42.79 0 0 0 -57.83 17.8 119.58 119.58 0 0 1 -12 18.91 83.35 83.35 0 0 1 -7.57-12.42c-6.43-29-41.89-183.29-94.85-304.36-11.22-25.65-23.31-44.69-36.95-58.19-20.94-20.74-42.2-24.92-56.44-24.74-19.28.21-47.88 8.58-74.6 47.22a211.61 211.61 0 0 0 -20.11 36.6 42.78 42.78 0 1 0 79.14 32.51 118.4 118.4 0 0 1 15.69-26.23c3.59 4.78 8.75 13.09 14.88 27.12 3.78 8.64 7.48 17.5 11.08 26.51a123 123 0 0 1 -10.32 111.46l-107 169.14a42.78 42.78 0 1 0 72.3 45.76l100.09-158.21c15.39 56.12 24.26 97.12 24.43 97.92a42.52 42.52 0 0 0 2.28 7.32c4.82 11.71 31.54 70.25 84.37 72.79 1.21.06 2.41.09 3.61.09 33.84 0 63.29-23.24 87.59-69.15a42.78 42.78 0 0 0 -17.79-57.85z\">
    </path>
  </g>
</svg>")

(define (favicon-handler request)
  (match (lookup-header ":path" request.headers) with
    (maybe:yes "/favicon.svg")
    -> (maybe:yes
        (lambda (request)
          (request.send-headers "200" (list (:tuple "content-type" "image/svg+xml")) #t)
          (request.send-data lambda-favicon #t)))
    _
    -> (maybe:no)
    ))

;; --------------------------------------------------------------------------------
;; basic authentication

(define (basic-auth-wrapper usermap realm path-prefix other-handler)

  (lambda (request)

    (define (check-auth request)
      (match (lookup-header "authorization" request.headers) with
        (maybe:yes auth)
        -> (match (string-split auth #\space) with
             ("Basic" b64auth)
             -> (match (string-split (b64-decode b64auth) #\:) with
                  (user pass)
                  -> (match (tree/member usermap string-cmp user) with
                       (maybe:yes pass0)
                       -> (string=? pass pass0)
                       _ -> #f
                       )
                  _ -> #f
                  )
             _ -> #f
             )
        _ -> #f
        ))

    (define (handle request)
      (if (check-auth request)
          (match (other-handler request) with
            (maybe:yes fun) -> (fun request)
            _ -> (send-status request "404"))
          (request.send-headers "401"
                                (list (:tuple "www-authenticate" (format "Basic realm=\"" realm "\"")))
                                #f)))

    (if (header-matches? request starts-with ":path" path-prefix)
        (maybe:yes handle)
        (maybe:no))))

;; --------------------------------------------------------------------------------
;; this is the top-level handler passed into the h2 code.
;; --------------------------------------------------------------------------------

(define (h2-handler sub-handler)
  (lambda (request)
    (match (sub-handler request) with
      (maybe:yes fun) -> (begin (fun request) (note-request request))
      (maybe:no)      -> (send-404 request)
      )))
