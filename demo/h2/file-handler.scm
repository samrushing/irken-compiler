;; -*- Mode: Irken -*-

(require "lib/mimetypes.scm")
(require "lib/codecs/zlib.scm")

(define (is-binary-file? path)
  (with-file file (file/open-read path)
    (let loop ((n 100)
	       (count 0)
	       (ch (file/read-char file)))
      (if (= n 0)
	  (> count 30)
	  (if (> (char->int ch) 126)
	      (loop (- n 1) (+ count 1) (file/read-char file))
	      (loop (- n 1)    count    (file/read-char file))
	      )))))

(define (guess-unknown-mime-type path)
  (if (is-binary-file? path)
      "application/octet-stream"
      "text/plain"))

(define (get-mime-type path)
  (define (look path)
    (let ((parts (string-split path #\/)))
      (if (> (length parts) 0)
	  (let ((exts (string-split (last parts) #\.)))
	    (if (> (length exts) 0)
		(guess-mime-type (downcase (last exts)))
		(maybe:no)))
	  (maybe:no))))
  (match (look path) with
    (maybe:yes t) -> t
    (maybe:no)    -> (guess-unknown-mime-type path)
    ))

(define compressible-mime-type-set
  ;; mime types that should probably be compressed on-the-fly
  (set/make string-cmp
   "application/json"
   "image/svg+xml"
   "text/csv"
   "text/html"
   "text/xml"
   "text/plain"
   ))

(define (compress-mime-type? t)
  (set/member? compressible-mime-type-set string-cmp t)
  )

(define table-css
  "body { font-family: monospace; }
  tr:nth-child(odd) {
    background-color:#f0f0f0;
  }
  tr:nth-child(even) {
    background-color:#e0e0e0;
  }
  .ellipsis {
    text-overflow: ellipsis;
    overflow: hidden;
    width:20em;
    display:block;
  }"
  )


(define (apply-zlib-encoding gen)
  (deflate gen 16384 1))

(define (gen-file-listing-html root path)

  (debugf "gen-file-listing root = " (string root) " path = " (string path) "\n")

  ;; we don't want the long version from http headers here.
  (define (format-timestamp time_t)
    (let ((tm (localtime time_t)))
      (format (int tm.year)
              "-" (zpad 2 (int (+ 1 tm.mon)))
              "-" (zpad 2 (int tm.mday))
              " " (zpad 2 (int tm.hour))
              ":" (zpad 2 (int tm.min))
              ":" (zpad 2 (int tm.sec))
              )))

  (define (gen-file-row name)
    ;; (debugf "gen-file-row path = " (string path) " name " (string name) "\n")
    (let ((info (stat (path/make root path name))))
      (rope->string
       (html (tr (td (&s (format-timestamp info.mtime)))
                 ((td (align "right")) (&f (int info.size)))
		 (td ((a (href name)) name))
		 )))
      ))

  (let ((files0 (listdir (path/make root path)))
        (files1 (sort (lambda (a b) (string<? a.name b.name)) files0)))
    (makegen emit
      (emit (format "<html><body><style type=\"text/css\">"
		    table-css
		    "</style><table><tr><th>modified</th><th>size</th><th>name</th></tr>"))
      (for-list entry files1
	;; (debugf "entry " (sym entry.kind) " " (string entry.name) "\n")
        (if (not (eq? #\. (string-ref entry.name 0)))
	    (match entry.kind with
	      'regular   -> (emit (gen-file-row entry.name))
	      'directory -> (emit (gen-file-row (format entry.name "/")))
	      _          -> #u)
            #u))
      (emit "</table></body></html>")
      )))

(define (file-closing-generator path)
  (let ((ifile (file/open-read path)))
    (makegen emit
      (let loop ((buf (file/read-buffer ifile)))
        (when (> (string-length buf) 0)
          (emit buf)
          (loop (file/read-buffer ifile))))
      (file/close ifile))))

;; RFC3986

(define (remove-dot-segments parts)
  (let loop ((acc (if (null? parts) (list "") (list:nil)))
             (parts parts))
    (match parts acc with
      (".." . ptl)  (ahd . atl) -> (loop atl ptl)
      (".." . ptl)  _           -> (loop acc ptl)
      ("." . ptl)   _           -> (loop acc ptl)
      (".")         _           -> (loop acc (list:nil))
      ("..")        _           -> (loop acc (list:nil))
      ("" "" . ptl) _           -> (loop (list:cons "" acc) ptl) ;; ignore redundant //
      (phd . ptl)   _           -> (loop (list:cons phd acc) ptl)
      ()            _           -> (reverse acc)
      )))

;; Note: this is a per-request handler, not a matching handler.

(define (file-handler root request)

  ;; cases:
  ;; 1) directory
  ;;   1a) index.html present
  ;;   1b) gen file listing
  ;; 2) file present
  ;; 3) no such file

  (define (format-etag info)
    (format "W/\"" (int info.mtime) "." (int info.size) "\""))

  (define (file-newer? request info)
    (match (lookup-header "if-none-match" request.headers) with
      (maybe:yes inm)
      -> (let ((w-etag (format-etag info))
               (tags (map string-trim-both (string-split inm #\,))))
           (not (member? w-etag tags string=?)))
      (maybe:no)
      -> #t ;; no if-none-match header
      ))

  (define (get-expires-header)
    (let ((ts (gettime/realtime)))
      (format-timestamp (+ *expires-delta* ts.sec))))

  (define (send-file request full-path)
    ;; (debugf "send-file: full-path = " (string full-path) "\n")
    (let ((info (stat full-path))
          (mime-type (get-mime-type full-path))
          (etag (format-etag info))
          (compress? (compress-mime-type? mime-type))
          ;;(compress? #f)
	  (headers (list (:tuple "content-type" mime-type)
                         (:tuple "etag" etag)
                         (:tuple "expires" (get-expires-header))
                         ;; XXX this header refers to the possibly-compressed length,
                         ;;     which confuses safari...
			 ;;(:tuple "content-length" (int->string info.size))
                         )))
      (if (file-newer? request info)
          (let ((fgen0 (file-closing-generator full-path))
                (fgen1 (if compress? (apply-zlib-encoding fgen0) fgen0))
                (headers0 (if compress? (list:cons (:tuple "content-encoding" "deflate") headers) headers)))
            ;; this triggers a bug/fault that I think happens when capturing a continuation
            ;;    in `set!`.  avoided by using let bindings instead.
            (request.send-headers "200" headers0 #t)
            (request.send-with-generator fgen1))
          (request.send-headers "304" headers #f)) ;; not modified
      ))

  (define (send-directory-listing request relative-path)
    (let ((lgen (gen-file-listing-html root relative-path)))
      (request.send-headers "200" (list (:tuple "content-type" "text/html")) #t)
      (request.send-with-generator lgen)
      ))

  (define (send-path request parts)
    (try
     (let ((norminal (remove-dot-segments parts))
           (relative (format (join "/" norminal)))
	   (rooted (path/make root relative)))
       ;;(debugf "send-path: relative = " (string relative) " rooted = " (string rooted) "\n")
       (if (string=? (last norminal) "") ;; a directory
	   (let ((index-probe (path/make rooted "index.html")))
             (if (isfile? index-probe)
                 (send-file request index-probe)
                 (send-directory-listing request relative)))
           (send-file request rooted))
       ;;(debugf "nano " (int (request.get-elapsed-time)) " " (string relative) "\n")
       )
     except
     (:OSError _)
     -> (send-404 request)
     ))

  (define (log request path)
    (define (timestamp)
      (let ((now (clock-gettime CLOCK_REALTIME))
            (frac (/ now.nsec 10000000))
            (tm (localtime now.sec)))
        (format (int tm.year)
                "-" (zpad 2 (int (+ 1 tm.mon)))
                "-" (zpad 2 (int tm.mday))
                " " (zpad 2 (int tm.hour))
                ":" (zpad 2 (int tm.min))
                ":" (zpad 2 (int tm.sec))
                "." (zpad 2 (int frac))
                )))
    (when-maybe log *logfile*
      (let ((info (request.get-info))
            (method (if-maybe a (lookup-header ":method" request.headers) a "none"))
            (authority (if-maybe a (lookup-header ":authority" request.headers) a "none"))
            ;; XXX this should be done ONCE, at connection time.
            (line (format (timestamp)
                          " " info.addr
                          " A" authority
                          " M" method
                          " P" path
                          " S" info.status
                          " T" (int info.elapsed)
                          " B" (int info.bytes)
                          "\n")))
        (file/write log line)
        (file/flush log)
        )))

  (let ((uri (lookup-header ":path" request.headers))
	(method (lookup-header ":method" request.headers)))
    ;;(debugf "request: " (string (if-maybe p uri p "none")) "\n")
    (match method with
      (maybe:yes "GET")
      -> (match uri with
	   (maybe:yes uri) ;; deliver files and 404's
	   -> (match (decode-uri uri) with
		(maybe:yes (:tuple parts params fragment))
                -> (send-path request parts)
                (maybe:no) ;; malformed URI
                -> (send-status request "400") ;; bad request
                )
	   (maybe:no)
	   -> (send-status request "400") ;; bad request
	   )
      otherwise
      -> (send-status request "405") ;; method not allowed.
      )
    (log request (if-maybe p uri p "none")))
  ) ;; file-handler


