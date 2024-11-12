;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/random.scm")
(require "lib/mtwist.scm")

(require "lib/codecs/zlib.scm")
;;(require "lib/codecs/brotli.scm")
(require "lib/codecs/base85.scm")
(require "lib/codecs/base64.scm")

(require "doom/tls/tls13.scm")
(require "doom/socket.scm")
(require "doom/http/h2.scm")
(require "doom/http/html.scm")

(require "demo/maze/gen.scm")
(require "demo/maze/solve.scm")
(require "demo/maze/svg2.scm")
(require "demo/maze/js.scm")
(require "demo/maze/ascii.scm")

(define format-header
  (:tuple name val) -> (html (dt name) (dd val))
  )

(define header->sexp
  (:tuple name val) -> (sexp (sym (string->symbol name)) (string val))
  )

;; XXX maybe use a multi-map for headers?
(define lookup-header
  key () -> (maybe:no)
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
          _         -> #u)))
    kvmap))

(define (decode-uri uri)
  (let ((parts0 (string-split uri #\?)))
    (match parts0 with
      (base)                 -> (:tuple (string-split base #\/) (tree/empty))
      (base params . ignore) -> (:tuple (string-split base #\/) (decode-params params))
      ()                     -> (:tuple '() (tree/empty))
      )))

(define (get-int-param params key default)
  (match (tree/member params string-compare key) with
    (maybe:yes val)
    -> (if (all? digit? (string->list val))
           (string->int val)
           default)
    (maybe:no)
    -> default
    ))

(datatype content-encoding
  ;; (:brotli)
  (:zlib)
  (:none))

(define *content-encoding*
  ;;(content-encoding:none)
  (content-encoding:zlib)
  ;;(content-encoding:brotli)
  )

(define (apply-content-encoding gen)
  (match *content-encoding* with
    ;; (content-encoding:brotli) -> (brotli/encode gen 16384 1)
    (content-encoding:zlib)   -> (deflate gen 16384 1)
    (content-encoding:none)   -> gen
    ))

(define (get-content-encoding)
  (match *content-encoding* with
    ;; (content-encoding:brotli) -> "br"
    (content-encoding:zlib)   -> "deflate"
    (content-encoding:none)   -> "identity"
    ))

(define html-form
  (html
   (form
    (fieldset
     (legend "maze type")
     (div ((input (type "radio") (name "svg") (id "svg")))
          ((label (for "svg")) "svg"))
     (div ((input (type "radio") (name "js") (id "js")))
          ((label (for "js")) "javascript"))
     (div ((input (type "radio") (name "b64") (id "b64")))
          ((label (for "b64")) "base64"))
     )
    ((button (name "submit") (type "submit")) "submit")
    )))

(define (char-gen->string-gen g)
  (makegen emit
    (let loop ((chunk (gen-take 80 g)))
      (when (not (null? chunk))
        (emit (list->string chunk))
        (loop (gen-take 80 g))))))

(define (maze-data-gen maze w h)
  (char-gen->string-gen (bytegen maze w h)))

(define (get-file-gen path)
  ;; XXX temp path hack
  (make-file-generator
   (file/open-read
    path
    ;;(format "demo/maze/" path)
    )))

(define (check-path path)
  (match path with
    "game.js"   -> (maybe:yes (:tuple "text/javascript" (get-file-gen path)))
    "game.html" -> (maybe:yes (:tuple "text/html" (get-file-gen path)))
    _           -> (maybe:no)
    ))

(define (maze-handler request)

  (define (clamp lo hi val)
    (max lo (min hi val)))

  (define (send-404 request)
    (request.send-headers (list (:tuple ":status" "404")) #f))

  (let ((path (lookup-header ":path" request.headers))
        (conn (request.get-conn)))
    (debugf "request: " (string (if-maybe p path p "none")) "\n")
    (match path with
      (maybe:no) -> (send-404 request)
      (maybe:yes path)
      -> (cond ((starts-with path "/svg")
                (match (decode-uri path) with
                  (:tuple parts params)
                  -> (let ((w (clamp 10 300 (get-int-param params "w" 100)))
                           (h (clamp 10 300 (get-int-param params "h" 70)))
                           (bias (clamp -50 50 (get-int-param params "bias" 0)))
                           (scale (clamp 2 50 (get-int-param params "scale" 10)))
                           (solve? (>0 (get-int-param params "solve" 0)))
                           (seed (get-int-param params "seed" (random))))
                       (request.send-headers
                        (list (:tuple ":status" "200")
                              (:tuple "content-type" "image/svg+xml")
                              (:tuple "content-encoding" (get-content-encoding)))
                        #t)
                       (let ((maze (make-maze w h seed bias))
                             (solution (if solve? (BFS maze w h) '()))
                             (content-gen (graph-svg-gen maze w h scale solution seed))
                             (zgen (apply-content-encoding content-gen)))
                         (for (last? chunk) (notify-last-gen zgen)
                           (request.send-data chunk last?))))))
               ((string=? path "/")
                (request.send-headers
                 (list (:tuple ":status" "302")
                       (:tuple "location" "/game.html"))
                 #f))
               ;; test target that places many svg images on a single page.
               ;; [mostly trying to get lots of parallel request activity]
               ((starts-with path "/many/")
                (match (decode-uri path) with
                  (:tuple parts params)
                  -> (let ((w (clamp 10 100 (get-int-param params "w" 50)))
                           (h (clamp 10 70 (get-int-param params "h" 20)))
                           (n (clamp 1 20 (get-int-param params "n" 10)))
                           (scale (clamp 2 50 (get-int-param params "scale" 10)))
                           (solve? (>0 (get-int-param params "solve" 0)))
                           (seed (get-int-param params "seed" (random)))
                           (urls '()))
                       (for-range i n
                         (push! urls
                                (format
                                 "<image src=\"/svg?"
                                 "w=" (int w)
                                 "&h=" (int h)
                                 "&solve=" (int (if solve? 1 0))
                                 "&seed=" (int (+ seed i))
                                 "\"><br>")))
                       (request.send-headers
                        (list (:tuple ":status" "200")
                              (:tuple "content-type" "text/html")) #t)
                       (request.send-data
                        (format
                         "<!DOCTYPE html>\n<html><body>\n"
                         (join "<br>" (reverse urls))
                         "</body></html>")
                        #t))))
               (else
                ;; deliver files and 404's
                (match (decode-uri path) with
                  (:tuple ("" path . ignore) params)
                  -> (match (check-path path) with
                       (maybe:yes (:tuple ctype fgen))
                       -> (begin
                            (request.send-headers
                             (list (:tuple ":status" "200")
                                   (:tuple "content-type" ctype)) #t)
                            (for (last? chunk) (notify-last-gen fgen)
                              (request.send-data chunk last?)))
                       _ -> (send-404 request)
                       )
                  x -> (begin
                         (printn x)
                         (send-404 request))
                  ))
               ))))

(define (h2-channel h2-handler x509s skey alpns)
  (lambda (sockfun)
    (poller/fork
     (lambda ()
       (try
        (let ((sock (tls13/make-channel sockfun x509s skey alpns)))
          (try
           ((make-h2-conn sock h2-handler))
           except
           (:Doom/EOF _)
           -> #u
           (:H2/BadFrame f)
           -> (debugf "conn exit - bad frame\n")
           (:H2/BadPreface p)
           -> (debugf "conn exit - bad preface: " (string p) "\n")
           (:TLS/Alert kind what)
           -> (debugf "tls alert: " what "\n")
           (:TLS/Failed desc) ;; fixme
           -> (debugf "tls failed [1].\n")
           (:OSError n)
           -> (debugf "conn [1] OSError " (int n) "\n")
           )
          (sock.close))
        except
        (:TLS/Failed (maybe:no)) ;; fixme
        -> (debugf "tls failed [2].\n")
        (:OSError n)
        -> (debugf "conn [2] OSError " (int n) "\n")
        )
       ))))

;; handler := (lambda (sockfun) ...)
(define (serve ip port handler)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (debugf "starting h2s server... on " (address/unparse addr) "\n")
    (debugf "  server fd = " (int (sock.get-fd)) "\n")
    (let loop ()
      (debugf "accept wait...\n")
      (let (((sockfun addr) (sock.accept)))
        (debugf "client: addr " (address/unparse addr) "\n")
        (handler sockfun)
        (loop))
      )))

(define (read-certs path)
  (with-file file (file/open-read path)
    (let ((pgen (pem-read-gen (file-char-generator file)))
          (result '()))
      (for pem pgen
        (match pem with
          (:tuple "CERTIFICATE" asn1) -> (push! result asn1)
          _ -> (raise (:TLS/ExpectedCertificate path))
          ))
      (reverse result))))

;; ----------------------------
;;
;; game state handler

(define (maze/make w h seed bias)
  {w=w h=h g=(make-maze w h seed bias) bias=bias seed=seed})

;; ws maze repr: two bytes of w,h followed by raw maze data.
(define (maze->data m)
  (assert (and (>= m.w 10) (< m.w 256) (>= m.h 10) (< m.h 256)))
  (let ((parts (generator->list (maze-data-gen m.g m.w m.h)))
        (enc-size (list->string (list #\M (int->char m.w) (int->char m.h)))))
    (string-concat (list:cons enc-size parts))))

(define next-player-id
  (let ((count 0))
    (lambda ()
      (inc! count)
      count)))

(define (player/make peer ws)
  (let ((pid (next-player-id)))
    {peer=peer
     score=0
     name=(format "player-" (int pid) "/" peer)
     ws=ws
     posx=0
     posy=0
     id=pid
     }
    ))

(define rand-range
  (let ((rng (mt19937 (get-seed))))
    (lambda (n)
      (mod (rng) n))))

(define (human-time s)
  (let (((m s) (divmod s 60)))
    (if (>0 m)
        (format (int m) "m " (int s) "s")
        (format (int s) "s"))))

(define (ws-handler)
  (let ((w 16) ;; starting values
        (h 12)
        (bias 0)
        (seed (get-seed))
        (m (maze/make w h seed bias))
        (mdata (maze->data m))
        (players (tree/empty))
        (gx (rand-range w))
        (gy (rand-range h))
        (goal-count 0)
        (last-goal-found (gettime/realtime))
        (level 1)
        )

    (define (reset)
      (set! w 16)
      (set! h 12)
      (set! level 1)
      (new-maze)
      (for-map k p players
        (set! p.posx (rand-range w))
        (set! p.posy (rand-range h))
        (set! gx (rand-range w))
        (set! gy (rand-range h))
        (p.ws.send-binary (encode #\f (list p.posx p.posy)) #t)
        (p.ws.send-binary (encode #\g (list gx gy)) #t)
        ))

    (define (split-command s)
      ;; (printf "cmd: " (string s) "\n")
      (let ((slen (string-length s)))
        (cond ((>0 slen)
               (:tuple (string-ref s 0) (substring s 1 slen)))
              (else
               (:tuple #\! "")))))

    (define (msg peer ws msg)
      (match (tree/member players string-compare peer) with
        (maybe:yes p)
        -> (match (split-command msg) with
             (:tuple #\m rest) -> (move p rest)
             (:tuple #\+ _)    -> (level-up)
             (:tuple #\! _)    -> (reset)
             _   -> #u)
        (maybe:no)
        -> #u))

    (define (new-conn peer ws)
      (let ((p (player/make peer ws)))
        (tree/insert! players string-compare peer p)
        (ws.send-binary mdata #t)
        (ws.send-binary (encode #\g (list gx gy)) #t)
        (ws.send-binary (format "N" p.name) #t)))

    (define (done peer)
      (tree/delete! players magic-cmp peer))

    (define (encode cmd data)
      (list->string (list:cons cmd (map int->char data))))

    (define (move p data)
      (let ((parts (map char->int (string->list data))))
        ;; (printf "move? " (int p.id) " " (join int->string "," parts) "\n")
        (match parts with
          (px py) -> (begin
                       (set! p.posx px)
                       (set! p.posy py)
                       (send-to-others p (encode #\m (list p.id px py))))

          _ -> #u
          )
        (when (and (= p.posx gx) (= p.posy gy))
          (let ((now (gettime/realtime))
                (delta (- now.sec last-goal-found.sec)))
            (send-to-all (format "nplayer " (int p.id)
                                 " captured level " (int level) " goal #" (int (+ 1 goal-count))
                                 " in " (human-time delta)))
            (new-goal))
          )))

    (define (new-maze)
      (set! seed (rand-range 1000000))
      (set! m (maze/make w h seed bias))
      (set! mdata (maze->data m))
      (send-to-all mdata)
      (set-goal gx gy)
      )

    (define (send-to-others me msg)
      (for-map k p players
        (when (not (eq? me p))
          (p.ws.send-binary msg #t))))

    (define (send-to-all msg)
      (for-map k p players
        (p.ws.send-binary msg #t)))

    (define (set-goal gx gy)
      (set! last-goal-found (gettime/realtime))
      (send-to-all (encode #\g (list gx gy))))

    (define (level-up)
      (set! goal-count 0)
      (inc! w (/ w 4))
      (inc! h (/ h 4))
      (inc! level)
      (cond ((= level 4) (set! bias 40))
            ((= level 5) (set! bias -40))
            ((= level 6) (set! bias 20)))
      ;; protocol currently uses uint8 everywhere.
      (if (or (> w 255) (> h 255))
          (reset)
          (new-maze)))

    (define (new-goal)
      (inc! goal-count)
      (when (>= goal-count 3)
        (level-up))
      (set! gx (rand-range w))
      (set! gy (rand-range h))
      (set-goal gx gy))

    {new=new-conn msg=msg done=done}
    ))

;; ----- websocket server -----

(require "doom/http/websocket.scm")

(define (wss-channel handler x509s skey alpns)
  (lambda (sockfun addr)
    (poller/fork
     (lambda ()
       (try
        (let ((sock (tls13/make-channel sockfun x509s skey alpns)))
          (try
           (wss-conn sock addr handler)
           except
           (:Doom/EOF _)
           -> #u
           (:TLS/Alert kind what)
           -> (debugf "tls alert: " what "\n")
           (:TLS/Failed)
           -> (debugf "tls failed [1].\n")
           (:OSError n)
           -> (debugf "conn [1] OSError " (int n) "\n")
           )
          (sock.close))
        except
        (:TLS/Failed)
        -> (debugf "tls failed [2].\n")
        (:OSError n)
        -> (debugf "conn [2] OSError " (int n) "\n")
        )
       ))))

(define (wss-conn sock addr handler)
  (let ((ws (websocket sock))
        (peer (address/unparse addr)))
    (handler.new peer ws)
    (for pkt ws.pktgen
      (match pkt with
        (:tuple _ data #t)
        -> (handler.msg peer ws data)
        (:tuple _ data #f)
        -> (raise (:H2Maze/UnfinishedWebsocketPacket data))
        ))
    (handler.done peer)
    (printf "exiting ws client...\n")
    (ws.close)
    ))

(define (wss-serve ip port channel)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (debugf "starting websocket server...\n")
    (let loop ()
      (let (((sockfun addr) (sock.accept))
            (addr0 (address/unparse addr)))
        (debugf "ws client: addr " addr0 "\n")
        (channel sockfun addr)
        (loop)))))

;; ------------------------------

(define (main)
  (let ((x509s (read-certs sys.argv[1]))
        (skey (read-skey sys.argv[2]))
        (mhandler (ws-handler)))
    (debugf "read " (int (length x509s)) " certs.\n")
    (poller/fork (lambda () (wss-serve "0.0.0.0" 4434 (wss-channel mhandler x509s skey '()))))
    (serve "0.0.0.0" 4433 (h2-channel maze-handler x509s skey '("h2")))
    (poller/wait-and-schedule)))

(main)
