;; -*- Mode: Irken -*-

;; XXX choose either 'get/put' or 'pack/unpack', let's not use both.

;; XXX rather than manually calling pack-xxx for each kind and layer,
;;   we should probably use the datatypes directly, so the only 'pack'
;;   function called is a high-level one, pack-record, that would accept
;;   a `tls-record` type rather than a rope/string.  this will simplify
;;   the code and make it safer.

;; --- unpack ---

;; XXX this design of returning (:tuple ob pos) is quite clumsy,
;;   I think I want to rewrite this to use a record holding the
;;   current position.

(define (int-at s pos)
  (char->int (string-ref s pos)))

(define (get-u8 s pos)
  (:tuple (int-at s pos) (+ pos 1)))

(define (get-u16 s pos)
  (let ((b0 (int-at s (+ pos 0)))
        (b1 (int-at s (+ pos 1))))
    (:tuple (logior (<< b0 8) b1) (+ pos 2))
    ))

(define (get-u24 s pos)
  (let ((b0 (int-at s (+ pos 0)))
        (b1 (int-at s (+ pos 1)))
        (b2 (int-at s (+ pos 2))))
    (:tuple (logior* (<< b0 16) (<< b1 8) b2) (+ pos 3))
    ))

(define (get-vector get-size getsub s pos)
  (let (((size pos) (get-size s pos))
        (stop (+ size pos)))
    (let loop ((acc '())
               (pos pos))
      (cond ((= pos stop) (:tuple (reverse acc) pos))
            ((> pos stop) (raise (:TLS/Underflow pos s)))
            (else
             (let (((item pos0) (getsub s pos)))
               (loop (list:cons item acc) pos0)))))))

(define (get-opaque n s pos)
  (:tuple (substring s pos (+ pos n)) (+ pos n)))

(define (get-vector-8 getsub s pos)
  (get-vector get-u8 getsub s pos))

(define (get-vector-16 getsub s pos)
  (get-vector get-u16 getsub s pos))

(define (get-vector-opaque get-size s pos)
  (let (((size pos) (get-size s pos)))
    (get-opaque size s pos)))

;; --- pack ---

;; packing is done with ropes rather than strings.

(define (as-char n)
  (int->char (logand #xff n)))

(define (put-u8 n)
  (rope:leaf (char->string (as-char n))))

(define (put-u16 n)
  (let ((s (make-string 2)))
    (string-set! s 0 (as-char (>> n 8)))
    (string-set! s 1 (as-char (>> n 0)))
    (rope:leaf s)))

(define (put-u24 n)
  (let ((s (make-string 3)))
    (string-set! s 0 (as-char (>> n 16)))
    (string-set! s 1 (as-char (>> n 8)))
    (string-set! s 2 (as-char (>> n 0)))
    (rope:leaf s)))

(define (put-vector putsub items)
  (rope/cat (map putsub items)))

(define (put-var-8 r)
  (let ((rlen (rope-length r)))
    (if (> rlen #xff)
        (raise (:TLS/Overflow))
        (rope-make (put-u8 rlen) r))))

(define (put-var-16 r)
  (let ((rlen (rope-length r)))
    (if (> rlen #xffff)
        (raise (:TLS/Overflow))
        (rope-make (put-u16 rlen) r))))

(define (put-var-24 r)
  (let ((rlen (rope-length r)))
    (if (> rlen #xffffff)
        (raise (:TLS/Overflow))
        (rope-make (put-u24 rlen) r))))

(define (put-vector-8 putsub items)
  (put-var-8 (put-vector putsub items)))

(define (put-vector-16 putsub items)
  (put-var-16 (put-vector putsub items)))

(define (put-vector-24 putsub items)
  (put-var-24 (put-vector putsub items)))

(define (put-opaque-8 s)
  (put-var-8 (rope:leaf s)))

(define (put-opaque-16 s)
  (put-var-16 (rope:leaf s)))

(define (put-opaque-24 s)
  (put-var-24 (rope:leaf s)))

(define (put-opaque s)
  (rope:leaf s))

;; --------------------------------------------------------------------------------

(define (get-host-name s pos)
  (get-vector-opaque get-u16 s pos))

(define (get-protocol-name s pos)
  (get-vector-opaque get-u8 s pos))

(define (get-server-name s pos)
  (let (((ntype pos) (get-u8 s pos)))
    (match ntype with
       0 -> (get-host-name s pos)
      _  -> (raise (:TLS/BadValue "NameType" ntype))
      )))

(define (get-key-share s pos)
  (let (((group pos) (get-u16 s pos))
        ((kex pos) (get-vector-opaque get-u16 s pos)))
    (:tuple {group=group kex=kex} pos)))

;; XXX the shape of these extensions can vary between client and server.
;;     e.g. the key_share extension is a vector in ClientHello, but a single
;;          element in ServerHello.  So we need to either parameterize this
;;          or make two different versions.  [though we might paper around it
;;          by using a single-element list]
(define (get-extension-by-type etype s pos)
  (match etype with
    00 -> (let (((names pos) (get-vector-16 get-server-name s pos)))
            (:tuple (tlsext:server-names names) pos))
    10 -> (let (((groups pos) (get-vector-16 get-u16 s pos)))
            (:tuple (tlsext:named-groups groups) pos))
    13 -> (let (((algos pos) (get-vector-16 get-u16 s pos)))
            (:tuple (tlsext:sigalgs algos) pos))
    16 -> (let (((alpns pos) (get-vector-16 get-protocol-name s pos)))
            (:tuple (tlsext:alpn alpns) pos))
    43 -> (let (((versions pos) (get-vector-8 get-u16 s pos)))
            (:tuple (tlsext:supported-versions versions) pos))
    45 -> (let (((modes pos) (get-vector-8 get-u8 s pos)))
            (:tuple (tlsext:psk-kex-modes modes) pos))
    50 -> (let (((algos pos) (get-vector-16 get-u16 s pos)))
            (:tuple (tlsext:sigalgs-cert algos) pos))
    51 -> (let (((shares pos) (get-vector-16 get-key-share s pos)))
            (:tuple (tlsext:client-shares shares) pos))
    21 -> (:tuple (tlsext:padding (string-length s)) (+ pos (string-length s)))
    xx -> (:tuple (tlsext:other xx s) (+ pos (string-length s)))
    ))

(define (get-extension s pos)
  (let (((etype pos) (get-u16 s pos))
        ((edata pos) (get-vector-opaque get-u16 s pos))
        ((ext pos0) (get-extension-by-type etype edata 0)))
    ;; XXX use an exception
    (assert (= pos0 (string-length edata)))
    (:tuple ext pos)))

;; XXX can we define these packers/unpackers declaratively?
(define (unpack-client-hello s pos len)
  (let (((legacy-version pos) (get-u16 s pos))
        ((random pos) (get-opaque 32 s pos))
        ((legacy-session-id pos) (get-vector-opaque get-u8 s pos))
        ((cipher-suites pos) (get-vector-16 get-u16 s pos))
        ((legacy-compression-methods pos) (get-vector-opaque get-u8 s pos))
        ((extensions pos) (get-vector-16 get-extension s pos))
        )
    (tls-hsk:client-hello
     {legver=legacy-version
      random=random
      sessid=legacy-session-id
      suites=cipher-suites
      exts=extensions}
     )))

(define (share->sexp share)
  (sexp (sym (string->symbol (named-group->string share.group)))
        (string (string->hex share.kex))))

(define extension->sexp
  (tlsext:server-names names)          -> (sexp1 'server-names (map sexp:string names))
  (tlsext:named-groups groups)         -> (sexp1 'named-groups (map sexp:string (map named-group->string groups)))
  (tlsext:sigalgs algos)               -> (sexp1 'signature-algorithms (map sexp:string (map sigalg->string algos)))
  (tlsext:supported-versions versions) -> (sexp1 'supported-versions (map sexp:int versions))
  (tlsext:psk-kex-modes modes)         -> (sexp1 'psk-kex-modes (map sexp:int modes))
  (tlsext:sigalgs-cert algos)          -> (sexp1 'sigalgs-cert (map sexp:string (map sigalg->string algos)))
  (tlsext:client-shares shares)        -> (sexp1 'client-shares (map share->sexp shares))
  (tlsext:key-share share)             -> (sexp (sym 'key-share) (share->sexp share))
  (tlsext:padding len)                 -> (sexp (sym 'padding) (int len))
  (tlsext:alpn protos)                 -> (sexp1 'alpn (map sexp:string protos))
  (tlsext:other etype data)            -> (sexp (sym 'other) (int etype) (string (string->hex data)))
  )

(define (client-hello->sexp ch)
  (sexp
   (sym 'client-hello)
   (sexp (sym 'legacy-version) (int ch.legver))
   (sexp (sym 'random) (string (string->hex ch.random)))
   (sexp1 'suites (map sexp:string (map suite->string ch.suites)))
   (sexp (sym 'legacy-session-id) (string (string->hex ch.sessid)))
   (sexp1 'extensions (map extension->sexp ch.exts))
   ))

;; XXX kinda broken because extensions can differ between client & server.
(define (unpack-server-hello s pos len)
  (let (((legacy-version pos) (get-u16 s pos))
        ((random pos) (get-opaque 32 s pos))
        ((legacy-session-id pos) (get-vector-opaque get-u8 s pos))
        ((cipher-suite pos) (get-u16 s pos))
        ((legacy-compression-methods pos) (get-u8 s pos))
        ((extensions pos) (get-vector-16 get-extension s pos))
        )
    (tls-hsk:server-hello
     {legver=legacy-version
      random=random
      sessid=legacy-session-id
      suite=cipher-suite
      exts=extensions})))

;; (define (print-server-hello sh)
;;   (printf "(server-hello\n")
;;   (printf "  (legacy-version " (hex sh.legver) "\n"
;;           "  (random " (string->hex sh.random) ")\n"
;;           "  (legacy-session-id " (string->hex sh.sessid) ")\n"
;;           "  (suite " (hex sh.suite) ")\n")
;;   (printf "  (extensions \n")
;;   (for-list ext sh.extensions
;;     (printf "    " (extension-repr ext) "\n"))
;;   (printf "    ))\n")
;;   )

(define (put-key-share ks)
  (rope-make
   (put-u16 ks.group)
   (put-opaque-16 ks.kex)))

(define (put-alpns alpns)
  (put-vector-16 put-opaque-8 alpns))

;; only need two to start, supported versions and key share.
(define (put-extension ext)
  (define encode
    ;; XXX since the enum is only used in two places, maybe let's not enum.
    (tlsext:supported-versions (v)) -> (:tuple 43 (put-u16 v))
    (tlsext:key-share ks)           -> (:tuple 51 (put-key-share ks))
    (tlsext:alpn alpns)             -> (:tuple 16 (put-alpns alpns))
    _                               -> (raise (:TLS/NotImplemented))
    )
  (let (((code ext0) (encode ext)))
    (rope/build (put-u16 code) (put-var-16 ext0))))

(define (unpack-header s pos)
  (let (((kind pos) (get-u8 s pos))
        ((version pos) (get-u16 s pos))
        ((length pos) (get-u16 s pos)))
    (:tuple kind version length pos)
    ))

(define (pack-header ctype len)
  (rope/build
   (put-u8 (tls-ctype->int ctype))
   (put-u16 #x303) ;; legacy version
   (put-u16 len)))

(define (pack-server-hello sh)
  (rope/build
   (put-u16 #x303)        ;; legacy version == tls1.2
   (put-opaque sh.random) ;; 32 bytes
   (put-opaque-8 sh.sessid)
   (put-u16 sh.suite) ;; server chosen cipher suite
   (put-u8 0)         ;; legacy compression method
   (put-vector-16 put-extension sh.exts)
   ))

(define (pack-enc-exts exts)
  (rope/build
   (put-vector-16 put-extension exts)))

(define (pack-hsk kind pkt)
  (rope/build
   (put-u8 (tls-hsk-type->int kind))
   (put-u24 (rope-length pkt))
   pkt))

(define (pack-alert level desc)
  (rope/build
   (put-u8 (tls-alert-level->int level))
   (put-u8 (tls-alert-desc->int desc))))

(define (unpack-hsk s pos)
  (let (((kind pos) (get-u8 s pos))
        ((len pos) (get-u24 s pos))
        (hstype (int->tls-hsk-type kind)))
    (tls-record:hsk
     (match hstype with
       (tls-hsk-type:client-hello) -> (unpack-client-hello s pos len)
       (tls-hsk-type:server-hello) -> (unpack-server-hello s pos len)
       (tls-hsk-type:finished)     -> (unpack-finished s pos len)
       _ -> (raise (:NotImplemented))
       ))))

(define (unpack-alert s pos)
  (let (((level pos) (get-u8 s pos))
        ((desc pos) (get-u8 s pos)))
    (tls-record:alert
     (int->tls-alert-level level)
     (int->tls-alert-desc desc))))

(define (unpack-appdata s pos)
  (tls-record:appdata s))

(define (unpack-finished s pos len)
  (tls-hsk:finished (substring s pos (+ pos len))))

(define (unpack-record kind s pos)
  ;;(printf "unpack record kind = " (int kind) "\n")
  (let ((ctype (int->tls-ctype kind)))
    (match ctype with
      (tls-ctype:hsk)                -> (unpack-hsk s pos)
      (tls-ctype:alert)              -> (unpack-alert s pos)
      (tls-ctype:change-cipher-spec) -> (tls-record:change-cipher-spec)
      (tls-ctype:appdata)            -> (unpack-appdata s pos)
      x                              -> (raise (:NotImplemented))
      )
    ))

(define (put-cert-entry x509)
  (rope/build
   (put-opaque-24 x509)
   (put-vector-16 put-extension '())))

(define (put-cert x509-list)
  (rope/build
   (put-vector-8 put-u8 '()) ;; put-u8 is placeholder for put-cert-request-context
   (put-vector-24 put-cert-entry x509-list)))

(define (put-cert-verify scheme sig)
  (rope/build
   (put-u16 scheme)
   (put-opaque-16 sig)))
