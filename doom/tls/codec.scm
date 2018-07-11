;; -*- Mode: Irken -*-

;; XXX choose either 'get/put' or 'pack/unpack', let's not use both.
;;     [unless 'unpack' means we were given a length?]

;; --- unpack ---

(define (get-records get-fragment)

  (let ((kind -1)
        (buf "")
        (blen 0)
        (pos 0)       ;; current read position
        (vpos 0)      ;; stream read position
        (current '()) ;; pieces of the current record
        )

    (define (assure n)
      (when (> n (- blen pos))
        (PUSH current buf)
        (let (((fkind frag) (get-fragment))
              (flen (string-length frag)))
          (when (not (or (= kind -1) (= fkind kind)))
            (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "unexpected handshake")))
          (cond ((> blen pos)
                 (set! buf (string-append (substring buf pos blen) frag))
                 (set! pos 0)
                 (set! blen (string-length buf)))
                ((= blen pos)
                 (set! buf frag)
                 (set! pos 0)
                 (set! blen flen)))
          (set! kind fkind)
          (assure n))))

    (define (bump exp n)
      (inc! pos n)
      (inc! vpos n)
      exp)

    (define (get-u8)
      (assure 1)
      (bump (char->int (string-ref buf pos)) 1))

    (define (get-opaque n)
      (assure n)
      (bump (substring buf pos (+ pos n)) n))

    (define (get-u16)
      (let ((b0 (get-u8))
            (b1 (get-u8)))
      (logior (<< b0 8) b1)))

    (define (get-u24)
      (let ((b0 (get-u8))
            (b1 (get-u8))
            (b2 (get-u8)))
        (logior* (<< b0 16) (<< b1 8) (<< b2 0))))

    (define (get-vector get-size getsub)
      (let ((size (get-size))
            (stop (+ size vpos)))
        (let loop ((acc '()))
          (cond ((= vpos stop) (reverse acc))
                ((> vpos stop) (raise (:TLS/Underflow)))
                (else
                 (let ((item (getsub)))
                   (loop (list:cons item acc))))))))

    (define (get-vector-8 getsub)
      (get-vector get-u8 getsub))

    (define (get-vector-16 getsub)
      (get-vector get-u16 getsub))

    (define (get-vector-opaque get-size)
      (get-opaque (get-size)))

    (define (get-host-name)
      (get-vector-opaque get-u16))

    (define (get-protocol-name)
      (get-vector-opaque get-u8))

    (define (get-server-name)
      (let ((ntype (get-u8)))
        (match ntype with
          0 -> (get-host-name)
          _  -> (raise (:TLS/BadValue "NameType" ntype))
          )))

    (define (get-key-share)
      (let ((group (get-u16))
            (kex (get-vector-opaque get-u16)))
        {group=group kex=kex}
        ))

    (define (get-padding elen)
      ;; XXX do we need to check the padding?
      (let ((padding (get-opaque elen)))
        elen))

    (define (get-other elen)
      (get-opaque elen))

    (define (get-maxfraglen)
      (match (get-u8) with
        1 -> 512
        2 -> 1024
        3 -> 2048
        4 -> 4096
        _ -> (raise (:TLS/Alert (tls-alert-desc:illegal-parameter) "bad maxfraglen value"))
        ))

    ;; XXX the shape of these extensions can vary between client and server.
    ;;     e.g. the key_share extension is a vector in ClientHello, but a single
    ;;          element in ServerHello.  So we need to either parameterize this
    ;;          or make two different versions.  [though we might paper around it
    ;;          by using a single-element list]
    ;; XXX or we could use a mode boolean arg to tls-unpacker?

    (define (get-extension-by-type etype elen)
      (match etype with
        00 -> (tlsext:server-names (get-vector-16 get-server-name))
        01 -> (tlsext:maxfraglen (get-maxfraglen))
        10 -> (tlsext:named-groups (get-vector-16 get-u16))
        13 -> (tlsext:sigalgs (get-vector-16 get-u16))
        16 -> (tlsext:alpn (get-vector-16 get-protocol-name))
        21 -> (tlsext:padding (get-padding elen))
        43 -> (tlsext:supported-versions (get-vector-8 get-u16))
        45 -> (tlsext:psk-kex-modes (get-vector-8 get-u8))
        50 -> (tlsext:sigalgs-cert (get-vector-16 get-u16))
        51 -> (tlsext:client-shares (get-vector-16 get-key-share))
        xx -> (tlsext:other xx (get-other elen))
        ))

    (define (get-extension)
      (let ((etype (get-u16))
            (elen (get-u16))
            (pos0 pos)
            (result (get-extension-by-type etype elen)))
        (when (not (= pos (+ pos0 elen)))
          (raise (:TLS/Alert (tls-alert-desc:decode-error) "bad extension length")))
        result))

    (define (unpack-client-hello len)
      (let ((pos0 pos)
            (legacy-version (get-u16))
            (random (get-opaque 32))
            (legacy-session-id (get-vector-opaque get-u8))
            (cipher-suites (get-vector-16 get-u16))
            (legacy-compression-methods (get-vector-opaque get-u8))
            (extensions (get-vector-16 get-extension))
            (result (tls-hsk:client-hello
                     {legver=legacy-version
                      random=random
                      sessid=legacy-session-id
                      suites=cipher-suites
                      exts=extensions})))
        (when (not (= pos (+ pos0 len)))
          (raise (:TLS/Alert (tls-alert-desc:decode-error) "bad length")))
        result))

    (define (unpack-finished len)
      (tls-hsk:finished (get-opaque len)))

    (define (unpack-alert)
      (let ((level (get-u8))
            (desc (get-u8)))
        (tls-record:alert
         (int->tls-alert-level level)
         (int->tls-alert-desc desc))))

    (define (unpack-appdata)
      (tls-record:appdata (get-opaque (- blen pos))))

    (define (unpack-hsk)
      (let ((kind (get-u8))
            (len (get-u24)))
        (tls-record:hsk
         (match (int->tls-hsk-type kind) with
           (tls-hsk-type:client-hello) -> (unpack-client-hello len)
           (tls-hsk-type:finished)     -> (unpack-finished len)
           _ -> (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "unexpected handshake"))
           ))))

    (define (unpack-change-cipher-spec)
      (let ((dummy (get-u8)))
        (if (= dummy #x01)
            (tls-record:change-cipher-spec)
            (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "unexpected ChangeCipherSpec")))))

    (define (unpack-record kind)
      (printf "unpack record kind = " (int kind) "\n")
      (let ((ctype (int->tls-ctype kind)))
        (match ctype with
          (tls-ctype:hsk)                -> (unpack-hsk)
          (tls-ctype:alert)              -> (unpack-alert)
          (tls-ctype:change-cipher-spec) -> (unpack-change-cipher-spec)
          (tls-ctype:appdata)            -> (unpack-appdata)
          x                              -> (raise (:NotImplemented)) ;; XXX raise alert
          )))

    ;; read one or more full records.
    (let ((records '())
          (start vpos))
      (assure 1) ;; we get a record kind with the first fragment
      (while (> (- blen pos) 0)
        (let ((record (unpack-record kind))
              ;; reconstruct full raw packet.
              (last (substring buf 0 pos))
              (pkt (string-concat (reverse (list:cons last current))))
              (plen (string-length pkt)))
          (PUSH records {raw=pkt record=record kind=kind version=#x303 length=plen})))
      (reverse records))
    ))

(define (unpack-header s)
  (match (map char->int (string->list s)) with
    (k v0 v1 l0 l1) -> (:tuple k (logior (<< v0 8) v1) (logior (<< l0 8) l1))
    _ -> (raise (:TLS/Alert (tls-alert-desc:decode-error) "bad header"))
    ))

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
  (tlsext:maxfraglen code)             -> (sexp (sym 'maxfraglen) (int code))
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

;; --------------------------------------------------------------------------------

;; XXX rather than manually calling pack-xxx for each kind and layer,
;;   we should probably use the datatypes directly, so the only 'pack'
;;   function called is a high-level one, pack-record, that would accept
;;   a `tls-record` type rather than a rope/string.  this will simplify
;;   the code and make it safer.

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
    (tlsext:maxfraglen v)           -> (:tuple  1 (put-maxfraglen v))
    _                               -> (raise (:TLS/NotImplemented))
    )
  (let (((code ext0) (encode ext)))
    (rope/build (put-u16 code) (put-var-16 ext0))))

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

(define (put-maxfraglen fraglen)
  (put-u8
   (match fraglen with
      512 -> 1
     1024 -> 2
     2048 -> 3
     4096 -> 4
     _    -> (raise (:TLS/Alert (tls-alert-desc:illegal-parameter) "bad maxfraglen value"))
     )))
