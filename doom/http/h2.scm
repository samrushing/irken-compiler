;; -*- Mode: Irken -*-

;; RFC7540

;; see [https://www.iana.org/assignments/http2-parameters/http2-parameters.xhtml]
;; for updates to these values.

(make-enum H2-ERROR
  (NO_ERROR            #x0)
  (PROTOCOL_ERROR      #x1)
  (INTERNAL_ERROR      #x2)
  (FLOW_CONTROL_ERROR  #x3)
  (SETTINGS_TIMEOUT    #x4)
  (STREAM_CLOSED       #x5)
  (FRAME_SIZE_ERROR    #x6)
  (REFUSED_STREAM      #x7)
  (CANCEL              #x8)
  (COMPRESSION_ERROR   #x9)
  (CONNECT_ERROR       #xa)
  (ENHANCE_YOUR_CALM   #xb)
  (INADEQUATE_SECURITY #xc)
  (HTTP_1_1_REQUIRED   #xd)
  )

(make-enum H2-FRAME
  (DATA          0)
  (HEADERS       1)
  (PRIORITY      2)
  (RST_STREAM    3)
  (SETTINGS      4)
  (PUSH_PROMISE  5)
  (PING          6)
  (GOAWAY        7)
  (WINDOW_UPDATE 8)
  (CONTINUATION  9)
  )

(make-enum H2-SETTINGS
  (HEADER_TABLE_SIZE      #x01)
  (ENABLE_PUSH            #x02)
  (MAX_CONCURRENT_STREAMS #x03)
  (INITIAL_WINDOW_SIZE    #x04)
  (MAX_FRAME_SIZE         #x05)
  (MAX_HEADER_LIST_SIZE   #x06) ;; advisory
  ;;(TLS_RENEG_PERMITTED    #x10)
  )

;; note: enum not appropriate for these.
(define H2-FLAGS-END-STREAM   #x01)
(define H2-FLAGS-END-HEADERS  #x04)
(define H2-FLAGS-PADDED       #x08)
(define H2-FLAGS-PRIORITY     #x20)
(define H2-FLAGS-PING-ACK     #x01)
(define H2-FLAGS-SETTINGS-ACK #x01)

(define (flag-set? flag val)
  (> (logand flag val) 0))

(define h2-preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(define (make-h2-conn conn handler) ;; conn := doom-socket.

  (define (ubyte s pos)
    (char->int (string-ref s pos)))

  (define (uu16 s pos)
    (logior*
     (<< (ubyte s (+ pos 0)) 8)
     (<< (ubyte s (+ pos 1)) 0)
     ))

  (define (ui16 s pos)
    (let ((n (uu16 s pos)))
      (if (> n #x7fff) (- n #x10000) n)
      ))

  (define (uu32 s pos)
    (logior*
     (<< (ubyte s (+ pos 0)) 24)
     (<< (ubyte s (+ pos 1)) 16)
     (<< (ubyte s (+ pos 2))  8)
     (<< (ubyte s (+ pos 3))  0)
     ))

  (define (ui32 s pos)
    (let ((n (uu32 s pos)))
      (if (> n #x7fffffff) (- n #x100000000) n)))

  (define (ustreamid s pos)
    (let ((n (uu32 s pos)))
      (:tuple (> (logand #x100000000 n) 0)
              (logand #x7fffffff n))))

  (define (pbyte s pos b)
    (string-set! s pos (int->char (logand #xff b))))

  (define (pu32 s pos n)
    (pbyte s (+ pos 0) (>> n 24))
    (pbyte s (+ pos 1) (>> n 16))
    (pbyte s (+ pos 2) (>> n  8))
    (pbyte s (+ pos 3) (>> n  0)))

  (define (pi32 s pos n)
    (pu32 s pos (if (< 0 n) (+ #x100000000 n) n)))

  (define (make-settings)
    {header-table-size      = 4096
     enable-push            = #t
     max-concurrent-streams = 100
     initial-window-size    = 65535
     max-frame-size         = 16384 ;; MUST be 16384 <= size <= 16777215
     max-header-list-size   = 102400 ;; rfc says it should be 'unlimited'
     })

  (let ((streams (tree/empty)) ;; int -> stream map.
        (priorities (tree/empty))
        (encoder (hpack/encoder))
        (decoder (hpack/decoder))
        (settings (make-settings))
        (done #f))

    (define (make-stream)
      {window-size=settings.initial-window-size}
      )

    (define (update-setting! ident value)
      ;; XXX sanity check values, etc.
      (match (int->H2-SETTINGS ident) with
        ;; XXX this won't work - rfc requires unrecognized values to be ignored.
        (H2-SETTINGS:HEADER_TABLE_SIZE)      -> (set! settings.header-table-size value)
        (H2-SETTINGS:ENABLE_PUSH)            -> (set! settings.enable-push (> value 0))
        (H2-SETTINGS:MAX_CONCURRENT_STREAMS) -> (set! settings.max-concurrent-streams value)
        (H2-SETTINGS:INITIAL_WINDOW_SIZE)    -> (set! settings.initial-window-size value)
        (H2-SETTINGS:MAX_FRAME_SIZE)         -> (set! settings.max-frame-size value)
        (H2-SETTINGS:MAX_HEADER_LIST_SIZE)   -> (set! settings.max-header-list-size value)
        )
      ;;(printf " set " (sym (H2-SETTINGS->name (int->H2-SETTINGS ident))) " = " (int value) "\n")
      )

    (define (unpack-frame-header h)
      (let ((lentype (uu32 h 0))
            (flags (ubyte h 4))
            ((_ stream-id) (ustreamid h 5)))
        (:tuple (>> lentype 8)
                (int->H2-FRAME (logand lentype #xff))
                flags
                stream-id)))

    (define (pack-frame-header type flags stream-id plen)
      (let ((h (make-string 9))
            (lentype (logior (<< plen 8) (H2-FRAME->int type))))
        (pu32 h 0 lentype)
        (pbyte h 4 flags)
        (pi32 h 5 stream-id)
        h))

    (define (read-frames)
      (let ((preface (conn.recv-exact (string-length h2-preface))))
        (when (not (string=? preface h2-preface))
          (raise (:H2/BadPreface preface)))
        (send-settings)
        (while (not done)
          (let (((flen ftype flags stream-id) (unpack-frame-header (conn.recv-exact 9)))
                (payload (if (> flen 0) (conn.recv-exact flen) ""))
                (frame {type=ftype flags=flags stream-id=stream-id payload=payload}))
            ;; (print-frame frame)
            (dispatch frame)
            ))
        ))

    (define (print-frame f)
      (printf (ansi (bold blue) "<-- " (rpad 13 (sym (H2-FRAME->name f.type))))
              " stream=" (lpad 3 (int f.stream-id))
              " flags=" (zpad 4 (hex f.flags))
              " len=" (int (string-length f.payload))
              "\n"))

    ;; XXX implement a FIFO for outgoing frames.
    (define (send-frame type flags stream-id payload)
      ;; (printf (ansi (bold red) "--> " (rpad 13 (sym (H2-FRAME->name type))))
      ;;         " stream=" (lpad 3 (int stream-id))
      ;;         " flags=" (zpad 4 (int flags))
      ;;         " len=" (int (string-length payload))
      ;;         "\n")
      (conn.send (pack-frame-header type flags stream-id (string-length payload)))
      (conn.send payload)
      )

    (define (send-settings)
      ;; XXX for now, leave everything at its default.
      (send-frame (H2-FRAME:SETTINGS) 0 0 "")
      )

    (define (handle-ping f)
      (if (= 8 (string-length f.payload))
          ;; XXX requirements on stream-id?
          (send-frame (H2-FRAME:PING) H2-FLAGS-PING-ACK f.stream-id f.payload)
          ;; XXX report an error via h2 or simply close the connection?
          (set! done #t)))

    (define (handle-settings f)
      ;; XXX need state variable for 'receipt of settings ack'.
      (when (not (flag-set? H2-FLAGS-SETTINGS-ACK f.flags))
        (let ((plen (string-length f.payload))
              ((n r) (divmod plen 6))
              (pos 0))
          (when (not (= 0 r)) (raise (:H2/BadFrame f)))
          (for-range i n
            (let ((ident (uu16 f.payload pos))
                  (value (uu32 f.payload (+ pos 2))))
              (update-setting! ident value)
              (inc! pos 6)
              ))
          (send-frame (H2-FRAME:SETTINGS) H2-FLAGS-SETTINGS-ACK 0 "")
          )))

    (define (handle-window-update f)
      ;; XXX ignored for now, other than sanity checks.
      (when (not (= 4 (string-length f.payload)))
        (raise (:H2/BadFrame f)))
      (let ((increment (ui32 f.payload 0)))
        (when (< increment 0)
          (raise (:H2/BadFrame f)))
        ;;(printf "window-update stream=" (int f.stream-id) " increment " (int increment) "\n")
        ))

    (define (print-headers h)
      (define print-pair
        (:tuple name val)
        -> (printf "  " name ": " val "\n")
        )
      (printf "headers {\n")
      (for-each print-pair h)
      (printf "}\n"))

    (define (make-request stream-id iheaders)

      (define (send-headers oheaders has-data?)
        (let ((flags (logior H2-FLAGS-END-HEADERS (if has-data? 0 H2-FLAGS-END-STREAM))))
          (send-frame (H2-FRAME:HEADERS) flags stream-id (encoder oheaders))))

      (define (send-data data last?)
        (send-frame (H2-FRAME:DATA) (if last? H2-FLAGS-END-STREAM 0) stream-id data))

      (define (get-conn)
        conn)

      {send-headers = send-headers
       send-data    = send-data
       ;;recv-data    = recv-data
       stream-id    = stream-id
       headers      = iheaders
       ;; hack to get at TLS data
       get-conn     = get-conn
       })

    (define (handle-headers f)
      (let ((pos 0)
            (pad-len 0)
            (stream-dep 0)
            (exclusive? #f)
            (weight 0))
        (when (< f.stream-id 1) (raise (:H2/BadFrame f)))
        (when (flag-set? H2-FLAGS-PADDED f.flags)
          (set! pad-len (ubyte f.payload pos))
          (inc! pos))
        (when (flag-set? H2-FLAGS-PRIORITY f.flags)
          ;; note: this is identical to the PRIORITY layout, should
          ;;   probably pull this into its own function.
          (match (ustreamid f.payload pos) with
            (:tuple e? sid) -> (begin (set! exclusive? e?)
                                      (set! stream-dep sid)))
          (set! weight (ubyte f.payload (+ pos 4)))
          (inc! pos 5))
        ;; END_STREAM, END_HEADERS
        ;; XXX teach unpack to take a pos param.
        (let ((headers (decoder (substring f.payload pos (- (string-length f.payload) pad-len)))))
          ;;(print-headers headers)
          ;; create & spawn a request object
          (poller/fork (lambda () (handler (make-request f.stream-id headers))))
          )))

    (define (handle-rst-stream f)
      (let ((code (uu32 f.payload 0)))
        ;;(printf "    RST_STREAM " (int code) " " (sym (H2-ERROR->name (int->H2-ERROR code))) "\n")
        (when (= f.stream-id 0) (set! done #t))
        ))

    (define (handle-goaway f)
      ;; XXX assert f.stream-id is 0
      (let (((_ last-stream-id) (ustreamid f.payload 0))
            (error-code (uu32 f.payload 4))
            (debug (substring f.payload 8 (string-length f.payload))))
        ;;(printf "    GOAWAY last-stream-id " (int last-stream-id) " code " (int error-code) " debug '" debug "'\n")
        (set! done #t)))

    (define (handle-priority f)
      (let (((exclusive? stream-dep) (ustreamid f.payload 0))
            (weight (ubyte f.payload 4)))
        ;; (printf "    PRIORITY " (int weight)
        ;;         " stream-dep " (int stream-dep)
        ;;         " exclusive? " (bool exclusive?) "\n")
        #u
        ))

    (define (handle-data f)
      (let ((padlen (ubyte f.payload 0)))
        ;; XXX data + padding
        ;; XXX need a way to feed this into a request.
        ;;(printf "    DATA unhandled " (int (- (string-length f.payload) padlen 1)) " bytes.\n")
        #u
        ))

    (define (handle-push-promise f)
      (let ((padlen (ubyte f.payload 0))
            (stream-id (ui32 f.payload 1))
            (fragment (substring f.payload 5 (- (string-length f.payload) padlen))))
        ;; (printf "    ignoring PUSH_PROMISE on stream " (int stream-id)
        ;;         " of " (int (- (string-length f.payload) padlen 5)) "\n")
        #u
        ))

    (define (handle-continuation f)
      ;;(printf "    ignoring CONTINUATION of " (int (string-length f.payload)) " bytes.\n")
      #u
      )

    (define (dispatch frame)
      (match frame.type with
        (H2-FRAME:DATA)          -> (handle-data frame)
        (H2-FRAME:HEADERS)       -> (handle-headers frame)
        (H2-FRAME:PRIORITY)      -> (handle-priority frame)
        (H2-FRAME:RST_STREAM)    -> (handle-rst-stream frame)
        (H2-FRAME:SETTINGS)      -> (handle-settings frame)
        (H2-FRAME:PUSH_PROMISE)  -> (handle-push-promise frame)
        (H2-FRAME:PING)          -> (handle-ping frame)
        (H2-FRAME:GOAWAY)        -> (handle-goaway frame)
        (H2-FRAME:WINDOW_UPDATE) -> (handle-window-update frame)
        (H2-FRAME:CONTINUATION)  -> (handle-continuation frame)
        ))

    read-frames

    ))
