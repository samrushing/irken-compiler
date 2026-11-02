;; -*- Mode: Irken -*-

;; a minimal HTTP/2 client (rfc9113): one request at a time, over a
;;  connection from tls13/client (or anything with send & recv-exact).
;;
;; push is disabled; flow control is handled by returning credit for
;;  each DATA frame as it arrives.

(require "doom/http/h2.scm")

(define (h2c/u32 n)
  (let ((s (make-string 4)))
    (for-range i 4
      (string-set! s i (int->char (logand #xff (>> n (* 8 (- 3 i)))))))
    s))

(define (h2c/get-u8 s pos) (char->int (string-ref s pos)))
(define (h2c/get-u16 s pos) (logior (<< (h2c/get-u8 s pos) 8) (h2c/get-u8 s (+ pos 1))))
(define (h2c/get-u32 s pos) (logior (<< (h2c/get-u16 s pos) 16) (h2c/get-u16 s (+ pos 2))))

(define (h2c/frame type flags stream-id payload)
  (let ((len (string-length payload)))
    (format (substring (h2c/u32 len) 1 4)
            (char->string (int->char type))
            (char->string (int->char flags))
            (h2c/u32 stream-id)
            payload)))

(define (h2c/setting id value)
  (format (substring (h2c/u32 id) 2 4) (h2c/u32 value)))

;; the payload of a HEADERS or DATA frame, without padding (and
;;  priority fields).
(define (h2c/strip-padding f priority?)
  (let ((pos 0)
        (padlen 0)
        (plen (string-length f.payload)))
    (when (flag-set? H2-FLAGS-PADDED f.flags)
      (set! padlen (h2c/get-u8 f.payload 0))
      (set! pos 1))
    (when (and priority? (flag-set? H2-FLAGS-PRIORITY f.flags))
      (inc! pos 5))
    (when (> (+ pos padlen) plen)
      (raise (:H2/BadFrame "padding")))
    (substring f.payload pos (- plen padlen))))

;; make a connection object.  the server's connection preface (its
;;  SETTINGS) is read along with the first response.
(define (h2/client conn)

  (let ((encoder (hpack/encoder 4096))
        (decoder (hpack/decoder 4096))
        (next-stream-id 1)
        (window (<< 1 24)))

    (define (send-frame type flags stream-id payload)
      (conn.send (h2c/frame (H2-FRAME->int type) flags stream-id payload)))

    (define (read-frame)
      (let ((h (conn.recv-exact 9))
            (len (logior (<< (h2c/get-u16 h 0) 8) (h2c/get-u8 h 2)))
            (payload (if (> len 0) (conn.recv-exact len) "")))
        {type=(h2c/get-u8 h 3)
         flags=(h2c/get-u8 h 4)
         stream-id=(logand #x7fffffff (h2c/get-u32 h 5))
         payload=payload}))

    ;; frames that aren't about any particular stream.
    (define (handle-connection-frame f)
      (match (alist/lookup H2-FRAME-rev-alist f.type) with
        (maybe:yes (H2-FRAME:SETTINGS))
        -> (when (not (flag-set? H2-FLAGS-SETTINGS-ACK f.flags))
             (send-frame (H2-FRAME:SETTINGS) H2-FLAGS-SETTINGS-ACK 0 ""))
        (maybe:yes (H2-FRAME:PING))
        -> (when (not (flag-set? H2-FLAGS-PING-ACK f.flags))
             (send-frame (H2-FRAME:PING) H2-FLAGS-PING-ACK 0 f.payload))
        (maybe:yes (H2-FRAME:GOAWAY))
        -> (raise (:H2/GoAway (h2c/get-u32 f.payload 4)
                              (substring f.payload 8 (string-length f.payload))))
        ;; WINDOW_UPDATE: we only send small requests.
        ;; PRIORITY, PUSH_PROMISE (disabled), unknown types: ignore.
        _ -> #u
        ))

    ;; send a request, and read the whole response.
    ;;  <headers> is a list of (:tuple name value), with lowercase names.
    ;;  <req-body> may be "".  (it must fit in the server's initial
    ;;  flow-control window: we don't wait for WINDOW_UPDATE.)
    (define (request method authority path headers req-body)
      (let ((sid next-stream-id)
            (block (list:nil))
            (status "")
            (rheaders (list:nil))
            (body (list:nil))
            (done #f))
        (define (end-headers)
          (let ((hs (decoder (string-concat (reverse block)))))
            (set! block (list:nil))
            (match (h2/header ":status" hs) with
              ;; 1xx responses are informational: wait for the real one.
              (maybe:yes s) -> (when (not (eq? (string-ref s 0) #\1))
                                 (set! status s)
                                 (set! rheaders hs))
              ;; trailers
              (maybe:no)    -> (set! rheaders (append rheaders hs)))))
        (define (end-stream? f)
          (when (flag-set? H2-FLAGS-END-STREAM f.flags)
            (set! done #t)))
        (inc! next-stream-id 2)
        (when (> (string-length req-body) 65535)
          (raise (:H2/BodyTooLarge (string-length req-body))))
        (send-frame (H2-FRAME:HEADERS)
                    (if (= 0 (string-length req-body))
                        (logior H2-FLAGS-END-HEADERS H2-FLAGS-END-STREAM)
                        H2-FLAGS-END-HEADERS)
                    sid
                    (encoder (append (list (:tuple ":method" method)
                                           (:tuple ":scheme" "https")
                                           (:tuple ":authority" authority)
                                           (:tuple ":path" path))
                                     headers)))
        ;; the body, in frames of at most 16KB (the minimum max-frame-size)
        (let loop ((pos 0))
          (when (< pos (string-length req-body))
            (let ((end (min (string-length req-body) (+ pos 16384))))
              (send-frame (H2-FRAME:DATA)
                          (if (= end (string-length req-body)) H2-FLAGS-END-STREAM 0)
                          sid (substring req-body pos end))
              (loop end))))
        (while (not done)
          (let ((f (read-frame)))
            (if (not (= f.stream-id sid))
                (handle-connection-frame f)
                (match (alist/lookup H2-FRAME-rev-alist f.type) with
                  (maybe:yes (H2-FRAME:HEADERS))
                  -> (begin
                       (push! block (h2c/strip-padding f #t))
                       (when (flag-set? H2-FLAGS-END-HEADERS f.flags) (end-headers))
                       (end-stream? f))
                  (maybe:yes (H2-FRAME:CONTINUATION))
                  -> (begin
                       (push! block f.payload)
                       (when (flag-set? H2-FLAGS-END-HEADERS f.flags) (end-headers)))
                  (maybe:yes (H2-FRAME:DATA))
                  -> (let ((len (string-length f.payload)))
                       (push! body (h2c/strip-padding f #f))
                       ;; give back the flow-control credit.
                       (when (> len 0)
                         (send-frame (H2-FRAME:WINDOW_UPDATE) 0 0 (h2c/u32 len))
                         (when (not (flag-set? H2-FLAGS-END-STREAM f.flags))
                           (send-frame (H2-FRAME:WINDOW_UPDATE) 0 sid (h2c/u32 len))))
                       (end-stream? f))
                  (maybe:yes (H2-FRAME:RST_STREAM))
                  -> (raise (:H2/StreamReset (h2c/get-u32 f.payload 0)))
                  _ -> #u
                  ))))
        {status=status headers=rheaders body=(string-concat (reverse body))}))

    ;; our preface: no push, a big window for the connection.
    (conn.send
     (format h2-preface
             (h2c/frame (H2-FRAME->int (H2-FRAME:SETTINGS)) 0 0
                        (format (h2c/setting (H2-SETTINGS->int (H2-SETTINGS:ENABLE_PUSH)) 0)
                                (h2c/setting (H2-SETTINGS->int (H2-SETTINGS:INITIAL_WINDOW_SIZE)) window)))
             (h2c/frame (H2-FRAME->int (H2-FRAME:WINDOW_UPDATE)) 0 0
                        (h2c/u32 (- window 65535)))))

    {request=request}
    ))

(define (h2/header name headers)
  (let loop ((hs headers))
    (match hs with
      () -> (maybe:no)
      ((:tuple n v) . rest) -> (if (string=? n name) (maybe:yes v) (loop rest))
      )))

(define (h2-error-name code)
  (match (alist/lookup H2-ERROR-rev-alist code) with
    (maybe:yes e) -> (symbol->string (H2-ERROR->name e))
    (maybe:no)    -> (int->string code)
    ))
