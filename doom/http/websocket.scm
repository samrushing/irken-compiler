;; -*- Mode: Irken -*-

;; RFC 6455

;; XXX provide some way of checking the `sec-websocket-protocol` header.

(define (websocket sock)

  (define exit? #f)

    (define magic "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

    (define (header-generator)
      (makegen emit
        (let loop0 ((data (sock.recv)))
          (let ((lines (string-split-string data "\r\n")))
            (let loop1 ((lines lines))
              (match lines with
                ()        -> (raise (:Websocket/NoHeader))
                ("" "")   -> #u ;; \r\n\r\n (done)
                (x "")    -> (begin (emit x) (loop0 (sock.recv))) ;; xxx\r\n
                (x)       -> (loop0 (string-append x (sock.recv))) ;; xxx
                (hd . tl) -> (begin (emit hd) (loop1 tl))          ;; xxx\r\nyyy...
                ))))))

    (define (add-header-value hmap key val)
      (match (tree/member hmap string-compare key) with
        (maybe:yes vals)
        -> (tree/insert (tree/delete hmap string-compare key) string-compare key (list:cons val vals))
        (maybe:no)
        -> (tree/insert hmap string-compare key (list:cons val (list:nil)))
        ))

    (define (parse-header linegen)
      (let ((hmap (tree/empty))
            (uri ""))
        (match (linegen) with
          (maybe:yes req)
          -> (match (string-split req #\space) with
               ("GET" uri0 "HTTP/1.1") -> (set! uri uri0)
               _ -> (raise (:Websocket/BadRequest req)))
          (maybe:no)
          -> (raise (:Websocket/NoRequest)))
        (printf "uri = " (string uri) "\n")
        (for line linegen
          (let ((parts (string-split-string line ": ")))
            (match parts with
              (key val) -> (set! hmap (add-header-value hmap (downcase key) val))
              _         -> (raise (:Websocket/BadHeader line))
              )))
        (:tuple uri hmap)))

    (define (get-uint n)
      (let ((s (sock.recv-exact n))
            (r 0))
        (for-range i n
          (set! r (logior (<< r 8) (char->int (string-ref s i)))))
        r))

    (define (pack-uint c n)
      (let ((r (list:nil)))
        (for-range i c
          (push! r (int->char (logand #xff n)))
          (set! n (>> n 8)))
        (list->string r)))

    (define (apply-mask! mask data)
      (for-range i (string-length data)
        (let ((byte0 (char->int (string-ref data i)))
              (shift (- 24 (* 8 (mod i 4))))
              (mbyte (logand #xff (>> mask shift)))
              (byte1 (logxor mbyte byte0)))
          (string-set! data i (int->char byte1)))))

    (define maybe-unmask!
      (maybe:yes mask) data -> (apply-mask! mask data)
      (maybe:no)       data -> #u
      )

    (define (packet-generator)
      (makegen emit
        (while (not exit?)
          (let ((h (get-uint 2))
                (fin    (>> (logand h #x8000) 15))
                (opcode (>> (logand h #x0f00) 8))
                (maskp  (>> (logand h #x0080) 7))
                (plen   (>> (logand h #x007f) 0))
                (masking (maybe:no)))
            (match plen with
              126 -> (set! plen (get-uint 2))
              127 -> (set! plen (get-uint 8))
              _   -> #u)
            (when (> plen (<< 1 20))
              (raise (:Websocket/TooMuchData plen)))
            (when (not (zero? maskp))
              (set! masking (maybe:yes (get-uint 4))))
            (let ((payload (sock.recv-exact plen)))
              (maybe-unmask! masking payload)
              (match opcode with
                8 -> (set! exit? #t)
                9 -> (send-pong payload)
                x -> (emit (:tuple opcode payload (not (zero? fin))))
                ))))))

    (define (send-packet opcode data fin?)
      (let ((head (logior (<< opcode 8) (if fin? #x8000 #x0000)))
            (dlen (string-length data))
            (pkt (cond ((< dlen 126)
                        (list (pack-uint 2 (logior head dlen)) data))
                       ((< dlen (<< 1 16))
                        (list (pack-uint 2 (logior head 126)) (pack-uint 2 dlen) data))
                       ((< dlen (<< 1 32))
                        (list (pack-uint 2 (logior head 127)) (pack-uint 8 dlen) data))
                       (else
                        (raise (:Websocket/TooMuchData dlen))))))
        ;; XXX rope interface someday.
        (sock.send (string-concat pkt))))

    (define (send-pong data)
      (send-packet 10 data #t))

    (define (send-text data fin?)
      (send-packet 1 data fin?))

    (define (send-binary data fin?)
      (send-packet 2 data fin?))

    (define (close)
      (send-packet 8 "" #t)
      (sock.close))

    (let (((uri hmap) (parse-header (header-generator)))
          (key (tree/get hmap string-compare "sec-websocket-key"))
          (khash (b64-encode (sha1 (list-generator (list (first key) magic)))))
          (reply (format
                  "HTTP/1.1 101 Switching Protocols\r\n"
                  "Upgrade: websocket\r\n"
                  "Connection: Upgrade\r\n"
                  "Sec-Websocket-Accept: " khash "\r\n"
                  "\r\n")))
      (sock.send reply))

    {pktgen=(packet-generator)
     send-text=send-text
     send-binary=send-binary
     send-packet=send-packet
     close=close}
    )
