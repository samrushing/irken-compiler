;; -*- Mode: Irken -*-

;; RFC 7541

(defmacro HF
  (HF name val)
  -> {name=name val=val}
  )

(define *static-table*
  (list->vector
   (LIST
    (HF "" "")
    (HF ":authority" "")
    (HF ":method" "GET")
    (HF ":method" "POST")
    (HF ":path" "/")
    (HF ":path" "/index.html")
    (HF ":scheme" "http")
    (HF ":scheme" "https")
    (HF ":status" "200")
    (HF ":status" "204")
    (HF ":status" "206")
    (HF ":status" "304")
    (HF ":status" "400")
    (HF ":status" "404")
    (HF ":status" "500")
    (HF "accept-charset" "")
    (HF "accept-encoding" "gzip deflate")
    (HF "accept-language" "")
    (HF "accept-ranges" "")
    (HF "accept" "")
    (HF "access-control-allow-origin" "")
    (HF "age" "")
    (HF "allow" "")
    (HF "authorization" "")
    (HF "cache-control" "")
    (HF "content-disposition" "")
    (HF "content-encoding" "")
    (HF "content-language" "")
    (HF "content-length" "")
    (HF "content-location" "")
    (HF "content-range" "")
    (HF "content-type" "")
    (HF "cookie" "")
    (HF "date" "")
    (HF "etag" "")
    (HF "expect" "")
    (HF "expires" "")
    (HF "from" "")
    (HF "host" "")
    (HF "if-match" "")
    (HF "if-modified-since" "")
    (HF "if-none-match" "")
    (HF "if-range" "")
    (HF "if-unmodified-since" "")
    (HF "last-modified" "")
    (HF "link" "")
    (HF "location" "")
    (HF "max-forwards" "")
    (HF "proxy-authenticate" "")
    (HF "proxy-authorization" "")
    (HF "range" "")
    (HF "referer" "")
    (HF "refresh" "")
    (HF "retry-after" "")
    (HF "server" "")
    (HF "set-cookie" "")
    (HF "strict-transport-security" "")
    (HF "transfer-encoding" "")
    (HF "user-agent" "")
    (HF "vary" "")
    (HF "via" "")
    (HF "www-authenticate" "")
    )))

(define nstatic (vector-length *static-table*))

(define static-map
  (let ((m (map-maker magic-cmp)))
    (for-range i nstatic
      (m::add *static-table*[i] i))
    m))

;; https://github.com/samrushing/hpack/blob/master/huffman.py
(define huffman-table-ascii
  (string-concat
   (LIST
    ".....3031.3261..6365.696f...7374..2025.2d2e...2f33.3435..3637.3839.....3d41.5f62"
    "..6466.6768...6c6d.6e70..7275..3a42.4344.....4546.4748..494a.4b4c...4d4e.4f50.."
    "5152.5354....5556.5759..6a6b.7176...7778.797a...262a.2c3b..585a...2122.2829..3f."
    "272b..7c.233e...0024.405b..5d7e..5e7d..3c60.7b....5cc3.d0.8082...83a2.b8c2..e0e2"
    "..99a1.a7ac.....b0b1.b3d1..d8d9.e3e5...e6.8184..8586.8892...9a9c.a0a3..a4a9.aaad"
    ".....b2b5.b9ba..bbbd.bec4...c6e4.e8e9...0187.898a..8b8c.8d8f.....9395.9697..989b"
    ".9d9e...a5a6.a8ae..afb4.b6b7....bcbf.c5e7..ef.098e..9091.949f....abce.d7e1..eced"
    "..c7cf.eaeb.....c0c1.c8c9..cacd.d2d5...dadb.eef0..f2f3.ff.cbcc.....d3d4.d6dd..de"
    "df.f1f4...f5f6.f7f8..fafb.fcfd....fe.0203..0405.0607...080b.0c0e..0f10.1112....1"
    "314.1517..1819.1a1b...1c1d.1e1f..7fdc.f9..0a0d.16Z")))

(datatype hufftree
  (:node hufftree hufftree)
  (:leaf int)
  )

;; this should go somewhere else
(define hexdig->int
  #\0 -> 0
  #\1 -> 1
  #\2 -> 2
  #\3 -> 3
  #\4 -> 4
  #\5 -> 5
  #\6 -> 6
  #\7 -> 7
  #\8 -> 8
  #\9 -> 9
  #\a -> 10
  #\b -> 11
  #\c -> 12
  #\d -> 13
  #\e -> 14
  #\f -> 15
  x   -> (raise (:BadHexDigit))
  )

(define (2hex->int s pos)
  (logior
   (<< (hexdig->int (string-ref s pos)) 4)
   (hexdig->int (string-ref s (+ pos 1)))))

(define (ascii->tree s pos)
  (let recur ()
    (match (string-ref s pos) with
      #\. -> (begin
               (set! pos (+ 1 pos))
               (hufftree:node (recur) (recur)))
      #\Z -> (hufftree:leaf 256)
      _   -> (let ((r (hufftree:leaf (2hex->int s pos))))
               (set! pos (+ pos 2))
               r)
      )))

(define huffman-table (ascii->tree huffman-table-ascii 0))

(define huffman-map
  (let ((m (make-vector 257 (:pair 0 0))))
    (let loop ((t huffman-table)
               (n 0)
               (bits 0))
      (match t with
        (hufftree:leaf val)
        -> (set! m[val] (:pair n bits))
        (hufftree:node L R)
        -> (begin
             (loop L (logior 0 (<< n 1)) (+ bits 1))
             (loop R (logior 1 (<< n 1)) (+ bits 1)))
        ))
    m))

(define masks
  (let ((v (make-vector 9 0)))
    (for-range i 8
      (set! v[(+ i 1)] (- (<< 1 (+ 1 i)) 1))
      )
    v))

(define (huffman-encode s)
  (let ((data '())
        (left 8)
        (byte 0))
    (define (emit-byte)
      (PUSH data (int->char byte))
      (set! left 8)
      (set! byte 0))
    (define (emit n bits)
      (while (> bits 0)
        (let ((slice (min left bits))
              (shift (- bits slice)))
          (set! byte (<< byte slice))
          (set! byte (logior byte (logand (>> n shift) masks[slice])))
          (dec! bits slice)
          (dec! left slice)
          (when (= left 0) (emit-byte))
          )))
    (define (encode s)
      (for-string ch s
        (match huffman-map[(char->int ch)] with
          (:pair n bits)
          -> (emit n bits))))
    (define (done)
      (if (< left 8)
          (emit #xffffffff left))
      (list->string (reverse data)))
    (encode s)
    (done)
    ))

(define (huffman-decode s pos nbytes)
  (let ((r '())
        (stop (+ nbytes pos))
        (bpos -1)
        (byte 0))
    (define (get-bit)
      (when (< bpos 0) (next-byte))
      (let ((set? (not (= 0 (logand byte (<< 1 bpos))))))
        (dec! bpos)
        set?
        ))
    (define (next-byte)
      (set! byte (char->int (string-ref s pos)))
      (inc! pos)
      (set! bpos 7)
      )
    (while (< pos stop)
      (let loop ((t huffman-table))
        (match t with
          (hufftree:leaf val) -> (PUSH r (int->char val))
          (hufftree:node L R) -> (loop (if (get-bit) R L))
          )))
    (list->string (reverse r))
    ))

(include "lib/basis.scm")
(include "lib/map.scm")

(let ((enc (huffman-encode "www.example.com")))
  (printf "encoded: " (string enc) "\n")
  (printf "decoded: " (string (huffman-decode enc 0 (string-length enc))) "\n")
  )
