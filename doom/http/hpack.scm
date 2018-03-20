;; -*- Mode: Irken -*-

;; RFC 7541

;; XXX need to support SETTINGS_MAX_HEADER_LIST_SIZE
;;     in order to defend against an 'hpack bomb'.

(define (H a b)
  (:tuple a b))

(define *static-table*
  #((H "" "")
    (H ":authority" "")
    (H ":method" "GET")
    (H ":method" "POST")
    (H ":path" "/")
    (H ":path" "/index.html")
    (H ":scheme" "http")
    (H ":scheme" "https")
    (H ":status" "200")
    (H ":status" "204")
    (H ":status" "206")
    (H ":status" "304")
    (H ":status" "400")
    (H ":status" "404")
    (H ":status" "500")
    (H "accept-charset" "")
    (H "accept-encoding" "gzip, deflate")
    (H "accept-language" "")
    (H "accept-ranges" "")
    (H "accept" "")
    (H "access-control-allow-origin" "")
    (H "age" "")
    (H "allow" "")
    (H "authorization" "")
    (H "cache-control" "")
    (H "content-disposition" "")
    (H "content-encoding" "")
    (H "content-language" "")
    (H "content-length" "")
    (H "content-location" "")
    (H "content-range" "")
    (H "content-type" "")
    (H "cookie" "")
    (H "date" "")
    (H "etag" "")
    (H "expect" "")
    (H "expires" "")
    (H "from" "")
    (H "host" "")
    (H "if-match" "")
    (H "if-modified-since" "")
    (H "if-none-match" "")
    (H "if-range" "")
    (H "if-unmodified-since" "")
    (H "last-modified" "")
    (H "link" "")
    (H "location" "")
    (H "max-forwards" "")
    (H "proxy-authenticate" "")
    (H "proxy-authorization" "")
    (H "range" "")
    (H "referer" "")
    (H "refresh" "")
    (H "retry-after" "")
    (H "server" "")
    (H "set-cookie" "")
    (H "strict-transport-security" "")
    (H "transfer-encoding" "")
    (H "user-agent" "")
    (H "vary" "")
    (H "via" "")
    (H "www-authenticate" "")
    ))

(define nstatic 62)
(assert (= nstatic (vector-length *static-table*)))

;; a map from header-pair to index in the static table.
(define *static-map*
  (let ((m (tree/empty)))
    (for-range i nstatic
      (tree/insert! m magic-cmp *static-table*[i] i))
    m))

;; XXX investigate: this is supposed to be a 'canonical' huffman
;;  encoding, which is supposed to have a very compact representation,
;;  where each entry consists of 'key', 'bits'.

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
        (bits (* nbytes 8))
        (bpos -1)
        (byte 0))
    (define (get-bit)
      (when (< bpos 0) (next-byte))
      (let ((set? (= 1 (logand 1 (>> byte bpos)))))
        (dec! bpos)
        (dec! bits)
        set?
        ))
    (define (next-byte)
      (set! byte (char->int (string-ref s pos)))
      (inc! pos)
      (set! bpos 7)
      )
    ;; note: after the last encoded character, there may be remaining
    ;; bits, we have no way of knowing ahead of time if they encode a
    ;; character, so we have to peel them off (i.e., we descend
    ;; partway down the tree toward `EOS`)
    (while (>= bits 5) ;; shortest code is 5 bits.
      (let loop ((t huffman-table))
        (match t with
          (hufftree:leaf val) -> (PUSH r (int->char val))
          (hufftree:node L R) -> (if (> bits 0) (loop (if (get-bit) R L)))
          )))
    (list->string (reverse r))
    ))

;; api for dynamic table:
;; get
;; set
;; get-index
;; count of one name
;; set-size

;; OK, how about this: we use two maps, just like cmap.
;; fwd, rev. Then we have a 'clock'.  when you put something
;; in, you store its clock.  To reference it, you pull it out
;; by clock - index.
;; to insert an entry,
;; we store it in fwd as (key,val) -> clock
;;                rev as clock -> (key, val)
;;

(define (make-dynamic-table max-size)
  (let ((fwd (tree/empty))
        (rev (tree/empty))
        (clock 0)
        (size 0))

    (define (entry-size name val)
      (+ 32 (string-length name) (string-length val)))

    (define (evict-one)
      (let (((index key) (tree/min rev))
            ((name val) key)
            (esize (entry-size name val)))
        ;;(printf "evicting " (int esize) " bytes.\n")
        (tree/delete! fwd magic-cmp key)
        (tree/delete! rev int-cmp index)
        (dec! size esize)
        ))

    (define (dump)
      (printf "table " (int clock) " {\n")
      (for-map index pair rev
        (let (((name val) pair))
          (printf (int index) "/" (int (- clock index 1)) " " name ": " val "\n")))
      (printf "}\n")
      )

    (define (set name val)
      (let ((key (:tuple name val))
            (esize (entry-size name val)))
        (while (> (+ size esize) max-size)
          (evict-one))
        (tree/insert! fwd magic-cmp key clock)
        (tree/insert! rev int-cmp clock key)
        (inc! clock)
        (inc! size esize)
        ;;(dump)
        ))

    (define (get index)
      ;;(printf "get " (int index) " nstatic " (int nstatic) "\n")
      (if (< index nstatic)
          *static-table*[index]
          (let ((index0 (- index nstatic))
                (index1 (- clock index0 1)))
            ;;(printf "clock " (int clock) " index0 " (int index0) " index1 " (int index1) "\n")
            (match (tree/member rev int-cmp index1) with
              (maybe:yes pair) -> pair
              (maybe:no) -> (raise (:Hpack/BadIndex index))
              ))))

    (define (set-size new-max-size)
      ;;(printf "set-size " (int new-max-size) "\n")
      (set! max-size new-max-size)
      (while (> size max-size)
        (evict-one)))

    ;; -------------------------------------------
    ;; everything below is to support `get-index`
    ;; -------------------------------------------

    (define (lookup-static name val)
      (tree/member *static-map* magic-cmp (:tuple name val)))

    (define (lookup-dynamic name val)
      (tree/member fwd magic-cmp (:tuple name val)))

    ;; return a list of entries with `name`.
    (define (name-range map name)
      (let ((range '()))
        (tree/range
         map magic-cmp
         (:tuple name "") (:tuple name "\xff")
         (lambda (k v) (PUSH range (:tuple k v))))
        (reverse range)))

    (define (lookup-name-only map name)
      (match (name-range map name) with
        ;; XXX possible optimization: choose the most recent index,
        ;;     which will remain in the table longer.
        ()                       -> (maybe:no)
        ((:tuple key index) . _) -> (maybe:yes index)
        ))


    ;; how many entries do we have under this name?
    (define (nvals name)
      (length (name-range fwd name)))

    (define (get-static-index name val)
      (match (lookup-static name val) with
        (maybe:yes index) -> (:tuple (maybe:yes index) #t)
        (maybe:no)
        -> (:tuple (lookup-name-only *static-map* name) #f)
        ))

    (define (get-dynamic-index name val)
      (match (lookup-dynamic name val) with
        (maybe:yes index) -> (:tuple (maybe:yes index) #t)
        (maybe:no)
        -> (:tuple (lookup-name-only fwd name) #f)
        ))

    ;; adjust dynamic index to reflect
    ;; 1) offset after static table
    ;; 2) our clock-based indexing scheme.
    (define adjust
      (maybe:yes index) -> (maybe:yes (+ nstatic (- clock index 1)))
      no                 -> no
      )

    (define (get-index name val)
      (match (get-static-index name val) with
        ;; both present static
        (:tuple probe0 #t) -> (:tuple probe0 #t)
        (:tuple probe0 #f)
        -> (let (((probe1 both1) (get-dynamic-index name val)))
             ;;(printf "get-index both1=" (bool both1) " ")
             ;;(printn (LIST probe0 probe1))
             (cond (both1
                    ;; both present in dynamic
                    (:tuple (adjust probe1) #t))
                   ((maybe? probe0)
                    ;; name only, prefer static
                    (:tuple probe0 #f))
                   ((maybe? probe1)
                    ;; name only in dynamic
                    (:tuple (adjust probe1) #f))
                   (else
                    ;; nothing in either table
                    (:tuple (maybe:no) #f)))
             )))

    {get=get set=set set-size=set-size get-index=get-index nvals=nvals}
    ))

;; one function, 'unpack'. so we probably don't need a record.

(define (hpack/decoder)
  (let ((data "")
        (pos -1)
        (byte 0)
        (stop 0)
        (table (make-dynamic-table 4096)))

    (define (forward n)
      (inc! pos n)
      (if (< pos stop)
          (set! byte (char->int (string-ref data pos)))))

    (define (next-byte)
      (forward 1))

    (define (get-integer nbits)
      (let ((mask masks[nbits])
            (r (logand mask byte)))
        (cond ((= r mask) ;; more octets
               (set! r 0)
               ;; note: little-endian encoding!
               (let loop ((m 0)) ;; XXX length check
                 (next-byte)
                 (set! r (logior r (<< (logand #x7f byte) m)))
                 (if (not (= 0 (logand #x80 byte)))
                     (loop (+ m 7))))
               (next-byte)
               (+ r mask))
              (else
               (next-byte)
               r))))

    (define (get-pair0 index)
      ;;(printf "get-pair0 " (int index) "\n")
      (let ((name (if (= index 0)
                      (get-literal)
                      (match (table.get index) with
                        (:tuple name1 _) -> name1))))
        (:tuple name (get-literal))
        ))

    (define (get-literal)
      (let ((huffman? (= (logand #x80 byte) #x80))
            (len (get-integer 7))
            (r (if huffman?
                   (huffman-decode data pos len)
                   (substring data pos (+ pos len)))))
        (forward len)
        ;;(printf " --> " (string r) "\n")
        r))

    (define (get-header)
      ;;(printf "get-header byte = " (hex byte) "\n")
      (cond ((= 1 (>> byte 7))
             ;; index name and value
             (let ((index (get-integer 7)))
               ;;(printf "7 " (int index) "\n")
               (table.get index)))
            ((= 1 (>> byte 6))
             ;; literal with incremental indexing
             (let ((index (get-integer 6))
                   (key (get-pair0 index))
                   ((name val) key))
               ;;(printf "6 " (int index) " name " name " val " (string val) "\n")
               (table.set name val)
               key))
            ((< (>> byte 4) 2)
             ;; literal without indexing
             (let ((never (= 1 (>> byte 4)))
                   (index (get-integer 4)))
               ;;(printf "4 " (int index) "\n")
               (get-pair0 index)))
            (else
             (raise (:Hpack/BadHeaderCode byte)))))

    (define (unpack s)
      (let ((r '()))
        (set! data s)
        (set! pos -1)
        (set! stop (string-length data))
        (next-byte)
        (while (< pos stop)
          (cond ((= 1 (>> byte 5))
                 (table.set-size (get-integer 5)))
                (else
                 (let ((r0 (get-header)))
                   (PUSH r r0)))))
        (reverse r)))

    unpack

    ))

;; does h2 allow specifying a standard size before-hand?
(define (hpack/encoder)
  (let ((table (make-dynamic-table 4096))
        (max-vals 5)
        (data '()) ;; XXX use a buffer object
        (never (set/empty)))

    (define (emit byte)
      (PUSH data (int->char byte)))

    (define (emit-string s)
      (for-string ch s
        (PUSH data ch)))

    (define (emit-integer n0 bits n1)
      (let ((mask masks[bits]))
        (cond ((< n1 mask)
               (emit (logior n1 (<< n0 bits))))
              (else
               (emit (logior mask (<< n0 bits)))
               (dec! n1 mask)
               ;; encode the remaining bits 7 at a time...
               (while (>= n1 #x80)
                 (emit (logior (logand n1 #x7f) #x80))
                 (set! n1 (>> n1 7)))
               ;; and any leftover (or 0).
               (emit n1)))))

    (define (emit-literal s0)
      (let ((len0 (string-length s0))
            (s1 (huffman-encode s0))
            (len1 (string-length s1)))
        (cond ((>= len1 len0)
               ;; oops, not huffman-friendly
               (emit-integer 0 7 len0)
               (emit-string s0))
              (else
               (emit-integer 1 7 len1)
               (emit-string s1)))))

    (define (emit-header name val)
      ;;(printf "emit-header " name " " val "\n")
      (cond ((set/member never string-compare name)
             ;; literal name & value: never index
             ;;(printf "*** never " name " " val "\n")
             (emit-integer 1 4 0)
             (emit-literal name)
             (emit-literal val))
            (else
             (match (table.get-index name val) with
               (:tuple (maybe:yes index) #t)
               -> (begin
                    ;;(printf "*** both index " (int index) "\n")
                    (emit-integer 1 7 index)
                    )
               (:tuple (maybe:yes index) #f)
               -> (cond ((>= (table.nvals name) max-vals)
                         ;; a header like `date`, which varies.
                         ;;(printf "*** !index " (int index) " " (string val) "\n")
                         (emit-integer 0 4 index)
                         (emit-literal val))
                        (else
                         ;; index name, literal value.
                         ;;(printf "*** index " (int index) " " (string val) "\n")
                         (table.set name val)
                         (emit-integer 1 6 index)
                         (emit-literal val)))
               (:tuple (maybe:no) _)
               -> (begin
                    ;; index literal name & value
                    ;;(printf "*** new   " name " " val "\n")
                    (table.set name val)
                    (emit-integer 1 6 0)
                    (emit-literal name)
                    (emit-literal val))
               ))))

    (define (pack headers)
      ;; rfc7540 8.1.2.1 Pseudo-Header Fields
      ;;   requires that pseudo-headers precede normal headers.
      (let ((pseudo '())
            (normal '()))
        (for-list header headers
          (match header with
            (:tuple name val)
            -> (if (eq? (string-ref name 0) #\:)
                   (PUSH pseudo header)
                   (PUSH normal header))))
        (for-list header (append (reverse pseudo) (reverse normal))
          (match header with
            (:tuple name val)
            -> (emit-header name val)))
        (let ((r (list->string (reverse data))))
          (set! data '())
          r)))

    pack
    ))

