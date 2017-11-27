;; -*- Mode: Irken -*-

(include "lib/enum.scm")

;; This is a start on an ASN.1 BER encoder.

;; DER note: no attempt is made (yet) to place encoded values in
;;   canonical order for SET. See section 5.15 of the "Layman's
;;   Guide..." for details.
;; DER note: 'BINTEGER' (ints larger than 3 bytes) does not properly
;;   check for canonical encoding yet.  User's responsibility for now.
;;
;; Safety Note: oversized OID elements raise an error.

;; many kinds of strings
(make-enum asn1string
 (OCTET            4)  ;; byte string
 (BIT              3)  ;; bit string
 (UTF8             8)  ;; utf-8 string
 (NUMERIC          18) ;; Numeric string
 (PRINTABLE        19) ;; Printable string (ASCII subset)
 (T61              20) ;; T61/Teletex string
 (VIDEOTEX         21) ;; Videotex string
 (IA5              22) ;; IA5/ASCII string
 (UTCTIME          23) ;; UTC time
 (GENERALIZEDTIME  24) ;; Generalized time
 (GRAPHIC          25) ;; Graphic string
 (VISIBLE          26) ;; Visible string (ASCII subset)
 (GENERAL          27) ;; General string
 (UNIVERSAL        28) ;; Universal string
 (BMP              30) ;; Basic Multilingual Plane/Unicode string
 )

(datatype ber
  ;; -- primitive --
  (:NULL)
  (:BOOLEAN bool)
  (:INTEGER int)
  (:BINTEGER string)
  (:ENUMERATED int)
  (:STRING asn1string string)
  (:OID (list int))
  ;; -- constructed --
  (:SEQUENCE (list ber))
  (:SET (list ber))
  (:CONTEXT int (list ber))
  (:APPLICATION int (list ber))
  (:UNKNOWN int int string) ;; tag flags value
  )

(define asn1-string-repr
  (asn1string:BIT) s -> (format "(BITSTRING \"" (string->hex s) "\")")
  other            s -> (format "(" (sym (asn1string->name other)) "STRING " (string s) ")")
  )

(define ber-repr
  (ber:NULL)                 -> "NULL"
  (ber:BOOLEAN b)            -> (if b "TRUE" "FALSE")
  (ber:INTEGER n)            -> (int->string n)
  (ber:BINTEGER s)           -> (format "(INTEGER " (string->hex s) ")")
  (ber:STRING kind s)        -> (asn1-string-repr kind s)
  (ber:ENUMERATED n)         -> (format "(ENUMERATED " (int n) ")")
  (ber:OID vals)             -> (format "(OID " (join int->string "." vals) ")")
  (ber:SEQUENCE vals)        -> (format "(SEQUENCE " (join ber-repr " " vals) ")")
  (ber:SET vals)             -> (format "(SET " (join ber-repr " " vals) ")")
  (ber:CONTEXT tag vals)     -> (format "(CONTEXT " (int tag) " " (join ber-repr " " vals) ")")
  (ber:APPLICATION tag vals) -> (format "(APPLICATION " (int tag) " " (join ber-repr " " vals) ")")
  (ber:UNKNOWN t f s)        -> (format "(UNKNOWN " (int t) " " (int f) " " (string s) ")")
  )

(define (pp-ber ob d)

  (define (p-vals prefix vals)
    (printf prefix)
    (for-list val vals (pp-ber val (+ d 1)))
    (printf ")"))

  (printf "\n" (repeat d "  "))
  (match ob with
    (ber:SEQUENCE vals)        -> (p-vals "(SEQUENCE " vals)
    (ber:SET vals)             -> (p-vals "(SET " vals)
    (ber:APPLICATION tag vals) -> (p-vals (format "(APPLICATION " (int tag) " ") vals)
    (ber:CONTEXT tag vals)     -> (p-vals (format "(CONTEXT " (int tag) " ") vals)
    _ -> (printf (ber-repr ob))
    ))

;; flags are:
;;  class      bit8 bit7
;; universal     0   0
;; application   0   1
;; context       1   0
;; private       1   1
;;
;; bit6 indicates 'structured'

;; --------------------------------------------------------------------------------
;; encoder
;; --------------------------------------------------------------------------------

;; ideally all these helper funs should go into a scope.

(define base256
  0  acc -> (if (> (first acc) #x7f) (list:cons #x00 acc) acc)
  -1 acc -> (if (< (first acc) #x80) (list:cons #xff acc) acc)
  n  acc -> (base256 (>> n 8) (list:cons (logand n #xff) acc))
  )

(define (ber/encode-length n)
  (base256 n '()))

(define (ints->string nums)
  (list->string (map ascii->char nums)))

;; be nice if we could do this in such a way that it can be
;;  optimized down to a literal for cases like NULL and INTEGER:0
(define (taglen tag flags len)
  (ints->string (list:cons (logior (<< flags 5) tag) (ber/encode-length len))))

(define (encode-integer n)
  (ints->string (base256 n '())))

(define (ber/encode-int emit n)
  (if (= 0 n)
      (emit "\x02\x01\x00")
      (let ((r0 (encode-integer n))
            (l0 (string-length r0))
            (r1 (taglen #x02 #x0 l0)))
        (emit r0)
        (+ l0 (emit r1)))))

;; XXX Q: is zero-length == 0 legal for enum as well?
(define (ber/encode-enumerated emit n)
  (let ((r0 (encode-integer n))
        (l0 (string-length r0))
        (r1 (taglen #x0a #x0 l0)))
    (emit r0)
    (+ l0 (emit r1))))

(define (ber/encode-string emit kind s)
  (let ((ls (string-length s))
        (tag (asn1string->int kind))
        (t0 (taglen tag #x0 ls)))
    (emit s)
    (+ ls (emit t0))))

(define (ber/encode-structured emit tag flags vals)
  (let ((bytes 0))
    (for-list val (reverse vals)
      (set! bytes (+ bytes (ber/encode emit val))))
    (+ bytes (emit (taglen tag (logior flags #x01) bytes)))))

;; build a base128 representation of n.  this encoding is specific to OIDs.
;; all bytes but the last have the high bit set.
;; note: assumes positive n
(define base128
  0 acc -> acc
  n ()  -> (base128 (>> n 7) (list:cons (logand n #x7f) (list:nil)))
  n acc -> (base128 (>> n 7) (list:cons (logior #x80 (logand n #x7f)) acc))
  )

(define make-oid
  ()        acc -> acc
  (hd . tl) acc -> (make-oid tl (append acc (base128 hd '())))
  )

(define (ber/encode-oid emit vals)
  (let ((b0 (match vals with
              (v0 v1 . tl) -> (make-oid tl (LIST (+ (* 40 v0) v1)))
              _            -> (raise (:BER/BadOID vals))))
        (r0 (ints->string b0))
        (l0 (string-length r0)))
    (emit r0)
    (+ l0 (emit (taglen #x06 #x0 l0)))
    ))

(define (ber/encode-raw emit tag flags s)
  (let ((slen (string-length s)))
    (emit s)
    (+ slen (emit (taglen tag flags slen)))))

;; consumer interface:
;; emit : string -> int
;; encode : consumer ber -> int

;; <emit> is a consumer of strings.
;; it *must* return the number of bytes emitted.
;; (as does each encoder, including this one).
;; note: this encoder emits strings in *reverse* order.  This is ideal
;;   for filling a buffer that grows automatically.

;; ber/encode: ((string -> int) ber -> int)

(define ber/encode
  emit (ber:NULL)                 -> (emit "\x05\x00")
  emit (ber:BOOLEAN b)            -> (emit (if b "\x01\x01\xff" "\x01\x01\x00"))
  emit (ber:INTEGER n)            -> (ber/encode-int emit n)
  emit (ber:ENUMERATED n)         -> (ber/encode-enumerated emit n)
  emit (ber:BINTEGER s)           -> (ber/encode-raw emit #x10 #x00 s)
  emit (ber:STRING k s)           -> (ber/encode-string emit k s)
  emit (ber:SEQUENCE vals)        -> (ber/encode-structured emit #x10 #x00 vals)
  emit (ber:SET vals)             -> (ber/encode-structured emit #x11 #x00 vals)
  emit (ber:OID vals)             -> (ber/encode-oid emit vals)
  emit (ber:APPLICATION tag vals) -> (ber/encode-structured tag #x01 vals)
  emit (ber:CONTEXT tag vals)     -> (ber/encode-structured tag #x02 vals)
  emit (ber:UNKNOWN t f v)        -> (ber/encode-raw emit t f v)
  )

;; --------------------------------------------------------------------------------
;; decoder
;; --------------------------------------------------------------------------------

(define (ber/get-tag in)
  (let ((t0 (in.byte))
        (t1 (logand t0 #x1f))
        (flags (>> (logand t0 #xe0) 5))) ;; 0..7
    (if (= t1 #x1f)
        ;; extended tag
        (raise (:BER/ExtendedTag "no extended tags yet" t0))
        (:tuple t1 flags 1))))

(define ber/get-base256
  in 0 acc -> acc
  in n acc -> (ber/get-base256 in (- n 1) (+ (<< acc 8) (in.byte)))
  )

(define (ber/get-length in)
  (let ((l0 (in.byte)))
    (if (< l0 #x80)
        (:tuple l0 1)
        (if (= l0 #x80)
            (raise (:BER/IndefiniteLength "indefinite length not supported"))
            (let ((l1 (logand l0 #x7f)))
              (printf "l1 " (hex l1) "\n")
              (if (> l1 4)
                  (raise (:BER/LengthTooLong "length-of-length longer than 32 bits not supported"))
                  (:tuple (ber/get-base256 in l1 0) (+ 1 l1))))))))

(define (ber/get-tag-length in)
  (let (((tag flags lent) (ber/get-tag in))
        ((len lenl) (ber/get-length in)))
    (:tuple tag flags len (+ lenl lent))))

(define (ber/decode-bool in len)
  (if (= len 0)
      (ber:BOOLEAN #f)
      (if (= (in.byte) 0)
          (ber:BOOLEAN #f)
          (ber:BOOLEAN #t))))

(define (ber/decode-int in n)
  (let ((b0 (in.byte)))
    (ber/get-base256
     in (- n 1)
     (if (>= b0 #x80) (- b0 256) b0))))

(define (ber/decode-integer in n)
  (if (> n (- *word-size* 1))
      (ber:BINTEGER (in.string n))
      (ber:INTEGER (ber/decode-int in n))))

(define (ber/decode-enumerated in n)
  (if (> n (- *word-size* 1))
      (raise (:BER/HugeEnum "huge enumerated value" n))
      (ber:ENUMERATED (ber/decode-int in n))))

(define (ber/decode-string in tag len)
  (match (alist/lookup asn1string-rev-alist tag) with
    (maybe:yes kind) -> (ber:STRING kind (in.string len))
    (maybe:no)       -> (ber:UNKNOWN tag #x0 (in.string len))
    ))

;; this encoding specific to OIDs
(define (ber/get-base128 in n acc)
  (let ((b0 (in.byte)))
    (if (> n 3)
        (raise (:BER/Skullduggery "evil OID"))
        (if (> b0 #x80)
            (ber/get-base128 in (+ n 1) (logior (<< acc 7) (logand #x7f b0)))
            (:tuple (logior (<< acc 7) b0) n)))))

(define (ber/decode-oid in len)
  (let ((b0 (in.byte)))
    (let loop ((vals (LIST (mod b0 40) (/ b0 40)))
               (lensum 1))
      (if (>= lensum len)
          (ber:OID (reverse vals))
          (let (((num len0) (ber/get-base128 in 1 0)))
            (loop (list:cons num vals) (+ lensum len0))))
      )))

(define (ber/decode-structured in len)
  (let loop ((vals '())
             (lensum 0))
    (if (>= lensum len)
        (reverse vals)
        (let (((val flags len0) (ber/decode in)))
          (loop (list:cons val vals) (+ lensum len0)))
        )))

;; ber/decode : {byte=(-> int) string=(int -> string)} -> ber

;; XXX consider matching against flags here as well, to catch misbehavior.
(define ber/decode*
  in len #x01 #f -> (ber/decode-bool in len)
  in len #x02 #f -> (ber/decode-integer in len)
  in len #x05 #f -> (ber:NULL)
  in len #x0a #f -> (ber/decode-enumerated in len)
  in len #x06 #f -> (ber/decode-oid in len)
  in len #x10 #t -> (ber:SEQUENCE (ber/decode-structured in len))
  in len #x11 #t -> (ber:SET (ber/decode-structured in len))
  in len tag  #f -> (ber/decode-string in tag len)
  _ _    tag  _  -> (raise (:BER/UnknownTag "unknown tag" tag))
  )

(define (ber/decode in)
  (let (((tag flags len len-tl) (ber/get-tag-length in))
        (structured? (= 1 (logand flags #x001)))
        (kind (>> flags 1)))
    ;;(printf "tag " (int tag) " flags " (int flags) " len " (int len) "\n")
    (match kind structured? with
      0  _ -> (let ((val (ber/decode* in len tag structured?)))
                (:tuple val flags (+ len len-tl)))
      1 #t -> (let ((vals (ber/decode-structured in len)))
                (:tuple (ber:APPLICATION tag vals) flags (+ len len-tl)))
      2 #t -> (let ((vals (ber/decode-structured in len)))
                (:tuple (ber:CONTEXT tag vals) flags (+ len len-tl)))
      _  _ -> (raise (:BER/UnsupportedFlags "Unsupported combination of flags" flags))
      )))
