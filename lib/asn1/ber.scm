;; -*- Mode: Irken -*-

(require "lib/enum.scm")

;; This is a start on an ASN.1 BER/DER encoder.

;; Note: not meant to be a 'full' BER encoder.  The purpose of
;;   this code is 99% for crypto standards, so lots of optional
;;   indefinite-length & constructed stuff is not implemented.

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
 (BIT              3)  ;; bit string (NOTE! first byte indicates padding)
 (UTF8             12) ;; utf-8 string
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

;; flags are:
;;  class      bit8 bit7
;; universal     0   0
;; application   0   1
;; context       1   0
;; private       1   1
;;
;; bit6 indicates 'structured'

(make-enum asn1class
  (univ 0)
  (appl 1)
  (ctxt 2)
  (priv 3)
  )

;; XXX this should probably be called 'asn1'.
(datatype ber
  ;; -- primitive --
  (:NULL)
  (:BOOLEAN bool)
  ;; XXX consider making a seprate int|big datatype for this.
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
  ;; -- random --
  ;; Note: useful for implicitly tagged objects.
  (:RAW asn1class bool int string) ;; class structured? tag value
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
  (ber:RAW class s? tag val) -> (format "(RAW [" (sym (asn1class->name class)) " " (int tag) "] "
                                        (bool s?) " " (string->hex val) ")")
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

;; --------------------------------------------------------------------------------
;; encoder
;; --------------------------------------------------------------------------------

  ;; ber/encode: ((string -> int) ber -> int)
(define (ber/encode emit asn1)

  (define base256
    0  acc -> (if (> (first acc) #x7f) (list:cons #x00 acc) acc)
    -1 acc -> (if (< (first acc) #x80) (list:cons #xff acc) acc)
    n  acc -> (base256 (>> n 8) (list:cons (logand n #xff) acc))
    )

  (define (encode-length n)
    ;; unsigned base-256 length
    (define l256
      0 acc -> acc
      n acc -> (l256 (>> n 8) (list:cons (logand #xff n) acc))
      )
    (if (< n 128)
        (LIST n)
        (let ((n0 (l256 n '())))
          (list:cons (logior #x80 (length n0)) n0))))

  (define (ints->string nums)
    (list->string (map ascii->char nums)))

  ;; be nice if we could do this in such a way that it can be
  ;;  optimized down to a literal for cases like NULL and INTEGER:0
  (define (taglen tag flags len)
    (ints->string (list:cons (logior (<< flags 5) tag) (encode-length len))))

  (define (encode-integer n)
    (ints->string (base256 n '())))

  (define (encode-int emit n)
    (if (= 0 n)
        (emit "\x02\x01\x00")
        (let ((r0 (encode-integer n))
              (l0 (string-length r0))
              (r1 (taglen #x02 #x0 l0)))
          (emit r0)
          (+ l0 (emit r1)))))

  ;; XXX Q: is zero-length == 0 legal for enum as well?
  (define (encode-enumerated emit n)
    (let ((r0 (encode-integer n))
          (l0 (string-length r0))
          (r1 (taglen #x0a #x0 l0)))
      (emit r0)
      (+ l0 (emit r1))))

  (define (encode-string emit kind s)
    (let ((ls (string-length s))
          (tag (asn1string->int kind))
          (t0 (taglen tag #x0 ls)))
      (emit s)
      (+ ls (emit t0))))

  (define (encode-structured emit tag flags vals)
    (let ((bytes 0))
      (for-list val (reverse vals)
        (set! bytes (+ bytes (ber/encode emit val))))
      (+ bytes (emit (taglen tag (logior (<< flags 1) #x01) bytes)))))

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

  (define (encode-oid emit vals)
    (let ((b0 (match vals with
                (v0 v1 . tl) -> (make-oid tl (LIST (+ (* 40 v0) v1)))
                _            -> (raise (:BER/BadOID vals))))
          (r0 (ints->string b0))
          (l0 (string-length r0)))
      (emit r0)
      (+ l0 (emit (taglen #x06 #x0 l0)))
      ))

  (define (encode-raw emit class structured? tag s)
    (let ((slen (string-length s))
          (flags (logior (<< (asn1class->int class) 1) (if structured? 1 0))))
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

  ;; --- body of ber/encode ---
  (match asn1 with
    (ber:NULL)                 -> (emit "\x05\x00")
    (ber:BOOLEAN b)            -> (emit (if b "\x01\x01\xff" "\x01\x01\x00"))
    (ber:INTEGER n)            -> (encode-int emit n)
    (ber:BINTEGER s)           -> (encode-raw emit (asn1class:univ) #f #x02 s)
    (ber:ENUMERATED n)         -> (encode-enumerated emit n)
    (ber:STRING k s)           -> (encode-string emit k s)
    (ber:SEQUENCE vals)        -> (encode-structured emit #x10 #x00 vals)
    (ber:SET vals)             -> (encode-structured emit #x11 #x00 vals)
    (ber:OID vals)             -> (encode-oid emit vals)
    (ber:APPLICATION tag vals) -> (encode-structured emit tag #x01 vals)
    (ber:CONTEXT tag vals)     -> (encode-structured emit tag #x02 vals)
    (ber:RAW class s? tag val) -> (encode-raw emit class s? tag val)
    ))

;; --------------------------------------------------------------------------------
;; decoder
;; --------------------------------------------------------------------------------

(define (ber/decode in)

  ;; tag bits
  ;; 7 6 5 4 3 2 1 0
  ;; | | | |       |
  ;; | | | +-------+- tag value
  ;; | | +- structured?
  ;; +-+- class (univ,context,app,private)

  (define (get-tag in)
    (let ((t0 (in.byte))
          (tag (logand t0 #x1f))
          (flags (>> (logand t0 #xe0) 5)) ;; 0..7
          (structured? (= 1 (logand flags #x001)))
          (class (>> flags 1)))
      (if (= tag #x1f)
          ;; extended tag
          (raise (:BER/ExtendedTag "extended tags not supported" t0))
          (:tuple class structured? tag))))

  (define get-base256
    in 0 acc -> acc
    in n acc -> (get-base256 in (- n 1) (+ (<< acc 8) (in.byte)))
    )

  (define (get-length in)
    (let ((l0 (in.byte)))
      (if (< l0 #x80)
          (:tuple l0 1)
          (if (= l0 #x80)
              (raise (:BER/IndefiniteLength "indefinite length not supported"))
              (let ((l1 (logand l0 #x7f)))
                (if (> l1 4)
                    (raise (:BER/LengthTooLong "length-of-length longer than 32 bits not supported"))
                    (:tuple (get-base256 in l1 0) (+ 1 l1))))))))

  (define (get-tag-length in)
    (let (((class structured? tag) (get-tag in))
          ((len lol) (get-length in)))
      ;; note: tag-length is always 1 because we do not support extended tags.
      (:tuple (int->asn1class class) structured? tag len (+ lol 1))))

  (define (decode-bool in len)
    (if (= len 0)
        (ber:BOOLEAN #f)
        (if (= (in.byte) 0)
            (ber:BOOLEAN #f)
            (ber:BOOLEAN #t))))

  (define (decode-int in n)
    (let ((b0 (in.byte)))
      (get-base256
       in (- n 1)
       (if (>= b0 #x80) (- b0 256) b0))))

  (define (decode-integer in n)
    (if (> n (- *word-size* 1))
        (ber:BINTEGER (in.string n))
        (ber:INTEGER (decode-int in n))))

  (define (decode-enumerated in n)
    (if (> n (- *word-size* 1))
        (raise (:BER/HugeEnum "huge enumerated value" n))
        (ber:ENUMERATED (decode-int in n))))

  (define (decode-string in tag len)
    (match (alist/lookup asn1string-rev-alist tag) with
      (maybe:yes kind) -> (ber:STRING kind (in.string len))
      (maybe:no)       -> (ber:RAW (asn1class:univ) #f tag (in.string len))
      ))

  ;; this encoding specific to OIDs
  (define (get-base128 in n acc)
    (let ((b0 (in.byte)))
      (if (> n 3)
          (raise (:BER/Skullduggery "evil OID"))
          (if (> b0 #x80)
              (get-base128 in (+ n 1) (logior (<< acc 7) (logand #x7f b0)))
              (:tuple (logior (<< acc 7) b0) n)))))

  (define (decode-oid in len)
    (let ((b0 (in.byte)))
      (let loop ((vals (LIST (mod b0 40) (/ b0 40)))
                 (lensum 1))
        (if (>= lensum len)
            (ber:OID (reverse vals))
            (let (((num len0) (get-base128 in 1 0)))
              (loop (list:cons num vals) (+ lensum len0))))
        )))

  (define (decode-structured in len)
    (let loop ((vals '())
               (lensum 0))
      (cond ((= lensum len) (reverse vals))
            ((> lensum len) (raise (:BER/ExtraData len lensum)))
            (else
             (let (((val len0) (ber/decode in)))
               (loop (list:cons val vals) (+ lensum len0)))))))

  (define decode-univ
    in len #x01 #f -> (decode-bool in len)
    in len #x02 #f -> (decode-integer in len)
    in len #x05 #f -> (ber:NULL)
    in len #x0a #f -> (decode-enumerated in len)
    in len #x06 #f -> (decode-oid in len)
    in len #x10 #t -> (ber:SEQUENCE (decode-structured in len))
    in len #x11 #t -> (ber:SET (decode-structured in len))
    in len tag  #f -> (decode-string in tag len)
    in len tag  #t -> (ber:RAW (asn1class:univ) #t tag (in.string len))
    ;;_ _    tag  _  -> (raise (:BER/UnknownTag "unknown universal structured tag" tag))
    )

  ;; --- body of decode ---
  (let (((class structured? tag len lentl) (get-tag-length in))
        (totlen (+ len lentl)))
    ;; (printf "class " (sym (asn1class->name class))
    ;;         " s? " (bool structured?)
    ;;         " tag " (int tag)
    ;;         " len " (int len) "\n")
    (match class structured? with
      (asn1class:univ)  _ -> (let ((val (decode-univ in len tag structured?)))
                               (:tuple val totlen))
      (asn1class:appl) #t -> (let ((vals (decode-structured in len)))
                               (:tuple (ber:APPLICATION tag vals) totlen))
      (asn1class:ctxt) #t -> (let ((vals (decode-structured in len)))
                               (:tuple (ber:CONTEXT tag vals) totlen))
      _ _                 -> (:tuple (ber:RAW class structured? tag (in.string len)) totlen)
      )))

;; --------------------------------------------------------------------------------
;; utility
;; --------------------------------------------------------------------------------

;; sample producer
(define (string-emitter s)
  (let ((pos 0)
        (len (string-length s)))
    (define (ensure n)
      (when (> (+ pos n) len)
        (raise (:BERUnderflow pos s))))
    (define (byte)
      (ensure 1)
      (let ((r (string-ref s pos)))
        (set! pos (+ pos 1))
        (char->int r)))
    (define (string n)
      (ensure n)
      (let ((r (substring s pos (+ pos n))))
        (set! pos (+ n pos))
        r))
    {byte=byte string=string}
    ))

;; sample consumer
(define (collector)
  (let ((val '()))
    (define (push x)
      (PUSH val x)
      (string-length x))
    (define (get-val)
      (string-concat val))
    (define (reset)
      (set! val '()))
    {push  = push
     val   = get-val
     reset = reset
     }))

(define (asn1->ber ob)
  (let ((c (collector)))
    (ber/encode c.push ob)
    (c.val)))

(define (ber->asn1 s)
  (let ((slen (string-length s))
        ((asn1 len) (ber/decode (string-emitter s))))
    ;; XXX any need to check flags for unexpected values?
    (when (not (= len slen))
      (raise (:ASN1/ExtraData len slen)))
    asn1))

;; --------------------------------------------------------------------------------
;; XXX can/should we put together a quick DSL for building asn1 objects?
;; --------------------------------------------------------------------------------
