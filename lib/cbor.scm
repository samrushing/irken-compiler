;; -*- Mode: Irken -*-

;; a CBOR (RFC 8949) decoder: enough for WebAuthn's attestation objects
;;  and COSE keys, and strict about it.  definite lengths only (WebAuthn
;;  requires them); arguments must fit in an Irken int (62 bits); floats
;;  are kept as their raw bytes.  anything malformed raises CBOR/Bad.

(datatype cbor
  (:uint int)
  (:nint int)                       ;; the value, e.g. -7 (not -1-n's n)
  (:bytes string)
  (:text string)
  (:array (list cbor))
  (:map (list (:tuple cbor cbor)))  ;; in the order they appeared
  (:tag int cbor)
  (:bool bool)
  (:null)
  (:undefined)
  (:simple int)
  (:float string)                   ;; the 2, 4 or 8 bytes, big-endian
  )

(define cbor-max-depth 16)

;; decode the item that starts at <pos> in <s>: (:tuple item next-pos).
(define (cbor/decode-at s pos)

  (define (bad what) (raise (:CBOR/Bad what pos)))

  (define (byte i)
    (if (< i (string-length s))
        (char->int (string-ref s i))
        (bad "truncated")))

  ;; the big-endian unsigned integer in the <n> bytes at <i>.
  (define (uint i n)
    (let ((r 0))
      (for-range j n
        (when (>= r (<< 1 54)) (bad "integer too large"))
        (set! r (logior (<< r 8) (byte (+ i j)))))
      r))

  ;; the bytes [i, i+n).
  (define (take i n)
    (if (and (>= n 0) (<= (+ i n) (string-length s)))
        (substring s i (+ i n))
        (bad "truncated")))

  (define (item i depth)
    (when (> depth cbor-max-depth) (bad "nested too deeply"))
    (let ((ib (byte i))
          (major (>> ib 5))
          (info (logand ib 31))
          (start (+ i 1))
          ;; the argument, and where the content after it starts.
          ((arg next)
           (cond ((< info 24) (:tuple info start))
                 ((= info 24) (:tuple (uint start 1) (+ start 1)))
                 ((= info 25) (:tuple (uint start 2) (+ start 2)))
                 ((= info 26) (:tuple (uint start 4) (+ start 4)))
                 ((= info 27) (:tuple (uint start 8) (+ start 8)))
                 ((= info 31) (bad "indefinite length"))
                 (else (bad "reserved additional information")))))
      (match major with
        0 -> (:tuple (cbor:uint arg) next)
        1 -> (:tuple (cbor:nint (- -1 arg)) next)
        2 -> (:tuple (cbor:bytes (take next arg)) (+ next arg))
        3 -> (:tuple (cbor:text (take next arg)) (+ next arg))
        4 -> (let ((items (list:nil)) (p next))
               (when (> arg (- (string-length s) p)) (bad "array too long"))
               (for-range k arg
                 (let (((x p1) (item p (+ depth 1))))
                   (push! items x)
                   (set! p p1)))
               (:tuple (cbor:array (reverse items)) p))
        5 -> (let ((pairs (list:nil)) (p next))
               (when (> arg (- (string-length s) p)) (bad "map too long"))
               (for-range k arg
                 (let (((key p1) (item p (+ depth 1)))
                       ((val p2) (item p1 (+ depth 1))))
                   (push! pairs (:tuple key val))
                   (set! p p2)))
               (:tuple (cbor:map (reverse pairs)) p))
        6 -> (let (((x p) (item next (+ depth 1))))
               (:tuple (cbor:tag arg x) p))
        _ -> (match info with
               20 -> (:tuple (cbor:bool #f) next)
               21 -> (:tuple (cbor:bool #t) next)
               22 -> (:tuple (cbor:null) next)
               23 -> (:tuple (cbor:undefined) next)
               25 -> (:tuple (cbor:float (take start 2)) next)
               26 -> (:tuple (cbor:float (take start 4)) next)
               27 -> (:tuple (cbor:float (take start 8)) next)
               _  -> (:tuple (cbor:simple arg) next))
        )))

  (item pos 0))

;; decode all of <s>, which must hold exactly one item.
(define (cbor/decode s)
  (let (((x end) (cbor/decode-at s 0)))
    (if (= end (string-length s))
        x
        (raise (:CBOR/Bad "trailing bytes" end)))))

;; the value for key <k> (compared with <same?>) in the map <m>.
(define (cbor/map-get m same? k)
  (match m with
    (cbor:map pairs)
    -> (let loop ((pairs pairs))
         (match pairs with
           () -> (maybe:no)
           ((:tuple key val) . rest) -> (if (same? key k) (maybe:yes val) (loop rest))))
    _ -> (maybe:no)))

;; keys as they appear in WebAuthn: small integers (COSE) and text.
(define cbor-int=?
  (cbor:uint a) b -> (= a b)
  (cbor:nint a) b -> (= a b)
  _ _ -> #f)

(define cbor-text=?
  (cbor:text a) b -> (string=? a b)
  _ _ -> #f)

(define (cbor/get-int m k) (cbor/map-get m cbor-int=? k))
(define (cbor/get-text m k) (cbor/map-get m cbor-text=? k))
