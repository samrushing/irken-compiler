;; -*- Mode: Irken -*-

;; this might want to be redone using a generator interface
;;  like base64.

(define (string->hex s)
  (let ((len (string-length s))
        (r (make-string (* 2 len))))
    (for-range i len
      (let ((e (char->int (string-ref s i)))
            (b0 hex-table[(logand #x0f (>> e 4))])
            (b1 hex-table[(logand #x0f e)]))
        (string-set! r (* i 2)       b0)
        (string-set! r (+ 1 (* i 2)) b1)))
    r))

(define hexdigit
  #\0 -> 0 #\1 -> 1 #\2 -> 2 #\3 -> 3 #\4 -> 4
  #\5 -> 5 #\6 -> 6 #\7 -> 7 #\8 -> 8 #\9 -> 9
  #\a -> #xa #\b -> #xb #\c -> #xc #\d -> #xd #\e -> #xe #\f -> #xf
  #\A -> #xa #\B -> #xb #\C -> #xc #\D -> #xd #\E -> #xe #\F -> #xf
  ch -> (raise (:Hex/BadDigit "bad hex digit" ch))
  )

(define (hex->string s)
  (let (((len rem) (divmod (string-length s) 2)))
    (if (not (zero? rem))
        (raise (:Hex/BadInputString "odd-length input string" s))
        (let ((r (make-string len)))
          (for-range i len
            (let ((b0 (hexdigit (string-ref s (* 2 i))))
                  (b1 (hexdigit (string-ref s (+ 1 (* 2 i))))))
              (string-set! r i (int->char (logior (<< b0 4) b1)))))
          r))))
